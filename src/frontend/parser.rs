use std::borrow::Cow;

use crate::frontend::error::{Messages, ParseResult};
use crate::frontend::file::SourceFile;
use crate::frontend::span::Span;
use crate::frontend::tokenizer::{Token, TokenKind, Tokenizer};
use crate::frontend::tree::*;

pub fn parse_file<'a>(messages: &mut Messages, file: &'a SourceFile) -> File<'a> {
	let mut tokenizer = Tokenizer::new(file.index, &file.source);
	let block = parse_root_block(messages, &mut tokenizer);

	let module_path = &file.module_path;
	File { source_file: file, module_path, block }
}

pub fn parse_root_block<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> Block<'a> {
	let statements = parse_statements(messages, tokenizer);
	Block { statements }
}

pub fn parse_block<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Block<'a>>> {
	let open = tokenizer.expect(messages, TokenKind::OpenBrace)?;
	let statements = parse_statements(messages, tokenizer);
	let close = tokenizer.expect(messages, TokenKind::CloseBrace)?;

	let block = Block { statements };
	let span = open.span + close.span;
	Ok(Node::new(block, span))
}

pub fn parse_statements<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> Vec<Statement<'a>> {
	let mut items = Vec::new();

	loop {
		while let Ok(token) = tokenizer.peek() {
			if token.kind != TokenKind::Newline {
				break;
			}

			if tokenizer.next(messages).is_err() {
				return items;
			}
		}

		let attributes = match parse_attributes(messages, tokenizer) {
			Ok(attributes) => attributes,

			Err(_) => {
				consume_error_syntax(messages, tokenizer);
				continue;
			}
		};

		let token = match tokenizer.peek() {
			Ok(token) => token,
			Err(_) => return items,
		};

		match token {
			Token { kind: TokenKind::Word, text: "import", .. } => {
				disallow_attributes(messages, attributes, token.span, "An import statement");
				if let Ok(statement) = parse_import_statement(messages, tokenizer) {
					items.push(Statement::Import(statement));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::Word, text: "const", .. } => {
				disallow_attributes(messages, attributes, token.span, "A const definition");
				if let Ok(statement) = parse_const_statement(messages, tokenizer) {
					items.push(Statement::Const(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::Word, text: "let" | "mut", .. } => {
				disallow_attributes(messages, attributes, token.span, "A let statement");
				if let Ok(statement) = parse_binding_statement(messages, tokenizer) {
					items.push(Statement::Binding(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::Word, text: "fn", .. } => {
				if let Ok(statement) = parse_function_declaration(messages, tokenizer, attributes) {
					items.push(Statement::Function(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::Word, text: "struct", .. } => {
				if let Ok(statement) = parse_struct_declaration(messages, tokenizer, attributes) {
					items.push(Statement::Struct(statement));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::Word, text: "return", .. } => {
				disallow_attributes(messages, attributes, token.span, "A return statement");
				if let Ok(statement) = parse_return_statement(messages, tokenizer) {
					items.push(Statement::Return(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::OpenBrace, .. } => {
				disallow_attributes(messages, attributes, token.span, "A block");
				if let Ok(statement) = parse_block(messages, tokenizer) {
					items.push(Statement::Block(statement));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::CloseBrace, .. } => break,

			_ => {
				disallow_attributes(messages, attributes, token.span, "An expression");
				if let Ok(expression) = parse_expression(messages, tokenizer, true) {
					items.push(Statement::Expression(expression));
					if tokenizer.expect(messages, TokenKind::Newline).is_err() {
						consume_error_syntax(messages, tokenizer);
					}
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}
		}
	}

	items
}

//TODO: Add function to only disallow specific attribute kinds
fn disallow_attributes(messages: &mut Messages, attributes: Attributes, span: Span, label: &str) {
	let mut buffer = [Span::unusable(); Attributes::FIELD_COUNT];
	let spans = attributes.attribute_spans(&mut buffer);

	if !spans.is_empty() {
		let mut message = error!("{label} does not allow attributes").span(span);
		for &span in spans {
			message = message.note(note!(span, "Attribute here"));
		}

		messages.message(message);
	}
}

fn parse_expression<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	allow_struct_literal: bool,
) -> ParseResult<Node<Expression<'a>>> {
	parse_expression_climb(messages, tokenizer, allow_struct_literal, 0)
}

fn parse_expression_climb<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	allow_struct_literal: bool,
	min_precedence: u32,
) -> ParseResult<Node<Expression<'a>>> {
	let mut atom = parse_expression_atom(messages, tokenizer, allow_struct_literal)?;

	loop {
		if tokenizer.peek_kind() == Ok(TokenKind::Period) {
			atom = parse_following_period(messages, tokenizer, atom)?;
		} else if tokenizer.peek_kind() == Ok(TokenKind::OpenBracket) {
			atom = parse_bracket_index(messages, tokenizer, atom)?;
		} else {
			break;
		}
	}

	while let Some(operator) = tokenizer.peek().ok().and_then(token_to_operator) {
		let precedence = operator.item.precedence();
		if precedence < min_precedence {
			break;
		}

		tokenizer.next(messages).expect("Known peeked token");

		let associativity = operator.item.associativity();
		let next_min_precedence = match associativity {
			Associativity::Left => precedence + 1,
			Associativity::Right => precedence,
		};

		let right = parse_expression_climb(messages, tokenizer, allow_struct_literal, next_min_precedence)?;
		let left_span = atom.span;
		let right_span = right.span;

		let binary_operation = BinaryOperation { op: operator, right, left: atom };
		let boxed = Box::new(binary_operation);
		let expression = Expression::BinaryOperation(boxed);

		atom = Node::new(expression, left_span + right_span);
	}

	Ok(atom)
}

fn token_to_operator(token: Token) -> Option<Node<BinaryOperator>> {
	let operator = match token.kind {
		TokenKind::Equal => BinaryOperator::Assign,

		TokenKind::Add => BinaryOperator::Add,
		TokenKind::Sub => BinaryOperator::Sub,
		TokenKind::Mul => BinaryOperator::Mul,
		TokenKind::Div => BinaryOperator::Div,

		TokenKind::CompEqual => BinaryOperator::Equals,
		TokenKind::CompNotEqual => BinaryOperator::NotEquals,

		TokenKind::CompGreater => BinaryOperator::GreaterThan,
		TokenKind::CompGreaterEqual => BinaryOperator::GreaterThanEquals,

		TokenKind::CompLess => BinaryOperator::LessThan,
		TokenKind::CompLessEqual => BinaryOperator::LessThanEquals,

		TokenKind::Word if token.text == "and" => BinaryOperator::LogicalAnd,
		TokenKind::Word if token.text == "or" => BinaryOperator::LogicalOr,

		_ => return None,
	};

	Some(Node::new(operator, token.span))
}

fn parse_expression_atom<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	allow_struct_literal: bool,
) -> ParseResult<Node<Expression<'a>>> {
	let peeked = tokenizer.peek()?;

	match peeked.kind {
		TokenKind::Sub => {
			let token = tokenizer.expect(messages, TokenKind::Sub)?;
			let op = Node::new(UnaryOperator::Negate, token.span);

			let expression = parse_expression(messages, tokenizer, allow_struct_literal)?;

			let span = token.span + expression.span;
			let negate = Box::new(UnaryOperation { op, expression });

			Ok(Node::new(Expression::UnaryOperation(negate), span))
		}

		TokenKind::String => {
			let string_token = tokenizer.expect(messages, TokenKind::String)?;
			let text = parse_string_contents(string_token.text);
			let value = Node::from_token(text, string_token);
			Ok(Node::from_token(Expression::StringLiteral(StringLiteral { value }), string_token))
		}

		TokenKind::Codepoint => {
			let codepoint_token = tokenizer.expect(messages, TokenKind::Codepoint)?;
			let value = Node::from_token(codepoint_token.text.chars().next().unwrap(), codepoint_token);

			Ok(Node::from_token(
				Expression::CodepointLiteral(CodepointLiteral { value }),
				codepoint_token,
			))
		}

		TokenKind::Number => {
			return parse_number(messages, tokenizer);
		}

		TokenKind::Word | TokenKind::DoubleColon => {
			match peeked.text {
				"if" => return parse_if(messages, tokenizer),

				"true" => {
					tokenizer.next(messages)?;
					return Ok(Node::new(Expression::BooleanLiteral(true), peeked.span));
				}

				"false" => {
					tokenizer.next(messages)?;
					return Ok(Node::new(Expression::BooleanLiteral(false), peeked.span));
				}

				_ => {}
			}

			let path_segments = parse_path_segments(messages, tokenizer)?;
			let type_arguments = parse_type_arguments(messages, tokenizer)?;

			let (is_call, is_struct_literal) = match tokenizer.peek() {
				Ok(Token { kind: TokenKind::OpenParen, .. }) => (true, false),
				Ok(Token { kind: TokenKind::OpenBrace, .. }) => (false, true),
				_ => (false, false),
			};

			if is_call {
				let type_arguments = type_arguments.map(|node| node.item).unwrap_or_default();
				let arguments_node = parse_arguments(messages, tokenizer)?;
				let arguments = arguments_node.item;
				let span = path_segments.span + arguments_node.span;
				let call = Call { path_segments, type_arguments, arguments };

				return Ok(Node::new(Expression::Call(call), span));
			}

			if is_struct_literal && allow_struct_literal {
				let type_span = match &type_arguments {
					Some(type_arguments) => path_segments.span + type_arguments.span,
					None => path_segments.span,
				};
				let type_arguments = type_arguments.map(|node| node.item).unwrap_or_default();
				let type_object = Type::Path { path_segments, type_arguments };
				let parsed_type = Node::new(type_object, type_span);

				let initializer = parse_struct_initializer(messages, tokenizer)?;

				let span = type_span + initializer.span;
				let struct_literal = StructLiteral { parsed_type, initializer };

				return Ok(Node::new(Expression::StructLiteral(struct_literal), span));
			}

			if let Some(type_arguments) = type_arguments {
				// This is a weird error
				messages.message(error!("Type arguments not allowed on binding read").span(type_arguments.span));
			}

			let span = path_segments.span;
			let read = Read { path_segments };

			Ok(Node::new(Expression::Read(read), span))
		}

		TokenKind::OpenParen => {
			tokenizer.expect(messages, TokenKind::OpenParen)?;
			// Regardless of if parent parsing context disallowed struct literals, we override that within parenthesis
			let expression = parse_expression(messages, tokenizer, true)?;
			tokenizer.expect(messages, TokenKind::CloseParen)?;
			Ok(expression)
		}

		TokenKind::OpenBracket => {
			let open_token = tokenizer.expect(messages, TokenKind::OpenBracket)?;

			let multi_line = tokenizer.peek_kind() == Ok(TokenKind::Newline);
			if multi_line {
				tokenizer.expect(messages, TokenKind::Newline)?;
			}

			let mut expressions = Vec::new();
			while tokenizer.peek_kind() != Ok(TokenKind::CloseBracket) {
				expressions.push(parse_expression(messages, tokenizer, true)?);

				if multi_line {
					if tokenizer.peek_kind() == Ok(TokenKind::Comma) {
						tokenizer.expect(messages, TokenKind::Comma)?;
					}
					tokenizer.expect(messages, TokenKind::Newline)?;
				} else if tokenizer.peek_kind() != Ok(TokenKind::CloseBracket) {
					tokenizer.expect(messages, TokenKind::Comma)?;
				}
			}

			let close_token = tokenizer.expect(messages, TokenKind::CloseBracket)?;

			let literal = ArrayLiteral { expressions };
			let expression = Expression::ArrayLiteral(literal);
			let span = open_token.span + close_token.span;
			Ok(Node::new(expression, span))
		}

		TokenKind::OpenBrace => {
			let parsed_block = parse_block(messages, tokenizer)?;

			let span = parsed_block.span;
			let block = parsed_block.item;

			Ok(Node::new(Expression::Block(block), span))
		}

		_ => {
			messages.message(
				error!("Unexpected token {:?} while attempting to parse expression atom", peeked.text).span(peeked.span),
			);
			Err(())
		}
	}
}

fn parse_following_period<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	atom: Node<Expression<'a>>,
) -> ParseResult<Node<Expression<'a>>> {
	tokenizer.expect(messages, TokenKind::Period)?;

	if tokenizer.peek_kind() == Ok(TokenKind::Exclamation) {
		let exclamation = tokenizer.expect(messages, TokenKind::Exclamation)?;
		let span = atom.span + exclamation.span;

		let op = Node::new(UnaryOperator::Invert, exclamation.span);
		let operation = UnaryOperation { op, expression: atom };
		let expression = Expression::UnaryOperation(Box::new(operation));
		return Ok(Node::new(expression, span));
	}

	if tokenizer.peek_kind() == Ok(TokenKind::Ampersand) {
		let ampersand = tokenizer.expect(messages, TokenKind::Ampersand)?;

		let mut op_span = ampersand.span;
		let mut op = UnaryOperator::AddressOf;

		if tokenizer.peek_kind() == Ok(TokenKind::Word) {
			let mut_token = tokenizer.expect_word(messages, "mut")?;
			op_span += mut_token.span;
			op = UnaryOperator::AddressOfMut;
		}

		let span = atom.span + op_span;
		let op = Node::new(op, op_span);
		let operation = UnaryOperation { op, expression: atom };
		let expression = Expression::UnaryOperation(Box::new(operation));
		return Ok(Node::new(expression, span));
	}

	if tokenizer.peek_kind() == Ok(TokenKind::Mul) {
		let asterisk = tokenizer.expect(messages, TokenKind::Mul)?;
		let span = atom.span + asterisk.span;

		let op = Node::new(UnaryOperator::Dereference, asterisk.span);
		let operation = UnaryOperation { op, expression: atom };
		let expression = Expression::UnaryOperation(Box::new(operation));
		return Ok(Node::new(expression, span));
	}

	if tokenizer.peek_kind() == Ok(TokenKind::OpenParen) {
		let open_paren = tokenizer.expect(messages, TokenKind::OpenParen)?;
		let parsed_type = parse_type(messages, tokenizer)?;
		let close_paren = tokenizer.expect(messages, TokenKind::CloseParen)?;

		let op_span = open_paren.span + close_paren.span;
		let span = atom.span + close_paren.span;

		let op = Node::new(UnaryOperator::Cast { parsed_type }, op_span);
		let operation = UnaryOperation { op, expression: atom };
		let expression = Expression::UnaryOperation(Box::new(operation));
		return Ok(Node::new(expression, span));
	}

	let name_token = tokenizer.expect(messages, TokenKind::Word)?;
	let name = Node::from_token(name_token.text, name_token);

	let span = atom.span + name.span;
	let field_read = Box::new(FieldRead { base: atom, name });
	return Ok(Node::new(Expression::FieldRead(field_read), span));
}

fn parse_bracket_index<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	atom: Node<Expression<'a>>,
) -> ParseResult<Node<Expression<'a>>> {
	tokenizer.expect(messages, TokenKind::OpenBracket)?;
	let index_expression = parse_expression(messages, tokenizer, true)?;
	let close_token = tokenizer.expect(messages, TokenKind::CloseBracket)?;

	let span = atom.span + close_token.span;
	let index = UnaryOperator::Index { index_expression };
	let op = Node::new(index, span);
	let operation = UnaryOperation { op, expression: atom };
	let expression = Expression::UnaryOperation(Box::new(operation));
	Ok(Node::new(expression, span))
}

fn parse_string_contents(string: &str) -> Cow<str> {
	let mut allocated = String::new();

	let mut index = 0;
	let mut last_extend_index = 0;
	while index < string.len() {
		let mut escape = "";
		match string[index..].as_bytes() {
			[b'\\', b'n', ..] => escape = "\n",
			[b'\\', b'\\', ..] => escape = "\\",
			[b'\\', b'"', ..] => escape = "\"",
			[b'\\', b'0', ..] => escape = "\0",
			_ => {}
		}

		if !escape.is_empty() {
			allocated.push_str(&string[last_extend_index..index]);
			last_extend_index = index + 2;
			index += 1;
			allocated.push_str(escape);
		}

		index += 1;
	}

	if !allocated.is_empty() {
		allocated.push_str(&string[last_extend_index..]);
		return Cow::Owned(allocated);
	}

	Cow::Borrowed(string)
}

// TODO: Replace with in-language test once stdout printing is in
#[test]
fn test_parse_string() {
	let cases = [
		(r"hello there", "hello there"),
		(r"hello\nthere", "hello\nthere"),
		(r"\nthere", "\nthere"),
		(r"\n", "\n"),
		(r"hello\n", "hello\n"),
		(r"\nhello\n", "\nhello\n"),
		(r"\\hel\\lo\\", "\\hel\\lo\\"),
		(r"hello\0there", "hello\0there"),
	];

	for (index, (process, expected)) in cases.iter().enumerate() {
		let processed = parse_string_contents(process);
		assert!(processed == *expected, "expected {expected}, got {processed}, index {index}");
	}
}

fn parse_if<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Expression<'a>>> {
	let if_token = tokenizer.expect_word(messages, "if")?;

	let condition = parse_expression(messages, tokenizer, false)?;
	let body = parse_expression(messages, tokenizer, true)?;

	let span = if_token.span + body.span;
	let value = If { condition, body };
	let expression = Expression::If(Box::new(value));
	Ok(Node::new(expression, span))
}

// Holy return type batman
// TODO: Do something about this
fn parse_type_arguments<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Option<Node<Vec<Node<Type<'a>>>>>> {
	if tokenizer.peek_kind() != Ok(TokenKind::OpenGeneric) {
		return Ok(None);
	}

	let open_token = tokenizer.expect(messages, TokenKind::OpenGeneric)?;

	let mut types = Vec::new();
	while !reached_close_generic(tokenizer) {
		types.push(parse_type(messages, tokenizer)?);

		if reached_close_generic(tokenizer) {
			break;
		}

		tokenizer.expect(messages, TokenKind::Comma)?;
	}

	let close_token = tokenizer.expect(messages, TokenKind::CompGreater)?;

	let span = open_token.span + close_token.span;
	Ok(Some(Node::new(types, span)))
}

fn parse_arguments<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Vec<Node<Expression<'a>>>>> {
	let open_paren_token = tokenizer.expect(messages, TokenKind::OpenParen)?;

	let multi_line = tokenizer.peek_kind() == Ok(TokenKind::Newline);
	if multi_line {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	let mut expressions = Vec::new();
	while !reached_close_paren(tokenizer) {
		expressions.push(parse_expression(messages, tokenizer, true)?);

		if reached_close_paren(tokenizer) {
			break;
		}

		if multi_line {
			if tokenizer.peek_kind() == Ok(TokenKind::Comma) {
				tokenizer.expect(messages, TokenKind::Comma)?;
			}
			if tokenizer.peek_kind() == Ok(TokenKind::Newline) {
				tokenizer.expect(messages, TokenKind::Newline)?;
			}
		} else if tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
			tokenizer.expect(messages, TokenKind::Comma)?;
		}
	}

	let close_paren_token = tokenizer.expect(messages, TokenKind::CloseParen)?;

	let span = open_paren_token.span + close_paren_token.span;
	Ok(Node::new(expressions, span))
}

fn parse_struct_initializer<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<StructInitializer<'a>>> {
	let open_brace_token = tokenizer.expect(messages, TokenKind::OpenBrace)?;

	let multi_line = tokenizer.peek_kind() == Ok(TokenKind::Newline);
	if multi_line {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	let mut field_initializers = Vec::new();

	while tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
		let name_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, name_token, "field name")?;
		let name = Node::from_token(name_token.text, name_token);

		tokenizer.expect(messages, TokenKind::Colon)?;

		let expression = parse_expression(messages, tokenizer, true)?;

		if multi_line {
			if tokenizer.peek_kind() == Ok(TokenKind::Comma) {
				tokenizer.expect(messages, TokenKind::Comma)?;
			}
			if tokenizer.peek_kind() == Ok(TokenKind::Newline) {
				tokenizer.expect(messages, TokenKind::Newline)?;
			}
		} else if tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
			tokenizer.expect(messages, TokenKind::Comma)?;
		}

		field_initializers.push(FieldInitializer { name, expression });
	}

	let close_brace_token = tokenizer.expect(messages, TokenKind::CloseBrace)?;

	Ok(Node::new(
		StructInitializer { field_initializers },
		open_brace_token.span + close_brace_token.span,
	))
}

fn parse_number<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Expression<'a>>> {
	let number_token = tokenizer.expect(messages, TokenKind::Number)?;
	let has_period = number_token.text.as_bytes().contains(&b'.');
	let span = number_token.span;

	let expression = if has_period {
		let value = match number_token.text.parse::<f64>() {
			Ok(value) => value,

			Err(_) => {
				messages.message(error!("Invalid float literal").span(span));
				return Err(());
			}
		};

		let literal = FloatLiteral { value: Node::new(value, span) };
		Expression::FloatLiteral(literal)
	} else {
		let value = match number_token.text.parse::<i128>() {
			Ok(value) => value,

			Err(_) => {
				messages.message(error!("Invalid integer literal").span(span));
				return Err(());
			}
		};

		let literal = IntegerLiteral { value: Node::new(value, span) };
		Expression::IntegerLiteral(literal)
	};

	Ok(Node::new(expression, span))
}

fn parse_attributes<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Attributes<'a>> {
	fn check_duplicate_attribute<T>(
		messages: &mut Messages,
		attribute: &Option<Node<T>>,
		name: &str,
		duplicate_span: Span,
	) -> ParseResult<()> {
		if let Some(attribute) = attribute {
			messages.message(
				error!("Duplicate attribute {name:?}")
					.span(duplicate_span)
					.note(note!(attribute.span, "Original here")),
			);
			return Err(());
		}

		Ok(())
	}

	let mut attributes = Attributes::blank();

	while let Ok(peeked) = tokenizer.peek() {
		match peeked.text {
			"generic" => {
				check_duplicate_attribute(messages, &attributes.generic_attribute, "generic", peeked.span)?;
				attributes.generic_attribute = Some(parse_generic_attribute(messages, tokenizer)?);
			}

			"intrinsic" => {
				check_duplicate_attribute(messages, &attributes.intrinsic_attribute, "intrinsic", peeked.span)?;
				attributes.intrinsic_attribute = Some(parse_intrinsic_attribute(messages, tokenizer)?);
			}

			"extern" => {
				check_duplicate_attribute(messages, &attributes.extern_attribute, "extern", peeked.span)?;
				attributes.extern_attribute = Some(parse_extern_attribute(messages, tokenizer)?);
			}

			"export" => {
				check_duplicate_attribute(messages, &attributes.extern_attribute, "extern", peeked.span)?;
				attributes.export_attribute = Some(parse_export_attribute(messages, tokenizer)?);
			}

			_ => break,
		}
	}

	Ok(attributes)
}

fn parse_generic_attribute<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<GenericAttribute<'a>>> {
	let generic_token = tokenizer.expect_word(messages, "generic")?;

	let mut names = Vec::new();
	loop {
		let name_token = tokenizer.expect(messages, TokenKind::Word)?;
		names.push(Node::new(name_token.text, name_token.span));

		if tokenizer.peek_kind() == Ok(TokenKind::Newline) {
			tokenizer.expect(messages, TokenKind::Newline)?;
			break;
		}
		tokenizer.expect(messages, TokenKind::Comma)?;
	}

	let span = generic_token.span + names.last().as_ref().unwrap().span;
	let generic_atttribute = GenericAttribute { names };
	Ok(Node::new(generic_atttribute, span))
}

fn parse_intrinsic_attribute<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<IntrinsicAttribute>> {
	let intrinsic_token = tokenizer.expect_word(messages, "intrinsic")?;
	tokenizer.expect(messages, TokenKind::Newline)?;
	Ok(Node::from_token(IntrinsicAttribute, intrinsic_token))
}

fn parse_extern_attribute<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<ExternAttribute<'a>>> {
	let extern_token = tokenizer.expect_word(messages, "extern")?;
	let name_token = tokenizer.expect(messages, TokenKind::String)?;
	tokenizer.expect(messages, TokenKind::Newline)?;

	let attribute = ExternAttribute { name: name_token.text };
	let span = extern_token.span + name_token.span;
	Ok(Node::new(attribute, span))
}

fn parse_export_attribute<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<ExportAttribute<'a>>> {
	let export_token = tokenizer.expect_word(messages, "export")?;
	let name_token = tokenizer.expect(messages, TokenKind::String)?;
	tokenizer.expect(messages, TokenKind::Newline)?;

	let attribute = ExportAttribute { name: &name_token.text };
	let span = export_token.span + name_token.span;
	Ok(Node::new(attribute, span))
}

fn parse_import_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Import<'a>>> {
	let import_token = tokenizer.expect_word(messages, "import")?;

	let mut segments = Vec::new();
	let mut symbol_names = Vec::new();
	let mut end;

	loop {
		let token = tokenizer.expect(messages, TokenKind::Word)?;
		end = token.span;

		let word = Node::from_token(token.text, token);

		let peeked_kind = tokenizer.peek_kind();
		if peeked_kind != Ok(TokenKind::DoubleColon) {
			symbol_names.push(word);
			break;
		}

		segments.push(word);
		tokenizer.expect(messages, TokenKind::DoubleColon)?;
	}

	while tokenizer.peek_kind() == Ok(TokenKind::Comma) {
		tokenizer.expect(messages, TokenKind::Comma)?;

		let token = tokenizer.expect(messages, TokenKind::Word)?;
		end = token.span;

		let word = Node::from_token(token.text, token);
		symbol_names.push(word);
	}

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = import_token.span + end;
	if segments.is_empty() {
		messages.message(error!("Missing import path").span(span));
		return Err(());
	}

	let path_segments = PathSegments::Path { segments };
	let item = Import { path_segments, symbol_names };
	Ok(Node { item, span })
}

fn parse_path_segments<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<PathSegments<'a>>> {
	if tokenizer.peek_kind() == Ok(TokenKind::DoubleColon) {
		let double_colon = tokenizer.expect(messages, TokenKind::DoubleColon)?;
		let symbol_token = tokenizer.expect(messages, TokenKind::Word)?;

		let symbol_name = Node::from_token(symbol_token.text, symbol_token);
		let path = PathSegments::MainModule { symbol_name };
		let span = double_colon.span + symbol_token.span;
		return Ok(Node::new(path, span));
	}

	let mut segments = Vec::new();
	loop {
		let segment_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, segment_token, "path segment")?;

		segments.push(Node::from_token(segment_token.text, segment_token));

		if tokenizer.peek_kind() == Ok(TokenKind::DoubleColon) {
			tokenizer.expect(messages, TokenKind::DoubleColon)?;
		} else {
			break;
		}
	}

	let span = segments.first().unwrap().span + segments.last().unwrap().span;
	Ok(Node::new(PathSegments::Path { segments }, span))
}

fn parse_type<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Type<'a>>> {
	let parsed_type = match tokenizer.peek()? {
		Token { text: "Void", .. } => {
			let token = tokenizer.expect_word(messages, "Void")?;
			Node::from_token(Type::Void, token)
		}

		Token { kind: TokenKind::Ampersand, .. } => {
			let ampersand = tokenizer.expect(messages, TokenKind::Ampersand)?;

			let mutable = if matches!(tokenizer.peek(), Ok(Token { kind: TokenKind::Word, text: "mut", .. })) {
				tokenizer.expect_word(messages, "mut")?;
				true
			} else {
				false
			};

			let pointee = Box::new(parse_type(messages, tokenizer)?);
			let span = ampersand.span + pointee.span;
			Node::new(Type::Pointer { pointee, mutable }, span)
		}

		Token { kind: TokenKind::OpenBracket, .. } => {
			let opening = tokenizer.expect(messages, TokenKind::OpenBracket)?;
			tokenizer.expect(messages, TokenKind::CloseBracket)?;

			let mut mutable = false;
			if tokenizer.peek().map(|t| t.text) == Ok("mut") {
				tokenizer.expect_word(messages, "mut")?;
				mutable = true;
			}

			let pointee = Box::new(parse_type(messages, tokenizer)?);

			let span = opening.span + pointee.span;
			Node::new(Type::Slice { pointee, mutable }, span)
		}

		_ => {
			let path_segments = parse_path_segments(messages, tokenizer)?;

			let mut type_arguments = Vec::new();
			let span = match tokenizer.peek() {
				Ok(Token { kind: TokenKind::OpenGeneric, .. }) => {
					tokenizer.expect(messages, TokenKind::OpenGeneric)?;

					if tokenizer.peek_kind() != Ok(TokenKind::CompGreater) {
						loop {
							type_arguments.push(parse_type(messages, tokenizer)?);
							if tokenizer.peek_kind() != Ok(TokenKind::Comma) {
								break;
							}
							tokenizer.expect(messages, TokenKind::Comma)?;
						}
					}

					let close_bracket = tokenizer.expect(messages, TokenKind::CompGreater)?;
					path_segments.span + close_bracket.span
				}

				_ => path_segments.span,
			};

			Node::new(Type::Path { path_segments, type_arguments }, span)
		}
	};

	Ok(parsed_type)
}

fn parse_function_declaration<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	attributes: Attributes<'a>,
) -> ParseResult<Function<'a>> {
	let generics = match attributes.generic_attribute {
		Some(attribute) => attribute.item.names,
		None => Vec::new(),
	};
	let intrinsic_attribute = attributes.intrinsic_attribute;
	let extern_attribute = attributes.extern_attribute;
	let export_attribute = attributes.export_attribute;

	tokenizer.expect_word(messages, "fn")?;

	let name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token, "function name")?;
	let name = Node::from_token(name_token.text, name_token);

	let parameters = parse_parameters(messages, tokenizer)?;

	let parsed_type = if tokenizer.peek_kind() == Ok(TokenKind::Colon) {
		tokenizer.expect(messages, TokenKind::Colon)?;
		Some(parse_type(messages, tokenizer)?)
	} else {
		None
	};

	let block = if extern_attribute.is_some() || intrinsic_attribute.is_some() {
		None
	} else {
		Some(parse_block(messages, tokenizer)?)
	};

	Ok(Function {
		generics,
		intrinsic_attribute,
		extern_attribute,
		export_attribute,
		name,
		parameters,
		parsed_type,
		block,
	})
}

fn parse_parameters<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Vec<Node<Parameter<'a>>>> {
	tokenizer.expect(messages, TokenKind::OpenParen)?;

	let mut parameters = Vec::new();

	while !reached_close_paren(tokenizer) {
		let is_mutable = match tokenizer.peek() {
			Ok(Token { text: "mut", .. }) => {
				tokenizer.expect_word(messages, "mut")?;
				true
			}

			_ => false,
		};

		let name_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, name_token, "parameter name")?;
		let name = Node::from_token(name_token.text, name_token);

		tokenizer.expect(messages, TokenKind::Colon)?;

		let parsed_type = parse_type(messages, tokenizer)?;

		let span = name_token.span + parsed_type.span;
		let parameter = Parameter { name, parsed_type, is_mutable };
		parameters.push(Node::new(parameter, span));

		if reached_close_paren(tokenizer) {
			break;
		}

		tokenizer.expect(messages, TokenKind::Comma)?;
	}

	tokenizer.expect(messages, TokenKind::CloseParen)?;

	Ok(parameters)
}

fn parse_struct_declaration<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	attributes: Attributes<'a>,
) -> ParseResult<Struct<'a>> {
	let generics = match attributes.generic_attribute {
		Some(attribute) => attribute.item.names,
		None => Vec::new(),
	};

	tokenizer.expect_word(messages, "struct")?;

	let struct_name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, struct_name_token, "struct name")?;
	let name = Node::from_token(struct_name_token.text, struct_name_token);

	tokenizer.expect(messages, TokenKind::OpenBrace)?;

	let mut fields = Vec::new();

	if tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
		while tokenizer.peek_kind() == Ok(TokenKind::Newline) {
			tokenizer.expect(messages, TokenKind::Newline)?;
		}

		while !reached_close_brace(tokenizer) {
			let field_name_token = tokenizer.expect(messages, TokenKind::Word)?;
			check_not_reserved(messages, field_name_token, "struct field")?;
			let name = Node::from_token(field_name_token.text, field_name_token);

			tokenizer.expect(messages, TokenKind::Colon)?;

			let parsed_type = parse_type(messages, tokenizer)?;

			fields.push(Field { name, parsed_type });

			while tokenizer.peek_kind() == Ok(TokenKind::Newline) {
				tokenizer.expect(messages, TokenKind::Newline)?;
			}
		}
	}

	tokenizer.expect(messages, TokenKind::CloseBrace)?;

	Ok(Struct { generics, name, fields })
}

fn parse_const_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Const<'a>>> {
	let const_token = tokenizer.expect_word(messages, "const")?;

	let name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token, "const name")?;
	let name = Node::from_token(name_token.text, name_token);

	let parsed_type = if tokenizer.peek_kind() == Ok(TokenKind::Colon) {
		tokenizer.expect(messages, TokenKind::Colon)?;
		Some(parse_type(messages, tokenizer)?)
	} else {
		None
	};

	tokenizer.expect(messages, TokenKind::Equal)?;

	let expression = parse_expression(messages, tokenizer, true)?;

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = const_token.span + expression.span;
	let item = Const { name, parsed_type, expression };
	Ok(Node { item, span })
}

fn parse_binding_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Binding<'a>>> {
	let is_mutable;
	let keyword_token = if let Ok(Token { text: "mut", .. }) = tokenizer.peek() {
		is_mutable = true;
		tokenizer.expect_word(messages, "mut")?
	} else {
		is_mutable = false;
		tokenizer.expect_word(messages, "let")?
	};

	let name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token, "binding name")?;
	let name = Node::from_token(name_token.text, name_token);

	let parsed_type = if tokenizer.peek_kind() == Ok(TokenKind::Colon) {
		tokenizer.expect(messages, TokenKind::Colon)?;
		Some(parse_type(messages, tokenizer)?)
	} else {
		None
	};

	tokenizer.expect(messages, TokenKind::Equal)?;

	let expression = parse_expression(messages, tokenizer, true)?;

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = keyword_token.span + expression.span;
	let item = Binding { name, parsed_type, expression, is_mutable };
	Ok(Node { item, span })
}

fn parse_return_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Return<'a>>> {
	let return_token = tokenizer.expect_word(messages, "return")?;

	let expression = match tokenizer.peek_kind() {
		Ok(TokenKind::Newline) => None,
		_ => Some(parse_expression(messages, tokenizer, true)?),
	};

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = match &expression {
		Some(expression) => return_token.span + expression.span,
		None => return_token.span,
	};

	let item = Return { expression };
	Ok(Node { item, span })
}

fn check_not_reserved(messages: &mut Messages, token: Token, use_as: &str) -> ParseResult<()> {
	let is_reserved = matches!(token.text, "const" | "fn" | "let" | "mut" | "return" | "struct" | "import" | "generic");

	if is_reserved {
		messages.message(error!("Cannot use reserved word {:?} as {use_as}", token.text).span(token.span));
		Err(())
	} else {
		Ok(())
	}
}

fn reached_close_paren(tokenizer: &mut Tokenizer) -> bool {
	tokenizer
		.peek()
		.map(|peeked| peeked.kind == TokenKind::CloseParen)
		.unwrap_or(false)
}

fn reached_close_brace(tokenizer: &mut Tokenizer) -> bool {
	tokenizer
		.peek()
		.map(|peeked| peeked.kind == TokenKind::CloseBrace)
		.unwrap_or(false)
}

fn reached_close_generic(tokenizer: &mut Tokenizer) -> bool {
	tokenizer
		.peek()
		.map(|peeked| peeked.kind == TokenKind::CompGreater)
		.unwrap_or(false)
}

fn consume_error_syntax(messages: &mut Messages, tokenizer: &mut Tokenizer) {
	let mut brackets = 0;
	let mut parens = 0;
	let mut braces = 0;

	while let Ok(token) = tokenizer.peek() {
		let all_zero = brackets == 0 && parens == 0 && braces == 0;
		match token.kind {
			TokenKind::Newline if all_zero => break,

			TokenKind::OpenBracket => brackets += 1,
			TokenKind::CloseBracket => brackets -= 1,

			TokenKind::OpenParen => parens += 1,
			TokenKind::CloseParen => parens -= 1,

			TokenKind::OpenBrace => braces += 1,
			TokenKind::CloseBrace => braces -= 1,

			_ => {}
		}

		tokenizer
			.next_with_optional_messages(&mut None)
			.expect("This should never fail");
	}

	//Reached end of file while unbalanced
	if brackets != 0 {
		messages.message(error!("Unbalanced brackets"));
	}
	if parens != 0 {
		messages.message(error!("Unbalanced parentheses"));
	}
	if braces != 0 {
		messages.message(error!("Unbalanced braces"));
	}
}
