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

pub fn parse_block<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	allow_braceless: bool,
	consume_newline: bool,
) -> ParseResult<Node<Block<'a>>> {
	if allow_braceless && tokenizer.peek_kind() == Ok(TokenKind::FatArrow) {
		let fat_arrow = tokenizer.expect(messages, TokenKind::FatArrow)?;

		let token = tokenizer.peek()?;
		let statement = parse_statement(messages, tokenizer, &mut 0, Attributes::blank(), token, false);

		if let Some(statement) = &statement {
			if let Statement::Block(_) = statement {
				let warning = warning!(
					"A single statement block is extraneous if it only contains another block, consider removing the `=>`"
				);
				messages.message(warning.span(fat_arrow.span))
			}
		}

		let statements: Vec<_> = statement.into_iter().collect();

		let newline = tokenizer.expect(messages, TokenKind::Newline)?;

		let block = Block { statements };
		let span = fat_arrow.span + newline.span;
		return Ok(Node::new(block, span));
	}

	let open = tokenizer.expect(messages, TokenKind::OpenBrace)?;
	let statements = parse_statements(messages, tokenizer);
	let close = tokenizer.expect(messages, TokenKind::CloseBrace)?;

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	let block = Block { statements };
	let span = open.span + close.span;
	Ok(Node::new(block, span))
}

pub fn parse_statements<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> Vec<Statement<'a>> {
	let mut items = Vec::new();
	let mut next_function_index = 0;

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

		if token.kind == TokenKind::CloseBrace {
			break;
		}

		if let Some(statement) = parse_statement(messages, tokenizer, &mut next_function_index, attributes, token, true) {
			items.push(statement);
		}
	}

	items
}

fn parse_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	next_function_index: &mut usize,
	attributes: Attributes<'a>,
	token: Token<'a>,
	consume_newline: bool,
) -> Option<Statement<'a>> {
	match token {
		Token { kind: TokenKind::Word, text: "import", .. } => {
			disallow_all_attributes(messages, attributes, token.span, "An import statement");
			if let Ok(statement) = parse_import_statement(messages, tokenizer, consume_newline) {
				return Some(Statement::Import(statement));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::Word, text: "const", .. } => {
			disallow_all_attributes(messages, attributes, token.span, "A const definition");
			if let Ok(statement) = parse_const_statement(messages, tokenizer, consume_newline) {
				return Some(Statement::Const(Box::new(statement)));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::Word, text: "static", .. } => {
			const ALLOWED: AllowedAttributes = AllowedAttributes {
				generic_attribute: false,
				extern_attribute: true,
				export_attribute: false, // TODO: Allow exporting
				method_attribute: false,
				intrinsic_attribute: false,
				lang_attribute: false,
			};
			disallow_attributes(messages, &attributes, ALLOWED, "A static definition");
			if let Ok(statement) = parse_static_statement(messages, tokenizer, attributes, consume_newline) {
				return Some(Statement::Static(Box::new(statement)));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::Word, text: "let" | "mut", .. } => {
			disallow_all_attributes(messages, attributes, token.span, "A let statement");
			if let Ok(statement) = parse_binding_statement(messages, tokenizer, consume_newline) {
				return Some(Statement::Binding(Box::new(statement)));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::Word, text: "fn", .. } => {
			let index = *next_function_index;
			*next_function_index += 1;
			if let Ok(statement) = parse_function_declaration(messages, tokenizer, attributes, index, consume_newline) {
				return Some(Statement::Function(Box::new(statement)));
			} else {
				*next_function_index -= 1;
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::Word, text: "struct", .. } => {
			if let Ok(statement) = parse_struct_declaration(messages, tokenizer, attributes, consume_newline) {
				return Some(Statement::Struct(statement));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::Word, text: "enum", .. } => {
			if let Ok(statement) = parse_enum_declaration(messages, tokenizer, attributes, consume_newline) {
				return Some(Statement::Enum(statement));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::Word, text: "while", .. } => {
			disallow_all_attributes(messages, attributes, token.span, "A while loop");
			if let Ok(statement) = parse_while_statement(messages, tokenizer, consume_newline) {
				return Some(Statement::While(statement));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::Word, text: "break", .. } => {
			disallow_all_attributes(messages, attributes, token.span, "A break statement");
			if let Ok(statement) = parse_break_statement(messages, tokenizer, consume_newline) {
				return Some(Statement::Break(statement));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::Word, text: "continue", .. } => {
			disallow_all_attributes(messages, attributes, token.span, "A continue statement");
			if let Ok(statement) = parse_continue_statement(messages, tokenizer, consume_newline) {
				return Some(Statement::Continue(statement));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::Word, text: "return", .. } => {
			disallow_all_attributes(messages, attributes, token.span, "A return statement");
			if let Ok(statement) = parse_return_statement(messages, tokenizer, consume_newline) {
				return Some(Statement::Return(Box::new(statement)));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		Token { kind: TokenKind::OpenBrace, .. } => {
			disallow_all_attributes(messages, attributes, token.span, "A block");
			if let Ok(statement) = parse_block(messages, tokenizer, false, consume_newline) {
				return Some(Statement::Block(statement));
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}

		_ => {
			disallow_all_attributes(messages, attributes, token.span, "An expression");
			if let Ok(expression) = parse_expression(messages, tokenizer, true) {
				let expression = Statement::Expression(expression);

				let expect_newline = consume_newline && tokenizer.previous_kind() != Some(TokenKind::Newline);
				if expect_newline && tokenizer.expect(messages, TokenKind::Newline).is_err() {
					consume_error_syntax(messages, tokenizer);
				}

				return Some(expression);
			} else {
				consume_error_syntax(messages, tokenizer);
			}
		}
	}

	None
}

fn disallow_all_attributes(messages: &mut Messages, attributes: Attributes, span: Span, label: &str) {
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

fn disallow_attributes(messages: &mut Messages, attributes: &Attributes, allowed: AllowedAttributes, label: &str) {
	fn disallow<T>(messages: &mut Messages, attribute: &Option<Node<T>>, allowed: bool, label: &str, kind: &str) {
		if let Some(node) = attribute {
			if !allowed {
				let message = error!("{label} does not allow {kind} attribute");
				messages.message(message.span(node.span));
			}
		}
	}

	disallow(messages, &attributes.generic_attribute, allowed.generic_attribute, label, "generic");
	disallow(messages, &attributes.extern_attribute, allowed.extern_attribute, label, "extern");
	disallow(messages, &attributes.export_attribute, allowed.export_attribute, label, "export");
	disallow(messages, &attributes.method_attribute, allowed.method_attribute, label, "method");
	disallow(messages, &attributes.intrinsic_attribute, allowed.intrinsic_attribute, label, "intrinsic");
	disallow(messages, &attributes.lang_attribute, allowed.lang_attribute, label, "lang");
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
			atom = parse_following_period(messages, tokenizer, atom, allow_struct_literal)?;
		} else if tokenizer.peek_kind() == Ok(TokenKind::OpenBracket) {
			atom = parse_bracket_index(messages, tokenizer, atom)?;
		} else {
			break;
		}
	}

	loop {
		if let Some(operator) = tokenizer.peek().ok().and_then(token_to_operator) {
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

			let binary_operation = BinaryOperation { op: operator, left: atom, right };
			let expression = Expression::BinaryOperation(Box::new(binary_operation));

			atom = Node::new(expression, left_span + right_span);
		} else if let Ok(Token { text: "is", .. }) = tokenizer.peek() {
			tokenizer.expect_word(messages, "is")?;

			let (binding_name, variant_names) = {
				let first = tokenizer.expect(messages, TokenKind::Word)?;

				if tokenizer.peek_kind() == Ok(TokenKind::Colon) {
					tokenizer.expect(messages, TokenKind::Colon)?;
					let variant_name = tokenizer.expect(messages, TokenKind::Word)?;
					let binding_name = Some(Node::from_token(first.text, first));
					let variant_name = Node::from_token(variant_name.text, variant_name);
					(binding_name, vec![variant_name])
				} else {
					let mut variant_names = vec![Node::from_token(first.text, first)];

					while tokenizer.peek_kind() == Ok(TokenKind::Comma) {
						tokenizer.expect(messages, TokenKind::Comma)?;
						let name = tokenizer.expect(messages, TokenKind::Word)?;
						variant_names.push(Node::from_token(name.text, name));
					}

					(None, variant_names)
				}
			};

			let left_span = atom.span;
			let right_span = variant_names.last().unwrap().span;

			let check_is = CheckIs { left: atom, binding_name, variant_names };
			let expression = Expression::CheckIs(Box::new(check_is));

			atom = Node::new(expression, left_span + right_span);
		} else {
			break;
		}
	}

	Ok(atom)
}

fn token_to_operator(token: Token) -> Option<Node<BinaryOperator>> {
	let operator = match token.kind {
		TokenKind::Equal => BinaryOperator::Assign,

		TokenKind::Add => BinaryOperator::Add,
		TokenKind::AddAssign => BinaryOperator::AddAssign,
		TokenKind::Sub => BinaryOperator::Sub,
		TokenKind::SubAssign => BinaryOperator::SubAssign,
		TokenKind::Mul => BinaryOperator::Mul,
		TokenKind::MulAssign => BinaryOperator::MulAssign,
		TokenKind::Div => BinaryOperator::Div,
		TokenKind::DivAssign => BinaryOperator::DivAssign,
		TokenKind::Modulo => BinaryOperator::Modulo,
		TokenKind::ModuloAssign => BinaryOperator::ModuloAssign,

		TokenKind::BitshiftLeft => BinaryOperator::BitshiftLeft,
		TokenKind::BitshiftLeftAssign => BinaryOperator::BitshiftLeftAssign,
		TokenKind::BitshiftRight => BinaryOperator::BitshiftRight,
		TokenKind::BitshiftRightAssign => BinaryOperator::BitshiftRightAssign,

		TokenKind::Ampersand => BinaryOperator::BitwiseAnd,
		TokenKind::AmpersandAssign => BinaryOperator::BitwiseAndAssign,
		TokenKind::Pipe => BinaryOperator::BitwiseOr,
		TokenKind::PipeAssign => BinaryOperator::BitwiseOrAssign,
		TokenKind::Caret => BinaryOperator::BitwiseXor,
		TokenKind::CaretAssign => BinaryOperator::BitwiseXorAssign,

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
			let codepoint = parse_codepoint_contents(messages, codepoint_token)?;
			let value = Node::from_token(codepoint, codepoint_token);

			Ok(Node::from_token(
				Expression::CodepointLiteral(CodepointLiteral { value }),
				codepoint_token,
			))
		}

		TokenKind::ByteCodepoint => {
			let codepoint_token = tokenizer.expect(messages, TokenKind::ByteCodepoint)?;
			let codepoint = parse_codepoint_contents(messages, codepoint_token)?;

			// Tokenizer guarentees that this will be a single byte in the success case
			let value = Node::from_token(codepoint as u8, codepoint_token);

			Ok(Node::from_token(
				Expression::ByteCodepointLiteral(ByteCodepointLiteral { value }),
				codepoint_token,
			))
		}

		TokenKind::Number => {
			return parse_number(messages, tokenizer);
		}

		TokenKind::Word | TokenKind::DoubleColon => {
			match peeked.text {
				"if" => return parse_if_else_chain(messages, tokenizer),

				"match" => return parse_match(messages, tokenizer),

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
				let type_object = Type::Path { path_segments, type_arguments, dot_access: None };
				let parsed_type = Node::new(type_object, type_span);

				let initializer = parse_struct_initializer(messages, tokenizer)?;

				let span = type_span + initializer.span;
				let struct_literal = StructLiteral { parsed_type, initializer };

				return Ok(Node::new(Expression::StructLiteral(struct_literal), span));
			}

			let span = match &type_arguments {
				Some(type_arguments) => path_segments.span + type_arguments.span,
				None => path_segments.span,
			};
			let type_arguments = type_arguments.map(|node| node.item).unwrap_or_default();
			let read = Read { path_segments, type_arguments };

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
			let parsed_block = parse_block(messages, tokenizer, false, false)?;

			let span = parsed_block.span;
			let block = parsed_block.item;

			Ok(Node::new(Expression::Block(block), span))
		}

		TokenKind::Period => {
			let period = tokenizer.expect(messages, TokenKind::Period)?;
			let name_token = tokenizer.expect(messages, TokenKind::Word)?;
			let name = Node::from_token(name_token.text, name_token);

			let mut span = period.span + name.span;

			let initializer = if allow_struct_literal && tokenizer.peek_kind() == Ok(TokenKind::OpenBrace) {
				let struct_initializer = parse_struct_initializer(messages, tokenizer)?;
				span += struct_initializer.span;
				Some(EnumInitializer::StructLike { struct_initializer })
			} else if tokenizer.peek_kind() == Ok(TokenKind::OpenParen) {
				tokenizer.expect(messages, TokenKind::OpenParen)?;
				let expression = parse_expression(messages, tokenizer, true)?;
				let close_paren = tokenizer.expect(messages, TokenKind::CloseParen)?;
				span += close_paren.span;
				Some(EnumInitializer::Transparent { expression })
			} else {
				None
			};

			let inferred_enum = Box::new(InferredEnum { name, initializer });
			Ok(Node::new(Expression::InferredEnum(inferred_enum), span))
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
	allow_struct_literal: bool,
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

	if allow_struct_literal && tokenizer.peek_kind() == Ok(TokenKind::OpenBrace) {
		let struct_initializer = parse_struct_initializer(messages, tokenizer)?;
		let span = atom.span + name.span;
		let enum_initializer = Some(EnumInitializer::StructLike { struct_initializer });
		let field_read = Box::new(DotAccess { base: atom, name, enum_initializer });
		return Ok(Node::new(Expression::DotAcccess(field_read), span));
	}

	if let Ok(TokenKind::OpenParen | TokenKind::OpenGeneric) = tokenizer.peek_kind() {
		let type_arguments = parse_type_arguments(messages, tokenizer)?.map(|a| a.item).unwrap_or_default();
		let arguments_node = parse_arguments(messages, tokenizer)?;
		let span = atom.span + arguments_node.span;
		let arguments = arguments_node.item;
		let method_call = MethodCall { base: atom, name, type_arguments, arguments };
		return Ok(Node::new(Expression::MethodCall(Box::new(method_call)), span));
	}

	let span = atom.span + name.span;
	let field_read = Box::new(DotAccess { base: atom, name, enum_initializer: None });
	return Ok(Node::new(Expression::DotAcccess(field_read), span));
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

fn parse_codepoint_contents(messages: &mut Messages, token: Token) -> ParseResult<char> {
	let bytes = token.text.as_bytes();
	assert!(bytes.len() >= 1);

	if bytes[0] == b'\\' {
		assert!(bytes.len() == 2);
		let escape_sequence = match bytes[1] {
			b'n' => b'\n',
			b'r' => b'\r',
			b't' => b'\t',
			b'\\' => b'\\',
			b'\'' => b'\'',
			b'0' => b'\0',

			_ => {
				let error = error!("Unrecognized codepoint escape sequence `{}`", &token.text[..2]);
				messages.message(error.span(token.span));
				return Err(());
			}
		};

		return Ok(escape_sequence as char);
	}

	Ok(token.text.chars().next().unwrap())
}

fn parse_string_contents(string: &str) -> Cow<str> {
	let mut allocated = String::new();

	let mut index = 0;
	let mut last_extend_index = 0;
	while index < string.len() {
		let mut escape = "";
		match string[index..].as_bytes() {
			[b'\\', b'n', ..] => escape = "\n",
			[b'\\', b'r', ..] => escape = "\r",
			[b'\\', b't', ..] => escape = "\t",
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

fn parse_if_else_chain<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Expression<'a>>> {
	let mut entries = Vec::new();
	let mut else_body = None;

	let if_token = tokenizer.expect_word(messages, "if")?;
	let mut span = if_token.span;

	let condition = parse_expression(messages, tokenizer, false)?;
	let body = parse_block(messages, tokenizer, true, false)?;
	span += body.span;
	entries.push(IfElseChainEntry { condition, body });

	while let Ok(Token { text: "else", .. }) = tokenizer.peek() {
		tokenizer.expect_word(messages, "else")?;

		if let Ok(Token { text: "if", .. }) = tokenizer.peek() {
			tokenizer.expect_word(messages, "if")?;
			let condition = parse_expression(messages, tokenizer, false)?;
			let body = parse_block(messages, tokenizer, true, false)?;
			span += body.span;
			entries.push(IfElseChainEntry { condition, body });
		} else {
			let body = parse_block(messages, tokenizer, true, false)?;
			span += body.span;
			else_body = Some(body);
			break;
		}
	}

	let value = IfElseChain { entries, else_body };
	let expression = Expression::IfElseChain(Box::new(value));
	Ok(Node::new(expression, span))
}

fn parse_match<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Expression<'a>>> {
	let match_token = tokenizer.expect_word(messages, "match")?;

	let expression = parse_expression(messages, tokenizer, false)?;

	tokenizer.expect(messages, TokenKind::OpenBrace)?;

	let mut arms = Vec::new();
	let mut else_arm: Option<ElseArm> = None;
	let mut warned_after_else = false;

	while tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
		tokenizer.consume_newlines(messages);

		let (binding_name, variant_names) = {
			let first = tokenizer.expect(messages, TokenKind::Word)?;

			if first.text == "else" {
				let block = parse_block(messages, tokenizer, true, true)?;

				if let Some(else_arm) = &else_arm {
					let error = error!("Match expression may not have multiple else arms");
					let noted = error.note(note!(else_arm.else_span, "Previous else arm here"));
					messages.message(noted.span(first.span));
				}

				else_arm = Some(ElseArm { block, else_span: first.span });
				continue;
			}

			if let Some(else_arm) = &else_arm {
				if !warned_after_else {
					let warning = warning!("Match expression arm should not follow else arm");
					let noted = warning.note(note!(else_arm.else_span, "Match else arm here, it should be the last arm"));
					messages.message(noted.span(first.span));
				}

				warned_after_else = true;
			}

			if tokenizer.peek_kind() == Ok(TokenKind::Colon) {
				tokenizer.expect(messages, TokenKind::Colon)?;
				let variant_name = tokenizer.expect(messages, TokenKind::Word)?;
				let binding_name = Some(Node::from_token(first.text, first));
				let variant_name = Node::from_token(variant_name.text, variant_name);
				(binding_name, vec![variant_name])
			} else {
				let mut variant_names = vec![Node::from_token(first.text, first)];

				while tokenizer.peek_kind() == Ok(TokenKind::Comma) {
					tokenizer.expect(messages, TokenKind::Comma)?;
					let name = tokenizer.expect(messages, TokenKind::Word)?;
					variant_names.push(Node::from_token(name.text, name));
				}

				(None, variant_names)
			}
		};

		let block = parse_block(messages, tokenizer, true, true)?;

		let arm = MatchArm { binding_name, variant_names, block };
		arms.push(arm);
	}

	let close_brace = tokenizer.expect(messages, TokenKind::CloseBrace)?;
	let span = match_token.span + close_brace.span;

	let boxed_match = Box::new(Match { expression, arms, else_arm });
	let expression = Expression::Match(boxed_match);
	Ok(Node::new(expression, span))
}

fn parse_while_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	consume_newline: bool,
) -> ParseResult<Node<While<'a>>> {
	let while_token = tokenizer.expect_word(messages, "while")?;

	let condition = parse_expression(messages, tokenizer, false)?;
	let body = parse_block(messages, tokenizer, true, consume_newline)?;

	let span = while_token.span + body.span;
	let value = While { condition, body };
	Ok(Node::new(value, span))
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
	while tokenizer.peek_kind() != Ok(TokenKind::CompGreater) {
		types.push(parse_type(messages, tokenizer)?);

		if tokenizer.peek_kind() == Ok(TokenKind::CompGreater) {
			break;
		}

		tokenizer.expect(messages, TokenKind::Comma)?;
	}

	let close_token = tokenizer.expect(messages, TokenKind::CompGreater)?;

	let span = open_token.span + close_token.span;
	if types.is_empty() {
		// Should this be a validation error instead?
		messages.message(error!("Empty type argument list").span(span));
	}

	Ok(Some(Node::new(types, span)))
}

fn parse_arguments<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Vec<Node<Expression<'a>>>>> {
	let open_paren_token = tokenizer.expect(messages, TokenKind::OpenParen)?;

	let multi_line = tokenizer.peek_kind() == Ok(TokenKind::Newline);
	if multi_line {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	let mut expressions = Vec::new();
	while tokenizer.peek_kind() != Ok(TokenKind::CloseParen) {
		expressions.push(parse_expression(messages, tokenizer, true)?);

		if tokenizer.peek_kind() == Ok(TokenKind::CloseParen) {
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

	tokenizer.consume_newlines(messages);

	let mut field_initializers = Vec::new();

	while tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
		let name_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, name_token, "field name")?;
		let name = Node::from_token(name_token.text, name_token);

		let expression = if tokenizer.peek_kind() == Ok(TokenKind::Colon) {
			tokenizer.expect(messages, TokenKind::Colon)?;
			parse_expression(messages, tokenizer, true)?
		} else {
			let path = PathSegments { segments: vec![name] };
			let type_arguments = Vec::new();
			let read = Read { path_segments: Node::new(path, name.span), type_arguments };
			Node::new(Expression::Read(read), name.span)
		};

		field_initializers.push(FieldInitializer { name, expression });
		if tokenizer.peek_kind() == Ok(TokenKind::CloseBrace) {
			break;
		}

		if tokenizer.peek_kind() == Ok(TokenKind::Comma) {
			tokenizer.expect(messages, TokenKind::Comma)?;
			tokenizer.consume_newlines(messages);
		} else {
			tokenizer.expect(messages, TokenKind::Newline)?;
		}
	}

	let close_brace_token = tokenizer.expect(messages, TokenKind::CloseBrace)?;

	let initializer = StructInitializer { field_initializers };
	let span = open_brace_token.span + close_brace_token.span;
	Ok(Node::new(initializer, span))
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
		if peeked.kind == TokenKind::Word {
			match peeked.text {
				"generic" => {
					check_duplicate_attribute(messages, &attributes.generic_attribute, "generic", peeked.span)?;
					attributes.generic_attribute = Some(parse_generic_attribute(messages, tokenizer)?);
				}

				"extern" => {
					check_duplicate_attribute(messages, &attributes.extern_attribute, "extern", peeked.span)?;
					attributes.extern_attribute = Some(parse_extern_attribute(messages, tokenizer)?);
				}

				"export" => {
					check_duplicate_attribute(messages, &attributes.export_attribute, "export", peeked.span)?;
					attributes.export_attribute = Some(parse_export_attribute(messages, tokenizer)?);
				}

				"method" => {
					check_duplicate_attribute(messages, &attributes.method_attribute, "method", peeked.span)?;
					attributes.method_attribute = Some(parse_method_attribute(messages, tokenizer)?);
				}

				_ => break,
			}
		} else if peeked.kind == TokenKind::PoundSign {
			tokenizer.expect(messages, TokenKind::PoundSign)?;
			match tokenizer.peek().map(|t| t.text) {
				Ok("intrinsic") => {
					check_duplicate_attribute(messages, &attributes.intrinsic_attribute, "intrinsic", peeked.span)?;
					attributes.intrinsic_attribute = Some(parse_intrinsic_attribute(messages, tokenizer, peeked.span)?);
				}

				Ok("lang") => {
					check_duplicate_attribute(messages, &attributes.lang_attribute, "lang", peeked.span)?;
					attributes.lang_attribute = Some(parse_lang_attribute(messages, tokenizer, peeked.span)?);
				}

				_ => break,
			}
		} else {
			break;
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

	let attribute = ExportAttribute { name: name_token.text };
	let span = export_token.span + name_token.span;
	Ok(Node::new(attribute, span))
}

fn parse_method_attribute<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<MethodAttribute<'a>>> {
	let method_token = tokenizer.expect_word(messages, "method")?;

	let kind = match tokenizer.peek() {
		Ok(Token { text: "mut", .. }) => {
			tokenizer.expect_word(messages, "mut")?;
			MethodKind::MutableSelf
		}

		Ok(Token { text: "static", .. }) => {
			tokenizer.expect_word(messages, "static")?;
			MethodKind::Static
		}

		_ => MethodKind::ImmutableSelf,
	};

	let base_type = parse_path_segments(messages, tokenizer)?;
	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = method_token.span + base_type.span;
	let attribute = MethodAttribute { base_type, kind };
	Ok(Node::new(attribute, span))
}

fn parse_intrinsic_attribute<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	pound_sign_span: Span,
) -> ParseResult<Node<IntrinsicAttribute>> {
	let intrinsic_token = tokenizer.expect_word(messages, "intrinsic")?;
	tokenizer.expect(messages, TokenKind::Newline)?;
	let span = pound_sign_span + intrinsic_token.span;
	Ok(Node::new(IntrinsicAttribute, span))
}

fn parse_lang_attribute<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	pound_sign_span: Span,
) -> ParseResult<Node<LangAttribute<'a>>> {
	let lang_token = tokenizer.expect_word(messages, "lang")?;
	let name_token = tokenizer.expect(messages, TokenKind::String)?;
	tokenizer.expect(messages, TokenKind::Newline)?;

	let attribute = LangAttribute { name: name_token.text };
	let span = pound_sign_span + lang_token.span + name_token.span;
	Ok(Node::new(attribute, span))
}

fn parse_import_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	consume_newline: bool,
) -> ParseResult<Node<Import<'a>>> {
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

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	let span = import_token.span + end;
	if segments.is_empty() {
		messages.message(error!("Missing import path").span(span));
		return Err(());
	}

	let path_segments = PathSegments { segments };
	let item = Import { path_segments, symbol_names };
	Ok(Node { item, span })
}

fn parse_path_segments<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<PathSegments<'a>>> {
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
	Ok(Node::new(PathSegments { segments }, span))
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

			let dot_access = if tokenizer.peek_kind() == Ok(TokenKind::Period) {
				let dot_token = tokenizer.expect(messages, TokenKind::Period)?;
				let name_token = tokenizer.expect(messages, TokenKind::Word)?;
				let span = dot_token.span + name_token.span;
				Some(Node::new(name_token.text, span))
			} else {
				None
			};

			let path = Type::Path { path_segments, type_arguments, dot_access };
			Node::new(path, span)
		}
	};

	Ok(parsed_type)
}

fn parse_function_declaration<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	attributes: Attributes<'a>,
	index_in_block: usize,
	consume_newline: bool,
) -> ParseResult<Function<'a>> {
	let generics = match attributes.generic_attribute {
		Some(attribute) => attribute.item.names,
		None => Vec::new(),
	};
	let extern_attribute = attributes.extern_attribute;
	let export_attribute = attributes.export_attribute;
	let method_attribute = attributes.method_attribute;
	let intrinsic_attribute = attributes.intrinsic_attribute;
	let lang_attribute = attributes.lang_attribute;

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
		Some(parse_block(messages, tokenizer, false, false)?)
	};

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	Ok(Function {
		generics,
		extern_attribute,
		export_attribute,
		method_attribute,
		intrinsic_attribute,
		lang_attribute,
		name,
		parameters,
		parsed_type,
		block,
		index_in_block,
	})
}

fn parse_parameters<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Parameters<'a>> {
	tokenizer.expect(messages, TokenKind::OpenParen)?;

	let mut parameters = Vec::new();
	let mut c_vararg = None;

	while tokenizer.peek_kind() != Ok(TokenKind::CloseParen) {
		let is_mutable = match tokenizer.peek() {
			Ok(Token { text: "mut", .. }) => {
				tokenizer.expect_word(messages, "mut")?;
				true
			}

			_ => false,
		};

		if tokenizer.peek_kind() == Ok(TokenKind::Period) {
			let a = tokenizer.expect(messages, TokenKind::Period)?;
			let b = tokenizer.expect(messages, TokenKind::Period)?;
			let c = tokenizer.expect(messages, TokenKind::Period)?;
			c_vararg = Some(a.span + b.span + c.span);
			break;
		}

		let name_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, name_token, "parameter name")?;
		let name = Node::from_token(name_token.text, name_token);

		tokenizer.expect(messages, TokenKind::Colon)?;

		let parsed_type = parse_type(messages, tokenizer)?;

		let span = name_token.span + parsed_type.span;
		let parameter = Parameter { name, parsed_type, is_mutable };
		parameters.push(Node::new(parameter, span));

		if tokenizer.peek_kind() == Ok(TokenKind::CloseParen) {
			break;
		}

		tokenizer.expect(messages, TokenKind::Comma)?;
	}

	tokenizer.expect(messages, TokenKind::CloseParen)?;

	Ok(Parameters { parameters, c_varargs: c_vararg })
}

fn parse_struct_declaration<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	attributes: Attributes<'a>,
	consume_newline: bool,
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

		while tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
			let field_name_token = tokenizer.expect(messages, TokenKind::Word)?;
			let field = parse_field(messages, tokenizer, field_name_token, "struct field")?;
			fields.push(field);

			tokenizer.expect(messages, TokenKind::Newline)?;
			while tokenizer.peek_kind() == Ok(TokenKind::Newline) {
				tokenizer.expect(messages, TokenKind::Newline)?;
			}
		}
	}

	tokenizer.expect(messages, TokenKind::CloseBrace)?;

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	Ok(Struct { generics, name, fields })
}

fn parse_enum_declaration<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	attributes: Attributes<'a>,
	consume_newline: bool,
) -> ParseResult<Enum<'a>> {
	let generics = match attributes.generic_attribute {
		Some(attribute) => attribute.item.names,
		None => Vec::new(),
	};

	tokenizer.expect_word(messages, "enum")?;

	let enum_name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, enum_name_token, "enum name")?;
	let name = Node::from_token(enum_name_token.text, enum_name_token);

	tokenizer.expect(messages, TokenKind::OpenBrace)?;

	let mut shared_fields = Vec::new();
	let mut variants = Vec::new();
	let mut transparent_variant_count = 0;

	if tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
		while tokenizer.peek_kind() == Ok(TokenKind::Newline) {
			tokenizer.expect(messages, TokenKind::Newline)?;
		}

		while tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
			let field_name_token = tokenizer.expect(messages, TokenKind::Word)?;
			if tokenizer.peek_kind() == Ok(TokenKind::Colon) {
				if !variants.is_empty() {
					let warning = warning!("Enum shared fields should proceed all variants, consider moving this field up");
					messages.message(warning.span(field_name_token.span));
				}

				let field = parse_field(messages, tokenizer, field_name_token, "enum shared field")?;
				shared_fields.push(field);
			} else {
				check_not_reserved(messages, field_name_token, "enum variant")?;
				let mut variant_fields = Vec::new();

				if tokenizer.peek_kind() == Ok(TokenKind::OpenBrace) {
					tokenizer.expect(messages, TokenKind::OpenBrace)?;
					let multi_line = if tokenizer.peek_kind() == Ok(TokenKind::Newline) {
						tokenizer.expect(messages, TokenKind::Newline)?;
						true
					} else {
						false
					};

					while tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
						while tokenizer.peek_kind() == Ok(TokenKind::Newline) {
							tokenizer.expect(messages, TokenKind::Newline)?;
						}

						let field_name_token = tokenizer.expect(messages, TokenKind::Word)?;
						let field = parse_field(messages, tokenizer, field_name_token, "enum variant field")?;
						variant_fields.push(field);

						if !multi_line {
							break;
						}

						tokenizer.expect(messages, TokenKind::Newline)?;
					}

					tokenizer.expect(messages, TokenKind::CloseBrace)?;
				} else if tokenizer.peek_kind() == Ok(TokenKind::OpenParen) {
					tokenizer.expect(messages, TokenKind::OpenParen)?;
					let parsed_type = parse_type(messages, tokenizer)?;
					tokenizer.expect(messages, TokenKind::CloseParen)?;

					let name = Node::from_token(field_name_token.text, field_name_token);
					let transparent = TransparentVariant { name, parsed_type };
					let variant = EnumVariant::Transparent(transparent);
					variants.push(variant);
					transparent_variant_count += 1;

					tokenizer.expect(messages, TokenKind::Newline)?;
					tokenizer.consume_newlines(messages);
					continue;
				}

				let name = Node::from_token(field_name_token.text, field_name_token);
				let struct_like = StructLikeVariant { name, fields: variant_fields };
				let variant = EnumVariant::StructLike(struct_like);
				variants.push(variant);
			}

			tokenizer.expect(messages, TokenKind::Newline)?;
			tokenizer.consume_newlines(messages);
		}
	}

	tokenizer.expect(messages, TokenKind::CloseBrace)?;

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	Ok(Enum {
		generics,
		name,
		shared_fields,
		variants,
		transparent_variant_count,
	})
}

fn parse_field<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	field_name_token: Token<'a>,
	label: &str,
) -> ParseResult<Field<'a>> {
	check_not_reserved(messages, field_name_token, label)?;
	let name = Node::from_token(field_name_token.text, field_name_token);

	tokenizer.expect(messages, TokenKind::Colon)?;
	let parsed_type = parse_type(messages, tokenizer)?;

	let mut read_only = false;
	let attribute = match tokenizer.peek() {
		Ok(Token { text: "readable", .. }) => {
			let token = tokenizer.expect_word(messages, "readable")?;
			Some(Node::from_token(FieldAttribute::Readable, token))
		}

		Ok(Token { text: "private", .. }) => {
			let token = tokenizer.expect_word(messages, "private")?;
			if let Ok(Token { text: "readonly", .. }) = tokenizer.peek() {
				tokenizer.expect_word(messages, "readonly")?;
				read_only = true;
			}
			Some(Node::from_token(FieldAttribute::Private, token))
		}

		_ => {
			if let Ok(Token { text: "readonly", .. }) = tokenizer.peek() {
				tokenizer.expect_word(messages, "readonly")?;
				read_only = true;
			}
			None
		}
	};

	Ok(Field { name, parsed_type, attribute, read_only })
}

fn parse_const_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	consume_newline: bool,
) -> ParseResult<Node<Const<'a>>> {
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

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	let span = const_token.span + expression.span;
	let item = Const { name, parsed_type, expression };
	Ok(Node { item, span })
}

fn parse_static_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	attributes: Attributes<'a>,
	consume_newline: bool,
) -> ParseResult<Node<Static<'a>>> {
	let keyword_token = tokenizer.expect_word(messages, "static")?;

	let name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token, "static name")?;
	let name = Node::from_token(name_token.text, name_token);

	tokenizer.expect(messages, TokenKind::Colon)?;
	let parsed_type = parse_type(messages, tokenizer)?;

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	let span = keyword_token.span + parsed_type.span;
	let item = Static {
		name,
		parsed_type,
		extern_attribute: attributes.extern_attribute,
	};

	Ok(Node { item, span })
}

fn parse_binding_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	consume_newline: bool,
) -> ParseResult<Node<Binding<'a>>> {
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

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	let span = keyword_token.span + expression.span;
	let item = Binding { name, parsed_type, expression, is_mutable };
	Ok(Node { item, span })
}

fn parse_break_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	consume_newline: bool,
) -> ParseResult<Node<Break>> {
	let break_token = tokenizer.expect_word(messages, "break")?;

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	Ok(Node::from_token(Break, break_token))
}

fn parse_continue_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	consume_newline: bool,
) -> ParseResult<Node<Continue>> {
	let continue_token = tokenizer.expect_word(messages, "continue")?;

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	Ok(Node::from_token(Continue, continue_token))
}

fn parse_return_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	consume_newline: bool,
) -> ParseResult<Node<Return<'a>>> {
	let return_token = tokenizer.expect_word(messages, "return")?;

	let expression = match tokenizer.peek_kind() {
		Ok(TokenKind::Newline) => None,
		_ => Some(parse_expression(messages, tokenizer, true)?),
	};

	if consume_newline {
		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	let span = match &expression {
		Some(expression) => return_token.span + expression.span,
		None => return_token.span,
	};

	let item = Return { expression };
	Ok(Node { item, span })
}

fn check_not_reserved(messages: &mut Messages, token: Token, use_as: &str) -> ParseResult<()> {
	let is_reserved = matches!(
		token.text,
		"const"
			| "fn" | "let"
			| "mut" | "return"
			| "struct" | "enum"
			| "import" | "generic"
			| "extern" | "export"
			| "method" | "if"
			| "else" | "while"
			| "or" | "and"
			| "break" | "continue"
	);

	if is_reserved {
		let error = error!("Cannot use reserved word {:?} as {use_as}", token.text);
		messages.message(error.span(token.span));
		Err(())
	} else {
		Ok(())
	}
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
			.next_with_optional_messages(&mut None, false)
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
