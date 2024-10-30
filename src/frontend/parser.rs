use std::borrow::Cow;

use bumpalo::collections::Vec as BumpVec;
use bumpalo::vec as bump_vec;
use bumpalo::Bump;
use rust_decimal::Decimal;

use crate::frontend::error::{Messages, ParseResult};
use crate::frontend::file::SourceFile;
use crate::frontend::span::Span;
use crate::frontend::tokenizer::{Token, TokenKind, Tokenizer, Tokens};
use crate::frontend::tree::*;

pub fn parse_file<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>, file: &'a SourceFile) -> File<'a> {
	let block = parse_root_block(bump, messages, tokens);

	File {
		source_file: file,
		line_starts: tokens.take_line_starts(),
		module_path: &file.module_path,
		block,
	}
}

pub fn parse_root_block<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> Block<'a> {
	let statements = parse_statements(bump, messages, tokens);
	Block { statements }
}

pub fn parse_block<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	allow_braceless: bool,
) -> ParseResult<Node<Block<'a>>> {
	if allow_braceless && tokens.peek_kind() == Ok(TokenKind::FatArrow) {
		let fat_arrow = tokens.next(messages)?;

		let statement = parse_statement(bump, messages, tokens, Attributes::blank());
		if let Some(Statement::Block(_)) = &statement {
			let warning =
				warning!("A single statement block is extraneous if it only contains another block, consider removing the `=>`");
			messages.message(warning.span(fat_arrow.span))
		}

		let statements = if let Some(statement) = statement {
			bump_vec![in bump; statement]
		} else {
			BumpVec::new_in(bump)
		};

		let newline = tokens.expect_peek(messages, TokenKind::Newline)?;

		let block = Block { statements: statements.into_bump_slice() };
		let span = fat_arrow.span + newline.span;
		return Ok(Node::new(block, span));
	}

	let open = tokens.expect(messages, TokenKind::OpenBrace)?;
	let statements = parse_statements(bump, messages, tokens);
	let close = tokens.expect(messages, TokenKind::CloseBrace)?;

	let block = Block { statements };
	let span = open.span + close.span;
	Ok(Node::new(block, span))
}

pub fn parse_statements<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> &'a [Statement<'a>] {
	let mut items = BumpVec::new_in(bump);

	loop {
		while let Ok(token) = tokens.peek() {
			if token.kind != TokenKind::Newline {
				break;
			}

			if tokens.next(messages).is_err() {
				return items.into_bump_slice();
			}
		}

		let attributes = match parse_attributes(bump, messages, tokens) {
			Ok(attributes) => attributes,

			Err(_) => {
				consume_error_syntax(messages, tokens);
				continue;
			}
		};

		tokens.consume_newlines();

		let token = match tokens.peek() {
			Ok(token) => token,
			Err(_) => return items.into_bump_slice(),
		};

		if token.kind == TokenKind::CloseBrace {
			break;
		}

		if let Some(statement) = parse_statement(bump, messages, tokens, attributes) {
			items.push(statement);
		}
	}

	items.into_bump_slice()
}

fn parse_statement<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	attributes: Attributes<'a>,
) -> Option<Statement<'a>> {
	let peeked = tokens.peek().ok()?;
	match peeked {
		Token { kind: TokenKind::Word, text: "import", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "An import statement");
			if let Ok(statement) = parse_import_statement(bump, messages, tokens) {
				return Some(Statement::Import(statement));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "const", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A const definition");
			if let Ok(statement) = parse_const_statement(bump, messages, tokens) {
				return Some(Statement::Const(bump.alloc(statement)));
			} else {
				consume_error_syntax(messages, tokens);
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

			if let Ok(statement) = parse_static_statement(bump, messages, tokens, attributes) {
				return Some(Statement::Static(bump.alloc(statement)));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "when", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A while statement");
			if let Ok(statement) = parse_when_else_chain(bump, messages, tokens) {
				return Some(Statement::WhenElseChain(statement));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "let" | "mut", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A let statement");
			if let Ok(statement) = parse_binding_statement(bump, messages, tokens) {
				return Some(Statement::Binding(bump.alloc(statement)));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "fn", .. } => {
			if let Ok(statement) = parse_function_declaration(bump, messages, tokens, attributes) {
				return Some(Statement::Function(bump.alloc(statement)));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "struct", .. } => {
			const ALLOWED: AllowedAttributes = AllowedAttributes {
				generic_attribute: true,
				extern_attribute: false,
				export_attribute: false,
				method_attribute: false,
				intrinsic_attribute: false,
				lang_attribute: true,
			};
			disallow_attributes(messages, &attributes, ALLOWED, "A struct definition");

			if let Ok(statement) = parse_struct_declaration(bump, messages, tokens, attributes) {
				return Some(Statement::Struct(statement));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "enum", .. } => {
			const ALLOWED: AllowedAttributes = AllowedAttributes {
				generic_attribute: true,
				extern_attribute: false,
				export_attribute: false,
				method_attribute: false,
				intrinsic_attribute: false,
				lang_attribute: true,
			};
			disallow_attributes(messages, &attributes, ALLOWED, "An enum definition");

			if let Ok(statement) = parse_enum_declaration(bump, messages, tokens, attributes) {
				return Some(Statement::Enum(statement));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "if", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "An if-else statement");
			if let Ok(chain) = parse_if_else_chain(bump, messages, tokens) {
				return Some(Statement::IfElseChain(chain));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "match", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A match statement");
			if let Ok(expression) = parse_match(bump, messages, tokens) {
				return Some(Statement::Match(expression));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "while", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A while loop");
			if let Ok(statement) = parse_while_statement(bump, messages, tokens) {
				return Some(Statement::While(statement));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "for", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A for loop");
			if let Ok(statement) = parse_for_statement(bump, messages, tokens) {
				return Some(Statement::For(statement));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "defer", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A defer statement");
			if let Ok(statement) = parse_defer_statement(bump, messages, tokens) {
				return Some(Statement::Defer(bump.alloc(statement)));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "break", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A break statement");
			if let Ok(statement) = parse_break_statement(messages, tokens) {
				return Some(Statement::Break(statement));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "continue", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A continue statement");
			if let Ok(statement) = parse_continue_statement(messages, tokens) {
				return Some(Statement::Continue(statement));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "yield", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A yield statement");
			if let Ok(statement) = parse_yield_statement(bump, messages, tokens) {
				return Some(Statement::Yield(bump.alloc(statement)));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::Word, text: "return", .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A return statement");
			if let Ok(statement) = parse_return_statement(bump, messages, tokens) {
				return Some(Statement::Return(bump.alloc(statement)));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		Token { kind: TokenKind::OpenBrace, .. } => {
			disallow_all_attributes(messages, attributes, peeked.span, "A block");
			if let Ok(statement) = parse_block(bump, messages, tokens, false) {
				return Some(Statement::Block(statement));
			} else {
				consume_error_syntax(messages, tokens);
			}
		}

		_ => {
			disallow_all_attributes(messages, attributes, peeked.span, "An expression");
			if let Ok(expression) = parse_expression(bump, messages, tokens, true) {
				let expression = Statement::Expression(expression);

				if tokens.expect_peek(messages, TokenKind::Newline).is_err() {
					consume_error_syntax(messages, tokens);
				}

				return Some(expression);
			} else {
				consume_error_syntax(messages, tokens);
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
	fn disallow<T>(messages: &mut Messages, attribute: Option<&Node<T>>, allowed: bool, label: &str, kind: &str) {
		if let Some(node) = attribute {
			if !allowed {
				let message = error!("{label} does not allow {kind} attribute");
				messages.message(message.span(node.span));
			}
		}
	}

	disallow(messages, attributes.generic_attribute, allowed.generic_attribute, label, "generic");
	disallow(messages, attributes.extern_attribute, allowed.extern_attribute, label, "extern");
	disallow(messages, attributes.export_attribute, allowed.export_attribute, label, "export");
	disallow(messages, attributes.method_attribute, allowed.method_attribute, label, "method");
	disallow(messages, attributes.intrinsic_attribute, allowed.intrinsic_attribute, label, "intrinsic");
	disallow(messages, attributes.lang_attribute, allowed.lang_attribute, label, "lang");
}

fn parse_expression<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	allow_struct_literal: bool,
) -> ParseResult<Node<Expression<'a>>> {
	parse_expression_climb(bump, messages, tokens, allow_struct_literal, 0)
}

fn parse_expression_climb<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	allow_struct_literal: bool,
	min_precedence: u32,
) -> ParseResult<Node<Expression<'a>>> {
	let mut atom = parse_expression_atom(bump, messages, tokens, allow_struct_literal)?;

	loop {
		if tokens.peek_kind() == Ok(TokenKind::Period) {
			tokens.next(messages)?;
			atom = parse_following_period(bump, messages, tokens, atom, allow_struct_literal)?;
		} else if tokens.peek_kind() == Ok(TokenKind::OpenBracket) {
			atom = parse_bracket_index(bump, messages, tokens, atom)?;
		} else {
			break;
		}
	}

	loop {
		if let Some(operator) = tokens.peek().ok().and_then(token_to_operator) {
			let precedence = operator.item.precedence();
			if precedence < min_precedence {
				break;
			}

			tokens.next(messages).expect("Known peeked token");

			let associativity = operator.item.associativity();
			let next_min_precedence = match associativity {
				Associativity::Left => precedence + 1,
				Associativity::Right => precedence,
			};

			let right = parse_expression_climb(bump, messages, tokens, allow_struct_literal, next_min_precedence)?;
			let left_span = atom.span;
			let right_span = right.span;

			let binary_operation = BinaryOperation { op: operator, left: atom, right };
			let expression = Expression::BinaryOperation(bump.alloc(binary_operation));

			atom = Node::new(expression, left_span + right_span);
		} else if let Ok(Token { text: "is", .. }) = tokens.peek() {
			tokens.next(messages)?;

			let (binding_name, variant_names) = {
				let first = tokens.expect(messages, TokenKind::Word)?;

				if tokens.peek_kind() == Ok(TokenKind::Colon) {
					tokens.next(messages)?;
					let variant_name = tokens.expect(messages, TokenKind::Word)?;
					let binding_name = Some(Node::from_token(first.text, first));
					check_not_reserved(messages, first, "`is` operator binding name")?;
					let variant_name = Node::from_token(variant_name.text, variant_name);
					(binding_name, bump_vec![in bump; variant_name].into_bump_slice())
				} else {
					let mut variant_names = bump_vec![in bump; Node::from_token(first.text, first)];

					while tokens.peek_kind() == Ok(TokenKind::Comma) {
						tokens.next(messages)?;
						let name = tokens.expect(messages, TokenKind::Word)?;
						variant_names.push(Node::from_token(name.text, name));
					}

					(None, variant_names.into_bump_slice())
				}
			};

			let left_span = atom.span;
			let right_span = variant_names.last().unwrap().span;

			let check_is = CheckIs { left: atom, binding_name, variant_names };
			let expression = Expression::CheckIs(bump.alloc(check_is));

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

		TokenKind::DoublePeriod => BinaryOperator::Range,

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
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	allow_struct_literal: bool,
) -> ParseResult<Node<Expression<'a>>> {
	let peeked = tokens.peek()?;

	match peeked.kind {
		TokenKind::Sub => {
			let token = tokens.next(messages)?;
			let op = Node::new(UnaryOperator::Negate, token.span);

			let expression = parse_expression(bump, messages, tokens, allow_struct_literal)?;

			let span = token.span + expression.span;
			let negate = bump.alloc(UnaryOperation { op, expression });

			Ok(Node::new(Expression::UnaryOperation(negate), span))
		}

		TokenKind::String => {
			let string_token = tokens.next(messages)?;
			let value = parse_string_contents(string_token.text);
			Ok(Node::from_token(Expression::StringLiteral(StringLiteral { value }), string_token))
		}

		TokenKind::FormatString => {
			let string_token = tokens.next(messages)?;
			let value = parse_format_string_contents(bump, messages, tokens, string_token);
			Ok(Node::from_token(Expression::FormatStringLiteral(value), string_token))
		}

		TokenKind::Codepoint => {
			let codepoint_token = tokens.next(messages)?;
			let codepoint = parse_codepoint_contents(messages, codepoint_token)?;
			let value = Node::from_token(codepoint, codepoint_token);

			Ok(Node::from_token(
				Expression::CodepointLiteral(CodepointLiteral { value }),
				codepoint_token,
			))
		}

		TokenKind::ByteCodepoint => {
			let codepoint_token = tokens.next(messages)?;
			let codepoint = parse_codepoint_contents(messages, codepoint_token)?;

			// Tokenizer guarentees that this will be a single byte in the success case
			let value = Node::from_token(codepoint as u8, codepoint_token);

			Ok(Node::from_token(
				Expression::ByteCodepointLiteral(ByteCodepointLiteral { value }),
				codepoint_token,
			))
		}

		TokenKind::Number => {
			return parse_number(messages, tokens);
		}

		TokenKind::Word | TokenKind::DoubleColon => {
			match peeked.text {
				"if" => {
					let node = parse_if_else_chain(bump, messages, tokens)?;
					let expression = Expression::IfElseChain(bump.alloc(node.item));
					return Ok(Node::new(expression, node.span));
				}

				"match" => {
					let node = parse_match(bump, messages, tokens)?;
					let expression = Expression::Match(bump.alloc(node.item));
					return Ok(Node::new(expression, node.span));
				}

				"true" => {
					tokens.next(messages)?;
					return Ok(Node::new(Expression::BooleanLiteral(true), peeked.span));
				}

				"false" => {
					tokens.next(messages)?;
					return Ok(Node::new(Expression::BooleanLiteral(false), peeked.span));
				}

				_ => {}
			}

			parse_path_expression(bump, messages, tokens, None, allow_struct_literal)
		}

		TokenKind::OpenParen => {
			tokens.next(messages)?;
			// Regardless of if parent parsing context disallowed struct literals, we override that within parenthesis
			let expression = parse_expression(bump, messages, tokens, true)?;
			tokens.expect(messages, TokenKind::CloseParen)?;
			Ok(expression)
		}

		TokenKind::OpenBracket => {
			let type_start_token = tokens.next(messages)?;
			tokens.expect(messages, TokenKind::CloseBracket)?;

			let parsed_type = if tokens.peek_kind() == Ok(TokenKind::OpenBrace) {
				None
			} else {
				Some(parse_type(bump, messages, tokens)?)
			};

			tokens.expect(messages, TokenKind::OpenBrace)?;
			tokens.consume_newlines();

			let mut expressions = BumpVec::new_in(bump);
			while tokens.peek_kind() != Ok(TokenKind::CloseBrace) {
				expressions.push(parse_expression(bump, messages, tokens, true)?);

				if tokens.peek_kind() == Ok(TokenKind::CloseBrace) {
					break;
				} else if tokens.peek_kind() == Ok(TokenKind::Comma) {
					tokens.next(messages)?;
					tokens.consume_newlines();
				} else {
					tokens.expect(messages, TokenKind::Newline)?;
					tokens.consume_newlines();
				}
			}

			let close_token = tokens.expect(messages, TokenKind::CloseBrace)?;

			let literal = ArrayLiteral { parsed_type, expressions: expressions.into_bump_slice() };
			let expression = Expression::ArrayLiteral(literal);
			let span = type_start_token.span + close_token.span;
			Ok(Node::new(expression, span))
		}

		TokenKind::OpenBrace => {
			let parsed_block = parse_block(bump, messages, tokens, false)?;

			let span = parsed_block.span;
			let block = parsed_block.item;

			Ok(Node::new(Expression::Block(block), span))
		}

		TokenKind::Period => parse_dot_infer(bump, messages, tokens, allow_struct_literal),

		_ => {
			messages.message(
				error!("Unexpected token {:?} while attempting to parse expression atom", peeked.text).span(peeked.span),
			);
			Err(())
		}
	}
}

fn parse_following_period<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	atom: Node<Expression<'a>>,
	allow_struct_literal: bool,
) -> ParseResult<Node<Expression<'a>>> {
	if tokens.peek_kind() == Ok(TokenKind::Exclamation) {
		let exclamation = tokens.next(messages)?;
		let span = atom.span + exclamation.span;

		let op = Node::new(UnaryOperator::Invert, exclamation.span);
		let operation = UnaryOperation { op, expression: atom };
		let expression = Expression::UnaryOperation(bump.alloc(operation));
		return Ok(Node::new(expression, span));
	}

	if tokens.peek_kind() == Ok(TokenKind::Ampersand) {
		let ampersand = tokens.next(messages)?;

		let mut op_span = ampersand.span;
		let mut op = UnaryOperator::AddressOf;

		if tokens.peek_kind() == Ok(TokenKind::Word) {
			let mut_token = tokens.expect_word(messages, "mut")?;
			op_span += mut_token.span;
			op = UnaryOperator::AddressOfMut;
		}

		let span = atom.span + op_span;
		let op = Node::new(op, op_span);
		let operation = UnaryOperation { op, expression: atom };
		let expression = Expression::UnaryOperation(bump.alloc(operation));
		return Ok(Node::new(expression, span));
	}

	if tokens.peek_kind() == Ok(TokenKind::Mul) {
		let asterisk = tokens.next(messages)?;
		let span = atom.span + asterisk.span;

		let op = Node::new(UnaryOperator::Dereference, asterisk.span);
		let operation = UnaryOperation { op, expression: atom };
		let expression = Expression::UnaryOperation(bump.alloc(operation));
		return Ok(Node::new(expression, span));
	}

	if tokens.peek_kind() == Ok(TokenKind::OpenParen) {
		let open_paren = tokens.next(messages)?;
		let parsed_type = parse_type(bump, messages, tokens)?;
		let close_paren = tokens.expect(messages, TokenKind::CloseParen)?;

		let op_span = open_paren.span + close_paren.span;
		let span = atom.span + close_paren.span;

		let op = Node::new(UnaryOperator::Cast { parsed_type }, op_span);
		let operation = UnaryOperation { op, expression: atom };
		let expression = Expression::UnaryOperation(bump.alloc(operation));
		return Ok(Node::new(expression, span));
	}

	parse_path_expression(bump, messages, tokens, Some(atom), allow_struct_literal)
}

fn parse_bracket_index<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	atom: Node<Expression<'a>>,
) -> ParseResult<Node<Expression<'a>>> {
	tokens.expect(messages, TokenKind::OpenBracket)?;
	let index_expression = parse_expression(bump, messages, tokens, true)?;
	let close_token = tokens.expect(messages, TokenKind::CloseBracket)?;

	let span = atom.span + close_token.span;
	let index = UnaryOperator::Index { index_expression };
	let op = Node::new(index, span);
	let operation = UnaryOperation { op, expression: atom };
	let expression = Expression::UnaryOperation(bump.alloc(operation));
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

fn parse_string_contents<'a>(string: &'a str) -> Cow<'a, str> {
	let mut allocated = String::new();

	let mut index = 0;
	let mut last_extend_index = 0;
	while index < string.len() {
		let mut escape = "";
		match string.as_bytes()[index..] {
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

fn parse_format_string_contents<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &Tokens<'a>,
	token: Token<'a>,
) -> FormatStringLiteral<'a> {
	let string = token.text;
	let file_index = token.span.file_index;
	let source = tokens.source();

	let mut items = BumpVec::new_in(bump);
	let mut allocated = String::new();

	let mut index = 0;
	let mut last_extend_index = 0;
	while index < string.len() {
		let mut escape = "";
		match string.as_bytes()[index..] {
			[b'\\', b'n', ..] => escape = "\n",
			[b'\\', b'r', ..] => escape = "\r",
			[b'\\', b't', ..] => escape = "\t",
			[b'\\', b'\\', ..] => escape = "\\",
			[b'\\', b'"', ..] => escape = "\"",
			[b'\\', b'0', ..] => escape = "\0",

			[b'\\', b'{', ..] => escape = "{",

			_ => {}
		}

		if !escape.is_empty() {
			allocated.push_str(&string[last_extend_index..index]);
			last_extend_index = index + 2;
			index += 1;
			allocated.push_str(escape);
		} else if string.as_bytes()[index] == b'{' {
			if (last_extend_index..index).len() > 0 {
				allocated.push_str(&string[last_extend_index..index]);
			}

			if !allocated.is_empty() {
				let cow = Cow::Owned(allocated);
				allocated = String::new();
				items.push(FormatStringItem::Text(cow));
			}

			index += 1;
			let start_index = index;
			let substring_start = token.span.start + 2 + index;

			let mut open_count = 1;
			while index < string.len() {
				let byte = string.as_bytes()[index];

				if byte == b'}' {
					if open_count <= 1 {
						break;
					}
					open_count -= 1
				} else if byte == b'{' {
					open_count += 1;
				}

				index += 1;
			}

			if index >= string.len() {
				let error = error!("Missing closing brace for format string sub-expression");
				let span = Span { start: token.span.end - 1, ..token.span };
				messages.message(error.span(span));
				break;
			} else if string.as_bytes()[index] != b'}' {
				panic!();
			}

			let substring_length = index - start_index;
			let lines_up_to_point = string.as_bytes()[..start_index].iter().filter(|&&byte| byte == b'\n').count() as u32;
			let line_index = token.span.line_index + lines_up_to_point;
			let mut tokenizer = Tokenizer::new_substring(file_index, source, substring_start, substring_length, line_index);
			let mut tokens = tokenizer.tokenize(Vec::new(), messages);

			if let Ok(expression) = parse_expression(bump, messages, &mut tokens, true) {
				items.push(FormatStringItem::Expression(expression));
			}

			last_extend_index = index + 1;
		}

		index += 1;
	}

	if last_extend_index < string.len() {
		allocated.push_str(&string[last_extend_index..]);
	}

	if !allocated.is_empty() {
		let cow = Cow::Owned(allocated);
		items.push(FormatStringItem::Text(cow));
	}

	FormatStringLiteral { items: items.into_bump_slice() }
}

fn parse_when_else_chain<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
) -> ParseResult<Node<WhenElseChain<'a>>> {
	let mut entries = BumpVec::new_in(bump);
	let mut else_body = None;

	let when_token = tokens.expect_word(messages, "when")?;
	let mut span = when_token.span;

	let condition_token = tokens.expect(messages, TokenKind::Word)?;
	let condition = Node::from_token(condition_token.text, condition_token);
	let body = parse_block(bump, messages, tokens, true)?;
	span += body.span;
	entries.push(WhenElseChainEntry { condition, body });

	while let Ok(Token { text: "else", .. }) = tokens.peek_maybe_after_newline() {
		tokens.consume_newlines();
		tokens.next(messages)?;

		if let Ok(Token { text: "when", .. }) = tokens.peek() {
			tokens.next(messages)?;
			let condition_token = tokens.expect(messages, TokenKind::Word)?;
			let condition = Node::from_token(condition_token.text, condition_token);
			let body = parse_block(bump, messages, tokens, true)?;
			span += body.span;
			entries.push(WhenElseChainEntry { condition, body });
		} else {
			let body = parse_block(bump, messages, tokens, true)?;
			span += body.span;
			else_body = Some(body);
			break;
		}
	}

	let value = WhenElseChain { entries: entries.into_bump_slice(), else_body };
	Ok(Node::new(value, span))
}

fn parse_if_else_chain<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
) -> ParseResult<Node<IfElseChain<'a>>> {
	let mut entries = BumpVec::new_in(bump);
	let mut else_body = None;

	let if_token = tokens.expect_word(messages, "if")?;
	let mut span = if_token.span;

	let condition = parse_expression(bump, messages, tokens, false)?;
	let body = parse_block(bump, messages, tokens, true)?;
	span += body.span;
	entries.push(IfElseChainEntry { condition, body });

	while let Ok(Token { text: "else", .. }) = tokens.peek_maybe_after_newline() {
		tokens.consume_newlines();
		tokens.next(messages)?;

		if let Ok(Token { text: "if", .. }) = tokens.peek() {
			tokens.next(messages)?;
			let condition = parse_expression(bump, messages, tokens, false)?;
			let body = parse_block(bump, messages, tokens, true)?;
			span += body.span;
			entries.push(IfElseChainEntry { condition, body });
		} else {
			let body = parse_block(bump, messages, tokens, true)?;
			span += body.span;
			else_body = Some(body);
			break;
		}
	}

	let value = IfElseChain { entries: entries.into_bump_slice(), else_body };
	Ok(Node::new(value, span))
}

fn parse_match<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<Match<'a>>> {
	let match_token = tokens.expect_word(messages, "match")?;

	let expression = parse_expression(bump, messages, tokens, false)?;

	tokens.expect(messages, TokenKind::OpenBrace)?;

	let mut arms = BumpVec::new_in(bump);
	let mut else_arm: Option<ElseArm> = None;
	let mut warned_after_else = false;

	while tokens.peek_kind() != Ok(TokenKind::CloseBrace) {
		tokens.consume_newlines();

		let (binding_name, variant_names) = {
			let first = tokens.expect(messages, TokenKind::Word)?;

			if first.text == "else" {
				let block = parse_block(bump, messages, tokens, true)?;
				tokens.consume_newlines();

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

			if tokens.peek_kind() == Ok(TokenKind::Colon) {
				tokens.next(messages)?;
				let variant_name = tokens.expect(messages, TokenKind::Word)?;
				let binding_name = Some(Node::from_token(first.text, first));
				let variant_name = Node::from_token(variant_name.text, variant_name);
				(binding_name, bump_vec![in bump; variant_name].into_bump_slice())
			} else {
				let mut variant_names = bump_vec![in bump; Node::from_token(first.text, first)];

				while tokens.peek_kind() == Ok(TokenKind::Comma) {
					tokens.next(messages)?;
					let name = tokens.expect(messages, TokenKind::Word)?;
					variant_names.push(Node::from_token(name.text, name));
				}

				(None, variant_names.into_bump_slice())
			}
		};

		let block = parse_block(bump, messages, tokens, true)?;
		tokens.consume_newlines();

		let arm = MatchArm { binding_name, variant_names, block };
		arms.push(arm);
	}

	let close_brace = tokens.expect(messages, TokenKind::CloseBrace)?;
	let span = match_token.span + close_brace.span;

	let value = Match { expression, arms: arms.into_bump_slice(), else_arm };
	Ok(Node::new(value, span))
}

fn parse_while_statement<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<While<'a>>> {
	let while_token = tokens.expect_word(messages, "while")?;

	let condition = parse_expression(bump, messages, tokens, false)?;
	let body = parse_block(bump, messages, tokens, true)?;

	let span = while_token.span + body.span;
	let value = While { condition, body };
	Ok(Node::new(value, span))
}

fn parse_for_statement<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<For<'a>>> {
	let for_token = tokens.expect_word(messages, "for")?;

	let item_token = tokens.expect(messages, TokenKind::Word)?;
	let mut reserved = check_not_reserved(messages, item_token, "`for` loop item name").is_err();
	let item = Node::from_token(item_token.text, item_token);

	let index = if tokens.peek_kind() == Ok(TokenKind::Comma) {
		tokens.next(messages)?;
		let index_token = tokens.expect(messages, TokenKind::Word)?;
		reserved |= check_not_reserved(messages, index_token, "`for` loop index name").is_err();
		Some(Node::from_token(index_token.text, index_token))
	} else {
		None
	};

	let is_last = if tokens.peek_kind() == Ok(TokenKind::Comma) {
		tokens.next(messages)?;
		let is_last_token = tokens.expect(messages, TokenKind::Word)?;
		reserved |= check_not_reserved(messages, is_last_token, "`for` loop last item flag name").is_err();
		Some(Node::from_token(is_last_token.text, is_last_token))
	} else {
		None
	};

	if reserved {
		return Err(());
	}

	// Intentionally peek for `of` first so if we have an unexpected token it falls through to the `in` expect
	// for a better "default" parse error message
	let iteration_kind = if let Ok(Token { text: "of", kind: TokenKind::Word, .. }) = tokens.peek() {
		let of_token = tokens.next(messages)?;
		Node::from_token(IterationKind::Of, of_token)
	} else {
		let in_token = tokens.expect_word(messages, "in")?;
		Node::from_token(IterationKind::In, in_token)
	};

	let initializer = parse_expression(bump, messages, tokens, false)?;

	let body = parse_block(bump, messages, tokens, true)?;

	let span = for_token.span + body.span;
	let value = For { item, index, is_last, iteration_kind, initializer, body };
	Ok(Node::new(value, span))
}

// Holy return type batman
// TODO: Do something about this
fn parse_type_arguments<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
) -> ParseResult<Option<Node<&'a [Node<Type<'a>>]>>> {
	if tokens.peek_kind() != Ok(TokenKind::OpenGeneric) {
		return Ok(None);
	}

	let open_token = tokens.expect(messages, TokenKind::OpenGeneric)?;

	let mut types = BumpVec::new_in(bump);
	while tokens.peek_kind() != Ok(TokenKind::CompGreater) {
		types.push(parse_type(bump, messages, tokens)?);

		if tokens.peek_kind() == Ok(TokenKind::CompGreater) {
			break;
		}

		tokens.expect(messages, TokenKind::Comma)?;
	}

	let close_token = tokens.expect(messages, TokenKind::CompGreater)?;

	let span = open_token.span + close_token.span;
	if types.is_empty() {
		messages.message(error!("Empty type argument list").span(span));
	}

	let arguments = types.into_bump_slice();
	Ok(Some(Node::new(arguments, span)))
}

fn parse_arguments<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
) -> ParseResult<Node<&'a [Node<Expression<'a>>]>> {
	let open_paren_token = tokens.expect(messages, TokenKind::OpenParen)?;
	tokens.consume_newlines();

	let mut expressions = BumpVec::new_in(bump);
	while tokens.peek_kind() != Ok(TokenKind::CloseParen) {
		expressions.push(parse_expression(bump, messages, tokens, true)?);

		if tokens.peek_kind() == Ok(TokenKind::CloseParen) {
			break;
		} else if tokens.peek_kind() == Ok(TokenKind::Comma) {
			tokens.next(messages)?;
			tokens.consume_newlines();
		} else {
			tokens.expect(messages, TokenKind::Newline)?;
			tokens.consume_newlines();
		}
	}

	let close_paren_token = tokens.expect(messages, TokenKind::CloseParen)?;

	let span = open_paren_token.span + close_paren_token.span;
	Ok(Node::new(expressions.into_bump_slice(), span))
}

fn parse_struct_initializer<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
) -> ParseResult<Node<StructInitializer<'a>>> {
	let open_brace_token = tokens.expect(messages, TokenKind::OpenBrace)?;

	let mut field_initializers = BumpVec::new_in(bump);

	while tokens.peek_kind() != Ok(TokenKind::CloseBrace) {
		tokens.consume_newlines();

		let name_token = tokens.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, name_token, "field name")?;
		let name = Node::from_token(name_token.text, name_token);

		let expression = if tokens.peek_kind() == Ok(TokenKind::Colon) {
			tokens.next(messages)?;
			parse_expression(bump, messages, tokens, true)?
		} else {
			let read = Read { name, type_arguments: &[] };
			Node::new(Expression::Read(read), name.span)
		};

		field_initializers.push(FieldInitializer { name, expression });
		if tokens.peek_kind() == Ok(TokenKind::CloseBrace) {
			break;
		}

		if tokens.peek_kind() == Ok(TokenKind::Comma) {
			tokens.next(messages)?;
		} else {
			tokens.expect(messages, TokenKind::Newline)?;
		}
		tokens.consume_newlines();
	}

	let close_brace_token = tokens.expect(messages, TokenKind::CloseBrace)?;

	let initializer = StructInitializer { field_initializers: field_initializers.into_bump_slice() };
	let span = open_brace_token.span + close_brace_token.span;
	Ok(Node::new(initializer, span))
}

fn parse_number<'a>(messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<Expression<'a>>> {
	let number_token = tokens.expect(messages, TokenKind::Number)?;
	let span = number_token.span;

	let Ok(value) = Decimal::from_str_exact(number_token.text) else {
		messages.message(error!("Invalid number literal").span(span));
		return Err(());
	};

	let literal = NumberLiteral { value: Node::new(value, span) };
	let expression = Expression::NumberLiteral(literal);
	Ok(Node::new(expression, span))
}

fn parse_attributes<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Attributes<'a>> {
	fn check_duplicate_attribute<T>(
		messages: &mut Messages,
		attribute: Option<&Node<T>>,
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

	while let Ok(peeked) = tokens.peek() {
		if peeked.kind == TokenKind::Word {
			match peeked.text {
				"generic" => {
					check_duplicate_attribute(messages, attributes.generic_attribute, "generic", peeked.span)?;
					let attribute = parse_generic_attribute(bump, messages, tokens)?;
					attributes.generic_attribute = Some(bump.alloc(attribute));
				}

				"extern" => {
					check_duplicate_attribute(messages, attributes.extern_attribute, "extern", peeked.span)?;
					let attribute = parse_extern_attribute(messages, tokens)?;
					attributes.extern_attribute = Some(bump.alloc(attribute));
				}

				"export" => {
					check_duplicate_attribute(messages, attributes.export_attribute, "export", peeked.span)?;
					let attribute = parse_export_attribute(messages, tokens)?;
					attributes.export_attribute = Some(bump.alloc(attribute));
				}

				"method" => {
					check_duplicate_attribute(messages, attributes.method_attribute, "method", peeked.span)?;
					let attribute = parse_method_attribute(bump, messages, tokens)?;
					attributes.method_attribute = Some(bump.alloc(attribute));
				}

				_ => break,
			}
		} else if peeked.kind == TokenKind::PoundSign {
			tokens.next(messages)?;
			match tokens.peek().map(|t| t.text) {
				Ok("intrinsic") => {
					check_duplicate_attribute(messages, attributes.intrinsic_attribute, "intrinsic", peeked.span)?;
					let attribute = parse_intrinsic_attribute(messages, tokens, peeked.span)?;
					attributes.intrinsic_attribute = Some(bump.alloc(attribute));
				}

				Ok("lang") => {
					check_duplicate_attribute(messages, attributes.lang_attribute, "lang", peeked.span)?;
					let attribute = parse_lang_attribute(messages, tokens, peeked.span)?;
					attributes.lang_attribute = Some(bump.alloc(attribute));
				}

				_ => break,
			}
		} else {
			break;
		}

		tokens.consume_newlines();
	}

	Ok(attributes)
}

fn parse_generic_attribute<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
) -> ParseResult<Node<GenericAttribute<'a>>> {
	let generic_token = tokens.expect_word(messages, "generic")?;

	let mut names = BumpVec::new_in(bump);
	loop {
		let name_token = tokens.expect(messages, TokenKind::Word)?;
		names.push(Node::new(name_token.text, name_token.span));

		if tokens.peek_kind() == Ok(TokenKind::Newline) {
			tokens.next(messages)?;
			break;
		}
		tokens.expect(messages, TokenKind::Comma)?;
	}

	let span = generic_token.span + names.last().as_ref().unwrap().span;
	let generic_atttribute = GenericAttribute { names: names.into_bump_slice() };
	Ok(Node::new(generic_atttribute, span))
}

fn parse_extern_attribute<'a>(messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<ExternAttribute<'a>>> {
	let extern_token = tokens.expect_word(messages, "extern")?;
	let name_token = tokens.expect(messages, TokenKind::String)?;
	tokens.expect(messages, TokenKind::Newline)?;

	let attribute = ExternAttribute { name: name_token.text };
	let span = extern_token.span + name_token.span;
	Ok(Node::new(attribute, span))
}

fn parse_export_attribute<'a>(messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<ExportAttribute<'a>>> {
	let export_token = tokens.expect_word(messages, "export")?;
	let name_token = tokens.expect(messages, TokenKind::String)?;
	tokens.expect(messages, TokenKind::Newline)?;

	let attribute = ExportAttribute { name: name_token.text };
	let span = export_token.span + name_token.span;
	Ok(Node::new(attribute, span))
}

fn parse_method_attribute<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
) -> ParseResult<Node<MethodAttribute<'a>>> {
	let method_token = tokens.expect_word(messages, "method")?;

	let kind = match tokens.peek() {
		Ok(Token { text: "mut", .. }) => {
			tokens.next(messages)?;
			MethodKind::MutableSelf
		}

		Ok(Token { text: "static", .. }) => {
			tokens.next(messages)?;
			MethodKind::Static
		}

		_ => MethodKind::ImmutableSelf,
	};

	let base_type = parse_path_segments(bump, messages, tokens)?;
	tokens.expect(messages, TokenKind::Newline)?;

	let span = method_token.span + base_type.span;
	let attribute = MethodAttribute { base_type, kind };
	Ok(Node::new(attribute, span))
}

fn parse_intrinsic_attribute<'a>(
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	pound_sign_span: Span,
) -> ParseResult<Node<IntrinsicAttribute>> {
	let intrinsic_token = tokens.expect_word(messages, "intrinsic")?;
	tokens.expect(messages, TokenKind::Newline)?;
	let span = pound_sign_span + intrinsic_token.span;
	Ok(Node::new(IntrinsicAttribute, span))
}

fn parse_lang_attribute<'a>(
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	pound_sign_span: Span,
) -> ParseResult<Node<LangAttribute<'a>>> {
	let lang_token = tokens.expect_word(messages, "lang")?;
	let name_token = tokens.expect(messages, TokenKind::String)?;
	tokens.expect(messages, TokenKind::Newline)?;

	let attribute = LangAttribute { name: name_token.text };
	let span = pound_sign_span + lang_token.span + name_token.span;
	Ok(Node::new(attribute, span))
}

fn parse_import_statement<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<Import<'a>>> {
	let import_token = tokens.expect_word(messages, "import")?;

	let mut segments = BumpVec::new_in(bump);
	let mut symbol_names = BumpVec::new_in(bump);
	let mut end;

	loop {
		let token = tokens.expect(messages, TokenKind::Word)?;
		end = token.span;

		let word = Node::from_token(token.text, token);

		let peeked_kind = tokens.peek_kind();
		if peeked_kind != Ok(TokenKind::Period) {
			symbol_names.push(word);
			break;
		}

		segments.push(word);
		tokens.expect(messages, TokenKind::Period)?;
	}

	while tokens.peek_kind() == Ok(TokenKind::Comma) {
		tokens.next(messages)?;

		let token = tokens.expect(messages, TokenKind::Word)?;
		end = token.span;

		let word = Node::from_token(token.text, token);
		symbol_names.push(word);
	}

	tokens.expect(messages, TokenKind::Newline)?;

	let span = import_token.span + end;
	if segments.is_empty() {
		messages.message(error!("Missing import path").span(span));
		return Err(());
	}

	let path_segments = PathSegments { segments: segments.into_bump_slice() };
	let item = Import { path_segments, symbol_names: symbol_names.into_bump_slice() };
	Ok(Node { item, span })
}

fn parse_path_segments<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
) -> ParseResult<Node<PathSegments<'a>>> {
	let mut segments = BumpVec::new_in(bump);
	loop {
		let segment_token = tokens.expect(messages, TokenKind::Word)?;
		segments.push(Node::from_token(segment_token.text, segment_token));

		if tokens.peek_kind() == Ok(TokenKind::Period) {
			tokens.next(messages)?;
		} else {
			break;
		}
	}

	let span = segments.first().unwrap().span + segments.last().unwrap().span;
	Ok(Node::new(PathSegments { segments: segments.into_bump_slice() }, span))
}

fn parse_path_expression<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	previous: Option<Node<Expression<'a>>>,
	allow_struct_literal: bool,
) -> ParseResult<Node<Expression<'a>>> {
	let word_token = tokens.expect(messages, TokenKind::Word)?;
	let name = Node::from_token(word_token.text, word_token);
	let type_arguments_node = parse_type_arguments(bump, messages, tokens)?;
	let name_span = type_arguments_node.map(|a| name.span + a.span).unwrap_or(name.span);
	let type_arguments = type_arguments_node.map(|a| a.item).unwrap_or_default();

	let (is_call, is_struct_literal) = match tokens.peek_kind() {
		Ok(TokenKind::OpenParen) => (true, false),
		Ok(TokenKind::OpenBrace) => (false, true),
		_ => (false, false),
	};

	if is_call {
		let arguments_node = parse_arguments(bump, messages, tokens)?;
		let arguments = arguments_node.item;
		let span = name_span + arguments_node.span;

		let call = Call { base: previous, name, type_arguments, arguments };
		return Ok(Node::new(Expression::Call(bump.alloc(call)), span));
	}

	let expression = if let Some(previous) = previous {
		let span = previous.span + name_span;
		let access = bump.alloc(DotAccess { base: previous, name, type_arguments });
		Node::new(Expression::DotAccess(access), span)
	} else {
		let read = Read { name, type_arguments };
		Node::new(Expression::Read(read), name_span)
	};

	if is_struct_literal && allow_struct_literal {
		let initializer = parse_struct_initializer(bump, messages, tokens)?;
		let span = name_span + initializer.span;

		let literal = bump.alloc(StructLiteral { base: expression, initializer });
		return Ok(Node::new(Expression::StructLiteral(literal), span));
	}

	if tokens.peek_kind() == Ok(TokenKind::Period) {
		tokens.next(messages)?;

		if tokens.peek_kind() != Ok(TokenKind::Word) {
			return parse_following_period(bump, messages, tokens, expression, allow_struct_literal);
		}

		return parse_path_expression(bump, messages, tokens, Some(expression), allow_struct_literal);
	}

	Ok(expression)
}

fn parse_dot_infer<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	allow_struct_literal: bool,
) -> ParseResult<Node<Expression<'a>>> {
	let period_token = tokens.expect(messages, TokenKind::Period)?;

	let word_token = tokens.expect(messages, TokenKind::Word)?;
	let name = Node::from_token(word_token.text, word_token);
	let span = period_token.span + name.span;

	if tokens.peek_kind() == Ok(TokenKind::OpenParen) {
		let arguments_node = parse_arguments(bump, messages, tokens)?;
		let arguments = arguments_node.item;
		let span = span + arguments_node.span;

		let infer = bump.alloc(DotInferCall { name, arguments });
		let expression = Node::new(Expression::DotInferCall(infer), span);
		return Ok(expression);
	}

	let infer = bump.alloc(DotInfer { name });
	let expression = Node::new(Expression::DotInfer(infer), span);

	if tokens.peek_kind() == Ok(TokenKind::OpenBrace) && allow_struct_literal {
		let initializer = parse_struct_initializer(bump, messages, tokens)?;
		let span = span + initializer.span;

		let literal = bump.alloc(StructLiteral { base: expression, initializer });
		return Ok(Node::new(Expression::StructLiteral(literal), span));
	}

	if tokens.peek_kind() == Ok(TokenKind::Period) {
		tokens.next(messages)?;

		if tokens.peek_kind() != Ok(TokenKind::Word) {
			return parse_following_period(bump, messages, tokens, expression, allow_struct_literal);
		}

		return parse_path_expression(bump, messages, tokens, Some(expression), allow_struct_literal);
	}

	Ok(expression)
}

fn parse_type<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<Type<'a>>> {
	let parsed_type = match tokens.peek()? {
		Token { text: "Void", .. } => {
			let token = tokens.next(messages)?;
			Node::from_token(Type::Void, token)
		}

		Token { kind: TokenKind::Mul, .. } => {
			let asterisk = tokens.next(messages)?;

			let mutable = if matches!(tokens.peek(), Ok(Token { kind: TokenKind::Word, text: "mut", .. })) {
				tokens.next(messages)?;
				true
			} else {
				false
			};

			let pointee = bump.alloc(parse_type(bump, messages, tokens)?);
			let span = asterisk.span + pointee.span;
			Node::new(Type::Pointer { pointee, mutable }, span)
		}

		Token { kind: TokenKind::OpenBracket, .. } => {
			let opening = tokens.next(messages)?;
			tokens.expect(messages, TokenKind::CloseBracket)?;

			let mut mutable = false;
			if tokens.peek().map(|t| t.text) == Ok("mut") {
				tokens.next(messages)?;
				mutable = true;
			}

			let pointee = bump.alloc(parse_type(bump, messages, tokens)?);

			let span = opening.span + pointee.span;
			Node::new(Type::Slice { pointee, mutable }, span)
		}

		_ => {
			let path_segments = parse_path_segments(bump, messages, tokens)?;

			let mut type_arguments = BumpVec::new_in(bump);
			let span = match tokens.peek() {
				Ok(Token { kind: TokenKind::OpenGeneric, .. }) => {
					tokens.next(messages)?;

					if tokens.peek_kind() != Ok(TokenKind::CompGreater) {
						loop {
							type_arguments.push(parse_type(bump, messages, tokens)?);
							if tokens.peek_kind() != Ok(TokenKind::Comma) {
								break;
							}
							tokens.expect(messages, TokenKind::Comma)?;
						}
					}

					let close_bracket = tokens.expect(messages, TokenKind::CompGreater)?;
					path_segments.span + close_bracket.span
				}

				_ => path_segments.span,
			};

			let type_arguments = type_arguments.into_bump_slice();
			let path = Type::Path { path_segments, type_arguments };
			Node::new(path, span)
		}
	};

	Ok(parsed_type)
}

fn parse_function_declaration<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	attributes: Attributes<'a>,
) -> ParseResult<Function<'a>> {
	let generics = match attributes.generic_attribute {
		Some(attribute) => attribute.item.names,
		None => &[],
	};
	let extern_attribute = attributes.extern_attribute;
	let export_attribute = attributes.export_attribute;
	let method_attribute = attributes.method_attribute;
	let intrinsic_attribute = attributes.intrinsic_attribute;
	let lang_attribute = attributes.lang_attribute;

	tokens.expect_word(messages, "fn")?;

	let name_token = tokens.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token, "function name")?;
	let name = Node::from_token(name_token.text, name_token);

	let parameters = parse_parameters(bump, messages, tokens)?;

	let parsed_type = if tokens.peek_kind() == Ok(TokenKind::Colon) {
		tokens.next(messages)?;
		Some(parse_type(bump, messages, tokens)?)
	} else {
		None
	};

	let block = if extern_attribute.is_some() || intrinsic_attribute.is_some() {
		None
	} else {
		Some(parse_block(bump, messages, tokens, false)?)
	};

	tokens.expect(messages, TokenKind::Newline)?;

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
	})
}

fn parse_parameters<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Parameters<'a>> {
	tokens.expect(messages, TokenKind::OpenParen)?;
	tokens.consume_newlines();

	let mut parameters = BumpVec::new_in(bump);
	let mut c_vararg = None;

	while tokens.peek_kind() != Ok(TokenKind::CloseParen) {
		let is_mutable = match tokens.peek() {
			Ok(Token { text: "mut", .. }) => {
				tokens.next(messages)?;
				true
			}

			_ => false,
		};

		if tokens.peek_kind() == Ok(TokenKind::TriplePeriod) {
			let triple_period = tokens.next(messages)?;
			c_vararg = Some(triple_period.span);
			break;
		}

		let name_token = tokens.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, name_token, "parameter name")?;
		let name = Node::from_token(name_token.text, name_token);

		tokens.expect(messages, TokenKind::Colon)?;

		let parsed_type = parse_type(bump, messages, tokens)?;

		let span = name_token.span + parsed_type.span;
		let parameter = Parameter { name, parsed_type, is_mutable };
		parameters.push(Node::new(parameter, span));

		if tokens.peek_kind() == Ok(TokenKind::CloseParen) {
			break;
		} else if tokens.peek_kind() == Ok(TokenKind::Comma) {
			tokens.next(messages)?;
			tokens.consume_newlines();
		} else {
			tokens.expect(messages, TokenKind::Newline)?;
			tokens.consume_newlines();
		}
	}

	tokens.expect(messages, TokenKind::CloseParen)?;

	Ok(Parameters {
		parameters: parameters.into_bump_slice(),
		c_varargs: c_vararg,
	})
}

fn parse_struct_declaration<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	attributes: Attributes<'a>,
) -> ParseResult<Struct<'a>> {
	let lang_attribute = attributes.lang_attribute;
	let generics = match attributes.generic_attribute {
		Some(attribute) => attribute.item.names,
		None => &[],
	};

	tokens.expect_word(messages, "struct")?;

	let struct_name_token = tokens.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, struct_name_token, "struct name")?;
	let name = Node::from_token(struct_name_token.text, struct_name_token);

	tokens.expect(messages, TokenKind::OpenBrace)?;
	tokens.consume_newlines();

	let mut fields = BumpVec::new_in(bump);
	while tokens.peek_kind() != Ok(TokenKind::CloseBrace) {
		let field_name_token = tokens.expect(messages, TokenKind::Word)?;
		let field = parse_field(bump, messages, tokens, field_name_token, "struct field")?;
		fields.push(field);

		if tokens.peek_kind() == Ok(TokenKind::CloseBrace) {
			break;
		} else if tokens.peek_kind() == Ok(TokenKind::Comma) {
			tokens.next(messages)?;
			tokens.consume_newlines();
		} else {
			tokens.expect(messages, TokenKind::Newline)?;
			tokens.consume_newlines();
		}
	}

	tokens.expect(messages, TokenKind::CloseBrace)?;
	tokens.expect(messages, TokenKind::Newline)?;

	Ok(Struct {
		lang_attribute,
		generics,
		name,
		fields: fields.into_bump_slice(),
	})
}

fn parse_enum_declaration<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	attributes: Attributes<'a>,
) -> ParseResult<Enum<'a>> {
	let lang_attribute = attributes.lang_attribute;
	let generics = match attributes.generic_attribute {
		Some(attribute) => attribute.item.names,
		None => &[],
	};

	tokens.expect_word(messages, "enum")?;

	let enum_name_token = tokens.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, enum_name_token, "enum name")?;
	let name = Node::from_token(enum_name_token.text, enum_name_token);

	tokens.expect(messages, TokenKind::OpenBrace)?;
	tokens.consume_newlines();

	let mut shared_fields = BumpVec::new_in(bump);
	let mut variants = BumpVec::new_in(bump);

	while tokens.peek_kind() != Ok(TokenKind::CloseBrace) {
		let field_name_token = tokens.expect(messages, TokenKind::Word)?;
		if tokens.peek_kind() == Ok(TokenKind::Colon) {
			if !variants.is_empty() {
				let warning = warning!("Enum shared fields should proceed all variants, consider moving this field up");
				messages.message(warning.span(field_name_token.span));
			}

			let field = parse_field(bump, messages, tokens, field_name_token, "enum shared field")?;
			shared_fields.push(field);
		} else {
			check_not_reserved(messages, field_name_token, "enum variant")?;
			let mut variant_fields = BumpVec::new_in(bump);

			if tokens.peek_kind() == Ok(TokenKind::OpenBrace) {
				tokens.next(messages)?;
				tokens.consume_newlines();

				while tokens.peek_kind() != Ok(TokenKind::CloseBrace) {
					let field_name_token = tokens.expect(messages, TokenKind::Word)?;
					let field = parse_field(bump, messages, tokens, field_name_token, "enum variant field")?;
					variant_fields.push(field);

					if tokens.peek_kind() == Ok(TokenKind::CloseBrace) {
						break;
					} else if tokens.peek_kind() == Ok(TokenKind::Comma) {
						tokens.next(messages)?;
						tokens.consume_newlines();
					} else {
						tokens.expect(messages, TokenKind::Newline)?;
						tokens.consume_newlines();
					}
				}

				tokens.expect(messages, TokenKind::CloseBrace)?;
			} else if tokens.peek_kind() == Ok(TokenKind::OpenParen) {
				tokens.next(messages)?;
				let parsed_type = parse_type(bump, messages, tokens)?;
				tokens.expect(messages, TokenKind::CloseParen)?;

				let name = Node::from_token(field_name_token.text, field_name_token);
				let transparent = TransparentVariant { name, parsed_type };
				let variant = EnumVariant::Transparent(transparent);
				variants.push(variant);

				if tokens.peek_kind() == Ok(TokenKind::CloseBrace) {
					break;
				} else if tokens.peek_kind() == Ok(TokenKind::Comma) {
					tokens.next(messages)?;
					tokens.consume_newlines();
				} else {
					tokens.expect(messages, TokenKind::Newline)?;
					tokens.consume_newlines();
				}

				continue;
			}

			let name = Node::from_token(field_name_token.text, field_name_token);
			let struct_like = StructLikeVariant { name, fields: variant_fields.into_bump_slice() };
			let variant = EnumVariant::StructLike(struct_like);
			variants.push(variant);
		}

		if tokens.peek_kind() == Ok(TokenKind::CloseBrace) {
			break;
		} else if tokens.peek_kind() == Ok(TokenKind::Comma) {
			tokens.next(messages)?;
			tokens.consume_newlines();
		} else {
			tokens.expect(messages, TokenKind::Newline)?;
			tokens.consume_newlines();
		}
	}

	tokens.expect(messages, TokenKind::CloseBrace)?;
	tokens.expect(messages, TokenKind::Newline)?;

	Ok(Enum {
		lang_attribute,
		generics,
		name,
		shared_fields: shared_fields.into_bump_slice(),
		variants: variants.into_bump_slice(),
	})
}

fn parse_field<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	field_name_token: Token<'a>,
	label: &str,
) -> ParseResult<Field<'a>> {
	check_not_reserved(messages, field_name_token, label)?;
	let name = Node::from_token(field_name_token.text, field_name_token);

	tokens.expect(messages, TokenKind::Colon)?;
	let parsed_type = parse_type(bump, messages, tokens)?;

	let mut read_only = false;
	let attribute = match tokens.peek() {
		Ok(Token { text: "readable", .. }) => {
			let token = tokens.next(messages)?;
			Some(Node::from_token(FieldAttribute::Readable, token))
		}

		Ok(Token { text: "internal", .. }) => {
			let token = tokens.next(messages)?;
			if let Ok(Token { text: "readonly", .. }) = tokens.peek() {
				tokens.next(messages)?;
				read_only = true;
			}
			Some(Node::from_token(FieldAttribute::Internal, token))
		}

		_ => {
			if let Ok(Token { text: "readonly", .. }) = tokens.peek() {
				tokens.next(messages)?;
				read_only = true;
			}
			None
		}
	};

	Ok(Field { name, parsed_type, attribute, read_only })
}

fn parse_const_statement<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<Const<'a>>> {
	let const_token = tokens.expect_word(messages, "const")?;

	let name_token = tokens.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token, "const name")?;
	let name = Node::from_token(name_token.text, name_token);

	let parsed_type = if tokens.peek_kind() == Ok(TokenKind::Colon) {
		tokens.next(messages)?;
		Some(bump.alloc(parse_type(bump, messages, tokens)?) as &_)
	} else {
		None
	};

	tokens.expect(messages, TokenKind::Equal)?;

	let expression = parse_expression(bump, messages, tokens, true)?;
	tokens.expect_peek(messages, TokenKind::Newline)?;

	let span = const_token.span + expression.span;
	let item = Const { name, parsed_type, expression };
	Ok(Node { item, span })
}

fn parse_static_statement<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
	attributes: Attributes<'a>,
) -> ParseResult<Node<Static<'a>>> {
	let keyword_token = tokens.expect_word(messages, "static")?;

	let name_token = tokens.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token, "static name")?;
	let name = Node::from_token(name_token.text, name_token);

	tokens.expect(messages, TokenKind::Colon)?;
	let parsed_type = parse_type(bump, messages, tokens)?;

	tokens.expect_peek(messages, TokenKind::Newline)?;

	let span = keyword_token.span + parsed_type.span;
	let item = Static {
		name,
		parsed_type,
		extern_attribute: attributes.extern_attribute,
	};

	Ok(Node { item, span })
}

fn parse_binding_statement<'a>(
	bump: &'a Bump,
	messages: &mut Messages,
	tokens: &mut Tokens<'a>,
) -> ParseResult<Node<Binding<'a>>> {
	let is_mutable;
	let keyword_token = if let Ok(Token { text: "mut", .. }) = tokens.peek() {
		is_mutable = true;
		tokens.next(messages)?
	} else {
		is_mutable = false;
		tokens.expect_word(messages, "let")?
	};

	let name_token = tokens.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token, "binding name")?;
	let name = Node::from_token(name_token.text, name_token);

	let parsed_type = if tokens.peek_kind() == Ok(TokenKind::Colon) {
		tokens.next(messages)?;
		Some(bump.alloc(parse_type(bump, messages, tokens)?) as &_)
	} else {
		None
	};

	tokens.expect(messages, TokenKind::Equal)?;

	let expression = parse_expression(bump, messages, tokens, true)?;

	tokens.expect_peek(messages, TokenKind::Newline)?;

	let span = keyword_token.span + expression.span;
	let item = Binding { name, parsed_type, expression, is_mutable };
	Ok(Node { item, span })
}

fn parse_defer_statement<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<Defer<'a>>> {
	let defer_token = tokens.expect_word(messages, "defer")?;

	let Some(statement) = parse_statement(bump, messages, tokens, Attributes::blank()) else {
		return Err(());
	};

	let span = defer_token.span + statement.span();
	let statement = Defer { statement };
	Ok(Node::new(statement, span))
}

fn parse_break_statement<'a>(messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<Break>> {
	let break_token = tokens.expect_word(messages, "break")?;
	tokens.expect_peek(messages, TokenKind::Newline)?;

	Ok(Node::from_token(Break, break_token))
}

fn parse_continue_statement<'a>(messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<Continue>> {
	let continue_token = tokens.expect_word(messages, "continue")?;
	tokens.expect_peek(messages, TokenKind::Newline)?;

	Ok(Node::from_token(Continue, continue_token))
}

fn parse_yield_statement<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<Yield<'a>>> {
	let return_token = tokens.expect_word(messages, "yield")?;

	let expression = parse_expression(bump, messages, tokens, true)?;

	tokens.expect_peek(messages, TokenKind::Newline)?;

	let span = return_token.span + expression.span;
	let item = Yield { expression };
	Ok(Node { item, span })
}

fn parse_return_statement<'a>(bump: &'a Bump, messages: &mut Messages, tokens: &mut Tokens<'a>) -> ParseResult<Node<Return<'a>>> {
	let return_token = tokens.expect_word(messages, "return")?;

	let expression = match tokens.peek_kind() {
		Ok(TokenKind::Newline) => None,
		_ => Some(parse_expression(bump, messages, tokens, true)?),
	};

	tokens.expect_peek(messages, TokenKind::Newline)?;

	let span = match &expression {
		Some(expression) => return_token.span + expression.span,
		None => return_token.span,
	};

	let item = Return { expression };
	Ok(Node { item, span })
}

pub fn is_word_reserved(word: &str) -> bool {
	matches!(
		word,
		"const"
			| "fn" | "let"
			| "mut" | "return"
			| "struct"
			| "enum" | "import"
			| "generic"
			| "extern"
			| "export"
			| "method"
			| "static"
			| "self" | "if"
			| "else" | "while"
			| "for" | "match"
			| "or" | "and"
			| "is" | "in"
			| "of" | "defer"
			| "break" | "continue"
			| "yield" | "true"
			| "false"
	)
}

fn check_not_reserved(messages: &mut Messages, token: Token, use_as: &str) -> ParseResult<()> {
	if is_word_reserved(token.text) {
		let error = error!("Cannot use reserved word {:?} as {use_as}", token.text);
		messages.message(error.span(token.span));
		Err(())
	} else {
		Ok(())
	}
}

fn consume_error_syntax(messages: &mut Messages, tokens: &mut Tokens) {
	let mut brackets = 0;
	let mut parens = 0;
	let mut braces = 0;

	let mut span = None;

	while let Ok(token) = tokens.peek() {
		span = Some(token.span);
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

		tokens.next(messages).expect("This should never fail");
	}

	//Reached end of file while unbalanced
	if brackets != 0 {
		messages.message(error!("Unbalanced brackets").span_if_some(span));
	}
	if parens != 0 {
		messages.message(error!("Unbalanced parentheses").span_if_some(span));
	}
	if braces != 0 {
		messages.message(error!("Unbalanced braces").span_if_some(span));
	}
}
