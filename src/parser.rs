use crate::error::{Messages, ParseResult};
use crate::ice::ice;
use crate::span::Span;
use crate::tokenizer::{Token, TokenKind, Tokenizer};
use crate::tree::*;

pub fn parse_file_root<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> File<'a> {
	let items = parse_items(messages, tokenizer);

	File { items }
}

pub fn parse_items<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> Vec<Item<'a>> {
	let mut items = Vec::new();

	while let Ok(token) = tokenizer.peek() {
		match token {
			Token {
				kind: TokenKind::Newline,
				..
			} => {
				_ = tokenizer.next(messages);
			}

			Token {
				kind: TokenKind::Word,
				text: "using",
				..
			} => {
				if let Ok(statement) = parse_using_statement(messages, tokenizer) {
					items.push(Item::Using(statement));
				} else {
					consume_rest_of_line(tokenizer);
				}
			}

			Token {
				kind: TokenKind::Word,
				text: "const",
				..
			} => {
				if let Ok(statement) = parse_const_statement(messages, tokenizer) {
					items.push(Item::Const(Box::new(statement)));
				} else {
					consume_rest_of_line(tokenizer);
				}
			}

			Token {
				kind: TokenKind::Word,
				text: "fn",
				..
			} => {
				if let Ok(declaration) = parse_function_declaration(messages, tokenizer) {
					items.push(Item::Function(Box::new(declaration)));
				} else {
					consume_rest_of_line(tokenizer);
				}
			}

			Token {
				kind: TokenKind::Word,
				text: "struct",
				..
			} => {
				if let Ok(declaration) = parse_struct_declaration(messages, tokenizer) {
					items.push(Item::Struct(declaration));
				} else {
					consume_rest_of_line(tokenizer);
				}
			}

			token => {
				messages.error(
					message!("Expected start of item but found {:?}", token.text).span(token.span),
				);
				consume_rest_of_line(tokenizer);
			}
		}
	}

	items
}

pub fn parse_block<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<Vec<Statement<'a>>>> {
	let open = tokenizer.expect(messages, TokenKind::OpenBrace)?;
	let block = parse_statements(messages, tokenizer)?;
	let close = tokenizer.expect(messages, TokenKind::CloseBrace)?;

	let span = open.span + close.span;
	Ok(Node::new(block, span))
}

pub fn parse_statements<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Vec<Statement<'a>>> {
	let mut items = Vec::new();

	while tokenizer.has_next() {
		match tokenizer.peek()? {
			Token {
				kind: TokenKind::Newline,
				..
			} => {
				tokenizer.next(messages)?;
			}

			Token {
				kind: TokenKind::Word,
				text: "using",
				..
			} => {
				items.push(Statement::Using(parse_using_statement(
					messages, tokenizer,
				)?));
			}

			Token {
				kind: TokenKind::Word,
				text: "const",
				..
			} => {
				items.push(Statement::Const(Box::new(parse_const_statement(
					messages, tokenizer,
				)?)));
			}

			Token {
				kind: TokenKind::Word,
				text: "let",
				..
			} => {
				items.push(Statement::Let(Box::new(parse_let_statement(
					messages, tokenizer,
				)?)));
			}

			Token {
				kind: TokenKind::Word,
				text: "mut",
				..
			} => {
				items.push(Statement::Mut(Box::new(parse_mut_statement(
					messages, tokenizer,
				)?)));
			}

			Token {
				kind: TokenKind::Word,
				text: "fn",
				..
			} => {
				items.push(Statement::Function(Box::new(parse_function_declaration(
					messages, tokenizer,
				)?)));
			}

			Token {
				kind: TokenKind::Word,
				text: "struct",
				..
			} => {
				items.push(Statement::Struct(parse_struct_declaration(
					messages, tokenizer,
				)?));
			}

			Token {
				kind: TokenKind::Word,
				text: "return",
				..
			} => {
				items.push(Statement::Return(Box::new(parse_return_statement(
					messages, tokenizer,
				)?)));
			}

			Token {
				kind: TokenKind::OpenBrace,
				..
			} => {
				items.push(Statement::Block(parse_block(messages, tokenizer)?.node));
			}

			Token {
				kind: TokenKind::CloseBrace,
				..
			} => break,

			_ => {
				items.push(Statement::Expression(
					parse_expression(messages, tokenizer)?.node,
				));
			}
		}
	}

	Ok(items)
}

//NOTE: This function is a bit gross but not horrible, it is by far the worst part of the parser
fn parse_expression<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<Expression<'a>>> {
	let peeked = tokenizer.peek()?;
	let is_paren_enclosed = peeked.kind == TokenKind::OpenParen;

	if is_paren_enclosed {
		tokenizer.expect(messages, TokenKind::OpenParen)?;
	}

	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	enum ItemKind {
		Expression,
		Operator,
	}

	enum InRpn<'a> {
		Expression(Node<Expression<'a>>),
		Operator(Node<Operator>),
	}

	let mut rpn = Vec::new();
	let mut operators: Vec<Node<Operator>> = Vec::new();
	let mut expected_next = ItemKind::Expression;

	fn check_expected_next(
		messages: &mut Messages,
		token: Token,
		expected: &mut ItemKind,
		actual: ItemKind,
	) -> ParseResult<()> {
		if *expected != actual {
			return match *expected {
				ItemKind::Expression => {
					messages.error(
						message!("Expected expression but found {:?}", token.text).span(token.span),
					);
					Err(())
				}

				ItemKind::Operator => {
					messages.error(
						message!("Expected operator but found {:?}", token.text).span(token.span),
					);
					Err(())
				}
			};
		}

		*expected = match actual {
			ItemKind::Expression => ItemKind::Operator,
			ItemKind::Operator => ItemKind::Expression,
		};

		Ok(())
	}

	while tokenizer.has_next() {
		let peeked = tokenizer.peek()?;

		match peeked.kind {
			TokenKind::Add
			| TokenKind::Sub
			| TokenKind::Mul
			| TokenKind::Div
			| TokenKind::Equal => {
				if peeked.kind == TokenKind::Sub && expected_next == ItemKind::Expression {
					check_expected_next(
						messages,
						peeked,
						&mut expected_next,
						ItemKind::Expression,
					)?;
					rpn.push(InRpn::Expression(parse_number(messages, tokenizer)?));
					continue;
				}

				let operator_token = tokenizer.next(messages)?;
				check_expected_next(
					messages,
					operator_token,
					&mut expected_next,
					ItemKind::Operator,
				)?;

				let operator_kind = match operator_token.kind {
					TokenKind::Equal => Operator::Assign,
					TokenKind::Add => Operator::Add,
					TokenKind::Sub => Operator::Sub,
					TokenKind::Mul => Operator::Mul,
					TokenKind::Div => Operator::Div,
					_ => ice(),
				};
				let precedence = operator_kind.precedence();
				let operator = Node::from_token(operator_kind, operator_token);

				while let Some(in_queue) = operators.pop() {
					let in_queue_precedence = in_queue.node.precedence();
					if precedence <= in_queue_precedence {
						rpn.push(InRpn::Operator(in_queue));
					} else {
						operators.push(in_queue);
						break;
					}
				}

				operators.push(operator);
			}

			TokenKind::String => {
				let string_token = tokenizer.expect(messages, TokenKind::String)?;
				check_expected_next(
					messages,
					string_token,
					&mut expected_next,
					ItemKind::Expression,
				)?;
				let value = Node::from_token(string_token.text, string_token);

				rpn.push(InRpn::Expression(Node::from_token(
					Expression::StringLiteral(StringLiteral { value }),
					string_token,
				)));
			}

			TokenKind::Char => {
				let char_token = tokenizer.expect(messages, TokenKind::Char)?;
				check_expected_next(
					messages,
					char_token,
					&mut expected_next,
					ItemKind::Expression,
				)?;
				let value = Node::from_token(char_token.text.chars().next().unwrap(), char_token);

				rpn.push(InRpn::Expression(Node::from_token(
					Expression::CharLiteral(CharLiteral { value }),
					char_token,
				)));
			}

			TokenKind::Word => {
				check_expected_next(messages, peeked, &mut expected_next, ItemKind::Expression)?;

				if peeked.text.as_bytes()[0].is_ascii_digit() {
					rpn.push(InRpn::Expression(parse_number(messages, tokenizer)?));
					continue;
				}

				let path_segments = parse_path_segments(messages, tokenizer)?;

				let (is_call, is_struct_literal) = match tokenizer.peek() {
					Ok(Token {
						kind: TokenKind::OpenParen,
						..
					}) => (true, false),

					Ok(Token {
						kind: TokenKind::OpenBrace,
						..
					}) => (false, true),

					_ => (false, false),
				};

				if is_call {
					let arguments = parse_arguments(messages, tokenizer)?;
					let span = path_segments.span + arguments.span;
					let call = Call {
						path_segments,
						arguments,
					};

					rpn.push(InRpn::Expression(Node::new(Expression::Call(call), span)));
					continue;
				}

				if is_struct_literal {
					let initializer = parse_struct_initializer(messages, tokenizer)?;

					let span = path_segments.span + initializer.span;
					let struct_literal = StructLiteral {
						path_segments,
						initializer,
					};

					rpn.push(InRpn::Expression(Node::new(
						Expression::StructLiteral(struct_literal),
						span,
					)));
					continue;
				}

				let span = path_segments.span;
				let read = Read { path_segments };

				rpn.push(InRpn::Expression(Node::new(Expression::Read(read), span)));
			}

			TokenKind::OpenParen => {
				check_expected_next(messages, peeked, &mut expected_next, ItemKind::Expression)?;
				rpn.push(InRpn::Expression(parse_expression(messages, tokenizer)?));
			}

			//Yuck, this is messy
			TokenKind::OpenBrace => {
				check_expected_next(messages, peeked, &mut expected_next, ItemKind::Expression)?;

				let parsed_block = parse_block(messages, tokenizer)?;
				let span = parsed_block.span;
				let block = parsed_block.node;

				let expression = Node::new(Expression::Block(block), span);
				rpn.push(InRpn::Expression(expression));
			}

			//NOTE: This is a catch-all to allow callers to handle following tokens
			_ => break,
		}
	}

	if is_paren_enclosed {
		tokenizer.expect(messages, TokenKind::CloseParen)?;
	}

	if expected_next == ItemKind::Expression {
		let token = tokenizer.next(messages)?;
		let error = message!("Missing final expression, found {:?}", token.text).span(token.span);
		messages.error(error);
		return Err(());
	}

	while let Some(in_queue) = operators.pop() {
		rpn.push(InRpn::Operator(in_queue));
	}

	let mut stack = Vec::new();
	for in_rpn in rpn {
		match in_rpn {
			InRpn::Expression(expression) => stack.push(expression),

			InRpn::Operator(op) => {
				let right = stack.pop().unwrap();
				let left = stack.pop().unwrap();

				let left_span = left.span;
				let right_span = right.span;

				let binary_operation = Box::new(BinaryOperation { op, right, left });
				let expression = Expression::BinaryOperation(binary_operation);
				stack.push(Node::new(expression, left_span + right_span));
			}
		}
	}

	assert!(stack.len() == 1);
	Ok(stack.pop().unwrap())
}

fn parse_arguments<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<Vec<Expression<'a>>>> {
	let open_paren_token = tokenizer.expect(messages, TokenKind::OpenParen)?;

	let mut expressions = Vec::new();

	while !reached_close_paren(tokenizer) {
		let expression = parse_expression(messages, tokenizer)?.node;
		expressions.push(expression);

		if reached_close_paren(tokenizer) {
			break;
		}
		tokenizer.expect(messages, TokenKind::Comma)?;
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
	tokenizer.expect(messages, TokenKind::Newline)?;

	let mut field_initializers = Vec::new();

	while tokenizer.peek()?.kind != TokenKind::CloseBrace {
		let name_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, name_token)?;
		let name = Node::from_token(name_token.text, name_token);

		tokenizer.expect(messages, TokenKind::Colon)?;

		let expression = parse_expression(messages, tokenizer)?;

		tokenizer.expect(messages, TokenKind::Comma)?;
		tokenizer.expect(messages, TokenKind::Newline)?;

		field_initializers.push(FieldInitializer { name, expression });
	}

	let close_brace_token = tokenizer.expect(messages, TokenKind::CloseBrace)?;

	Ok(Node::new(
		StructInitializer { field_initializers },
		open_brace_token.span + close_brace_token.span,
	))
}

fn parse_number<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<Expression<'a>>> {
	let is_negative = tokenizer
		.peek()
		.map(|peeked| peeked.kind == TokenKind::Sub)
		.unwrap_or(false);

	if is_negative {
		tokenizer.expect(messages, TokenKind::Sub)?;
	}

	let first_number_token = tokenizer.expect(messages, TokenKind::Word)?;

	let followed_by_period = tokenizer
		.peek()
		.map(|peeked| peeked.kind == TokenKind::Period)
		.unwrap_or(false);

	if followed_by_period {
		tokenizer.expect(messages, TokenKind::Period)?;
		let second_number_token = tokenizer.expect(messages, TokenKind::Word)?;

		let combined_text =
			&tokenizer.source()[first_number_token.span.start..second_number_token.span.end];

		let value = match combined_text.parse::<f64>() {
			Ok(value) => value,
			Err(_) => {
				messages.error(message!("Invalid float literal").span(Span {
					start: first_number_token.span.start,
					end: second_number_token.span.end,
				}));

				return Err(());
			}
		};

		let value = if is_negative { -value } else { value };

		let span = first_number_token.span + second_number_token.span;
		return Ok(Node::new(
			Expression::FloatLiteral(FloatLiteral {
				value: Node::new(value, span),
			}),
			span,
		));
	} else if is_negative {
		let value = match first_number_token.text.parse::<i64>() {
			Ok(value) => value,
			Err(_) => {
				messages.error(
					message!("Invalid signed integer literal").span(first_number_token.span),
				);
				return Err(());
			}
		};

		return Ok(Node::from_token(
			Expression::SignedIntegerLiteral(SignedIntegerLiteral {
				value: Node::from_token(-value, first_number_token),
			}),
			first_number_token,
		));
	} else {
		let value = match first_number_token.text.parse::<u64>() {
			Ok(value) => value,
			Err(_) => {
				messages.error(
					message!("Invalid unsigned integer literal").span(first_number_token.span),
				);
				return Err(());
			}
		};

		return Ok(Node::from_token(
			Expression::UnsignedIntegerLiteral(UnsignedIntegerLiteral {
				value: Node::from_token(value, first_number_token),
			}),
			first_number_token,
		));
	}
}

fn parse_using_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Using<'a>> {
	tokenizer.expect_word(messages, "using")?;

	let path_segments = parse_path_segments(messages, tokenizer)?;

	tokenizer.expect(messages, TokenKind::Newline)?;

	Ok(Using { path_segments })
}

fn parse_path_segments<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<PathSegments<'a>>> {
	let mut segments = Vec::new();

	loop {
		let segment_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, segment_token)?;

		segments.push(Node::from_token(segment_token.text, segment_token));

		if tokenizer.peek()?.kind == TokenKind::Colon {
			tokenizer.expect(messages, TokenKind::Colon)?;
			tokenizer.expect(messages, TokenKind::Colon)?;
		} else {
			break;
		}
	}

	let span = segments.first().unwrap().span + segments.last().unwrap().span;
	Ok(Node::new(PathSegments { segments }, span))
}

fn parse_type<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<Type<'a>>> {
	let parsed_type = match tokenizer.peek()? {
		Token { text: "Void", .. } => {
			let token = tokenizer.expect_word(messages, "Void")?;
			Node::from_token(Type::Void, token)
		}

		Token {
			kind: TokenKind::Ampersand,
			..
		} => {
			let ampersand = tokenizer.expect(messages, TokenKind::Ampersand)?;

			if tokenizer.peek()?.kind == TokenKind::OpenBracket {
				tokenizer.expect(messages, TokenKind::OpenBracket)?;

				let inner = Box::new(parse_type(messages, tokenizer)?);

				let closing = tokenizer.expect(messages, TokenKind::CloseBracket)?;

				let span = ampersand.span + closing.span;
				Node::new(Type::Slice(inner), span)
			} else {
				let inner = Box::new(parse_type(messages, tokenizer)?);
				let span = ampersand.span + inner.span;
				Node::new(Type::Reference(inner), span)
			}
		}

		Token {
			kind: TokenKind::Mul,
			..
		} => {
			let asterisk = tokenizer.expect(messages, TokenKind::Mul)?;
			let inner = Box::new(parse_type(messages, tokenizer)?);
			let span = asterisk.span + inner.span;
			Node::new(Type::Pointer(inner), span)
		}

		_ => {
			let parsed_path = parse_path_segments(messages, tokenizer)?;
			let span = parsed_path.span;
			let path = parsed_path.node;
			Node::new(Type::Path(path), span)
		}
	};

	Ok(parsed_type)
}

fn parse_function_declaration<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Function<'a>> {
	tokenizer.expect_word(messages, "fn")?;

	let name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token)?;
	let name = Node::from_token(name_token.text, name_token);

	let parameters = parse_parameters(messages, tokenizer)?;

	tokenizer.expect(messages, TokenKind::Colon)?;
	let parsed_type = parse_type(messages, tokenizer)?;

	let block = parse_block(messages, tokenizer)?;

	Ok(Function {
		name,
		parameters,
		parsed_type,
		block,
	})
}

fn parse_parameters<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Vec<Node<Parameter<'a>>>> {
	tokenizer.expect(messages, TokenKind::OpenParen)?;

	let mut parameters = Vec::new();

	while !reached_close_paren(tokenizer) {
		let name_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, name_token)?;
		let name = Node::from_token(name_token.text, name_token);

		tokenizer.expect(messages, TokenKind::Colon)?;

		let parsed_type = parse_type(messages, tokenizer)?;

		let span = name_token.span + parsed_type.span;
		parameters.push(Node::new(Parameter { name, parsed_type }, span));

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
) -> ParseResult<Struct<'a>> {
	tokenizer.expect_word(messages, "struct")?;

	let struct_name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, struct_name_token)?;
	let name = Node::from_token(struct_name_token.text, struct_name_token);

	tokenizer.expect(messages, TokenKind::OpenBrace)?;
	tokenizer.expect(messages, TokenKind::Newline)?;

	let mut fields = Vec::new();

	while !reached_close_brace(tokenizer) {
		let field_name_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, field_name_token)?;
		let name = Node::from_token(field_name_token.text, field_name_token);

		tokenizer.expect(messages, TokenKind::Colon)?;

		let parsed_type = parse_type(messages, tokenizer)?;

		fields.push(Field { name, parsed_type });

		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	tokenizer.expect(messages, TokenKind::CloseBrace)?;

	Ok(Struct { name, fields })
}

fn parse_const_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Const<'a>> {
	tokenizer.expect_word(messages, "const")?;

	let name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token)?;
	let name = Node::from_token(name_token.text, name_token);

	let parsed_type = if tokenizer.peek()?.kind == TokenKind::Colon {
		//Parse explicit type
		tokenizer.expect(messages, TokenKind::Colon)?;
		Some(parse_type(messages, tokenizer)?)
	} else {
		None
	};

	tokenizer.expect(messages, TokenKind::Equal)?;
	let expression = parse_expression(messages, tokenizer)?;

	Ok(Const {
		name,
		parsed_type,
		expression,
	})
}

fn parse_let_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Let<'a>> {
	tokenizer.expect_word(messages, "let")?;

	let name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token)?;
	let name = Node::from_token(name_token.text, name_token);

	let parsed_type = if tokenizer.peek()?.kind == TokenKind::Colon {
		tokenizer.expect(messages, TokenKind::Colon)?;
		Some(parse_type(messages, tokenizer)?)
	} else {
		None
	};

	tokenizer.expect(messages, TokenKind::Equal)?;
	let expression = parse_expression(messages, tokenizer)?;

	Ok(Let {
		name,
		parsed_type,
		expression,
	})
}

fn parse_mut_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Mut<'a>> {
	tokenizer.expect_word(messages, "mut")?;

	let name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token)?;
	let name = Node::from_token(name_token.text, name_token);

	let parsed_type = if tokenizer.peek()?.kind == TokenKind::Colon {
		tokenizer.expect(messages, TokenKind::Colon)?;
		Some(parse_type(messages, tokenizer)?)
	} else {
		None
	};

	tokenizer.expect(messages, TokenKind::Equal)?;
	let expression = parse_expression(messages, tokenizer)?;

	Ok(Mut {
		name,
		parsed_type,
		expression,
	})
}

fn parse_return_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Return<'a>> {
	tokenizer.expect_word(messages, "return")?;

	let expression = parse_expression(messages, tokenizer)?;

	Ok(Return { expression })
}

fn check_not_reserved(messages: &mut Messages, token: Token) -> ParseResult<()> {
	let is_reserved = matches!(
		token.text,
		"const" | "fn" | "let" | "module" | "return" | "struct" | "using"
	);

	if is_reserved {
		messages.error(message!("Reserved word {:?}", token.text).span(token.span));
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

fn consume_rest_of_line(tokenizer: &mut Tokenizer) {
	while let Ok(token) = tokenizer.peek() {
		if token.kind == TokenKind::Newline {
			break;
		}

		tokenizer
			.next_optional_messages(&mut None)
			.expect("This should never fail");
	}
}
