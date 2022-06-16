use crate::error::{ParseError, ParseErrorKind, ParseResult};
use crate::ice::ice;
use crate::span::Span;
use crate::tokenizer::{Token, TokenKind, Tokenizer};
use crate::tree::*;

pub fn parse_file_root<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<File<'a>> {
	let module = parse_module_declaration(tokenizer)?;
	let root_expression = parse_block(tokenizer, true)?;

	Ok(File {
		module,
		root_expression,
	})
}

pub fn parse_block<'a>(
	tokenizer: &mut Tokenizer<'a>,
	is_root: bool,
) -> ParseResult<Node<Expression<'a>>> {
	let mut items = Vec::new();

	let start = if is_root {
		if let Ok(peeked) = tokenizer.peek() {
			peeked.span.start
		} else {
			tokenizer.byte_index()
		}
	} else {
		tokenizer.expect(TokenKind::OpenBrace)?.span.start
	};

	while tokenizer.has_next() {
		match tokenizer.peek()? {
			Token {
				kind: TokenKind::Newline,
				..
			} => {
				tokenizer.next()?;
			}

			Token {
				kind: TokenKind::Word,
				text: "using",
				..
			} => {
				items.push(Expression::Using(parse_using_statement(tokenizer)?));
			}

			Token {
				kind: TokenKind::Word,
				text: "const",
				..
			} => {
				items.push(Expression::Const(Box::new(parse_const_statement(
					tokenizer,
				)?)));
			}

			Token {
				kind: TokenKind::Word,
				text: "let",
				..
			} => {
				items.push(Expression::Let(Box::new(parse_let_statement(tokenizer)?)));
			}

			Token {
				kind: TokenKind::Word,
				text: "fn",
				..
			} => {
				if !is_root {
					let token = tokenizer.next()?;
					return Err(ParseError {
						span: token.span,
						kind: ParseErrorKind::SubLevelFunction,
					});
				}

				items.push(Expression::Function(Box::new(parse_function_declaration(
					tokenizer,
				)?)));
			}

			Token {
				kind: TokenKind::Word,
				text: "struct",
				..
			} => {
				if !is_root {
					let token = tokenizer.next()?;
					return Err(ParseError {
						span: token.span,
						kind: ParseErrorKind::SubLevelStruct,
					});
				}

				items.push(Expression::Struct(parse_struct_declaration(tokenizer)?));
			}

			Token {
				kind: TokenKind::Word,
				text: "return",
				..
			} => {
				items.push(Expression::Return(Box::new(parse_return_statement(
					tokenizer,
				)?)));
			}

			Token {
				kind: TokenKind::CloseBrace,
				..
			} => break,

			_ => {
				//HACK: What?
				items.push(parse_expression(tokenizer)?.node);
			}
		}
	}

	let end = if !is_root {
		tokenizer.expect(TokenKind::CloseBrace)?.span.end
	} else {
		tokenizer.byte_index()
	};

	let span = Span { start, end };
	Ok(Node::new(Expression::Block(items), span))
}

//NOTE: This function is a bit gross but not horrible, it is by far the worst part of the parser
fn parse_expression<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Expression<'a>>> {
	let peeked = tokenizer.peek()?;
	let is_block = peeked.kind == TokenKind::OpenBrace;
	let is_paren_enclosed = peeked.kind == TokenKind::OpenParen;

	if is_block {
		return parse_block(tokenizer, false);
	} else if is_paren_enclosed {
		tokenizer.expect(TokenKind::OpenParen)?;
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
		token: Token,
		expected: &mut ItemKind,
		actual: ItemKind,
	) -> ParseResult<()> {
		if *expected != actual {
			return match *expected {
				ItemKind::Expression => Err(ParseError {
					span: token.span,
					kind: ParseErrorKind::ExpectedExpression {
						found: format!("{:?}", token.text),
					},
				}),

				ItemKind::Operator => Err(ParseError {
					span: token.span,
					kind: ParseErrorKind::ExpectedOperator {
						found: format!("{:?}", token.text),
					},
				}),
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
			TokenKind::Add | TokenKind::Sub | TokenKind::Mul | TokenKind::Div => {
				if peeked.kind == TokenKind::Sub && expected_next == ItemKind::Expression {
					check_expected_next(peeked, &mut expected_next, ItemKind::Expression)?;
					rpn.push(InRpn::Expression(parse_number(tokenizer)?));
					continue;
				}

				let operator_token = tokenizer.next()?;
				check_expected_next(operator_token, &mut expected_next, ItemKind::Operator)?;

				let operator_kind = match operator_token.kind {
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
				let string_token = tokenizer.expect(TokenKind::String)?;
				check_expected_next(string_token, &mut expected_next, ItemKind::Expression)?;
				let value = Node::from_token(string_token.text, string_token);

				rpn.push(InRpn::Expression(Node::from_token(
					Expression::StringLiteral(StringLiteral { value }),
					string_token,
				)));
			}

			TokenKind::Char => {
				let char_token = tokenizer.expect(TokenKind::Char)?;
				check_expected_next(char_token, &mut expected_next, ItemKind::Expression)?;
				let value = Node::from_token(char_token.text.chars().next().unwrap(), char_token);

				rpn.push(InRpn::Expression(Node::from_token(
					Expression::CharLiteral(CharLiteral { value }),
					char_token,
				)));
			}

			TokenKind::Word => {
				check_expected_next(peeked, &mut expected_next, ItemKind::Expression)?;

				if peeked.text.as_bytes()[0].is_ascii_digit() {
					rpn.push(InRpn::Expression(parse_number(tokenizer)?));
					continue;
				}

				let path_segments = parse_path_segments(tokenizer)?;

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
					let arguments = parse_arguments(tokenizer)?;
					let span = path_segments.span + arguments.span;
					let call = Call {
						path_segments,
						arguments,
					};

					rpn.push(InRpn::Expression(Node::new(Expression::Call(call), span)));
					continue;
				}

				if is_struct_literal {
					let initializer = parse_struct_initializer(tokenizer)?;

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
				check_expected_next(peeked, &mut expected_next, ItemKind::Expression)?;
				rpn.push(InRpn::Expression(parse_expression(tokenizer)?));
			}

			//NOTE: This is a catch-all to allow callers to handle following tokens
			_ => break,
		}
	}

	if is_paren_enclosed {
		tokenizer.expect(TokenKind::CloseParen)?;
	}

	if expected_next == ItemKind::Expression {
		let token = tokenizer.next()?;
		return Err(ParseError {
			span: token.span,
			kind: ParseErrorKind::ExpectedExpression {
				found: format!("{:?}", token.text),
			},
		});
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

fn parse_arguments<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Vec<Expression<'a>>>> {
	let open_paren_token = tokenizer.expect(TokenKind::OpenParen)?;

	let mut expressions = Vec::new();

	while !reached_close_paren(tokenizer) {
		let expression = parse_expression(tokenizer)?.node;
		expressions.push(expression);

		if reached_close_paren(tokenizer) {
			break;
		}
		tokenizer.expect(TokenKind::Comma)?;
	}

	let close_paren_token = tokenizer.expect(TokenKind::CloseParen)?;

	let span = open_paren_token.span + close_paren_token.span;
	Ok(Node::new(expressions, span))
}

fn parse_struct_initializer<'a>(
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<StructInitializer<'a>>> {
	let open_brace_token = tokenizer.expect(TokenKind::OpenBrace)?;
	tokenizer.expect(TokenKind::Newline)?;

	let mut field_initializers = Vec::new();

	while tokenizer.peek()?.kind != TokenKind::CloseBrace {
		let name_token = tokenizer.expect(TokenKind::Word)?;
		check_not_reserved(name_token)?;
		let name = Node::from_token(name_token.text, name_token);

		tokenizer.expect(TokenKind::Colon)?;

		let expression = parse_expression(tokenizer)?;

		tokenizer.expect(TokenKind::Comma)?;
		tokenizer.expect(TokenKind::Newline)?;

		field_initializers.push(FieldInitializer { name, expression });
	}

	let close_brace_token = tokenizer.expect(TokenKind::CloseBrace)?;

	Ok(Node::new(
		StructInitializer { field_initializers },
		open_brace_token.span + close_brace_token.span,
	))
}

fn parse_number<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Expression<'a>>> {
	let is_negative = tokenizer
		.peek()
		.map(|peeked| peeked.kind == TokenKind::Sub)
		.unwrap_or(false);

	if is_negative {
		tokenizer.expect(TokenKind::Sub)?;
	}

	let first_number_token = tokenizer.expect(TokenKind::Word)?;

	let followed_by_period = tokenizer
		.peek()
		.map(|peeked| peeked.kind == TokenKind::Period)
		.unwrap_or(false);

	if followed_by_period {
		tokenizer.expect(TokenKind::Period)?;
		let second_number_token = tokenizer.expect(TokenKind::Word)?;

		let combined_text =
			&tokenizer.source()[first_number_token.span.start..second_number_token.span.end];

		let value = match combined_text.parse::<f64>() {
			Ok(value) => value,
			Err(_) => {
				return Err(ParseError {
					span: Span {
						start: first_number_token.span.start,
						end: second_number_token.span.end,
					},
					kind: ParseErrorKind::InvalidFloatLiteral,
				});
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
				return Err(ParseError {
					span: first_number_token.span,
					kind: ParseErrorKind::InvalidIntegerLiteral,
				});
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
				return Err(ParseError {
					span: first_number_token.span,
					kind: ParseErrorKind::InvalidIntegerLiteral,
				});
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

fn parse_module_declaration<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Module<'a>>> {
	let module_token = tokenizer.expect_word("module")?;

	let path_segments = parse_path_segments(tokenizer)?;

	let module = Module {
		path_segments: Node::from_token(path_segments.node, module_token),
	};
	let span = module_token.span + path_segments.span;

	Ok(Node::new(module, span))
}

fn parse_using_statement<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Using<'a>> {
	tokenizer.expect_word("using")?;

	let path_segments = parse_path_segments(tokenizer)?;

	tokenizer.expect(TokenKind::Newline)?;

	Ok(Using { path_segments })
}

fn parse_path_segments<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<PathSegments<'a>>> {
	let mut segments = Vec::new();

	loop {
		let segment_token = tokenizer.expect(TokenKind::Word)?;
		check_not_reserved(segment_token)?;

		segments.push(Node::from_token(segment_token.text, segment_token));

		if tokenizer.peek()?.kind == TokenKind::Colon {
			tokenizer.expect(TokenKind::Colon)?;
			tokenizer.expect(TokenKind::Colon)?;
		} else {
			break;
		}
	}

	let span = segments.first().unwrap().span + segments.last().unwrap().span;
	Ok(Node::new(PathSegments { segments }, span))
}

fn parse_function_declaration<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Function<'a>> {
	tokenizer.expect_word("fn")?;

	let name_token = tokenizer.expect(TokenKind::Word)?;
	check_not_reserved(name_token)?;
	let name = Node::from_token(name_token.text, name_token);

	let parameters = parse_parameters(tokenizer)?;

	tokenizer.expect(TokenKind::Colon)?;
	let type_path_segments = parse_path_segments(tokenizer)?;

	let block = parse_block(tokenizer, false)?;

	Ok(Function {
		name,
		parameters,
		type_path_segments,
		block,
	})
}

fn parse_parameters<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Vec<Node<Parameter<'a>>>> {
	tokenizer.expect(TokenKind::OpenParen)?;

	let mut parameters = Vec::new();

	while !reached_close_paren(tokenizer) {
		let name_token = tokenizer.expect(TokenKind::Word)?;
		check_not_reserved(name_token)?;
		let name = Node::from_token(name_token.text, name_token);

		tokenizer.expect(TokenKind::Colon)?;

		let type_path_segments = parse_path_segments(tokenizer)?;

		let span = name_token.span + type_path_segments.span;
		parameters.push(Node::new(
			Parameter {
				name,
				type_path_segments,
			},
			span,
		));

		if reached_close_paren(tokenizer) {
			break;
		}
		tokenizer.expect(TokenKind::Comma)?;
	}

	tokenizer.expect(TokenKind::CloseParen)?;

	Ok(parameters)
}

fn parse_struct_declaration<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Struct<'a>> {
	tokenizer.expect_word("struct")?;

	let struct_name_token = tokenizer.expect(TokenKind::Word)?;
	check_not_reserved(struct_name_token)?;
	let name = Node::from_token(struct_name_token.text, struct_name_token);

	tokenizer.expect(TokenKind::OpenBrace)?;
	tokenizer.expect(TokenKind::Newline)?;

	let mut fields = Vec::new();

	while !reached_close_brace(tokenizer) {
		let field_name_token = tokenizer.expect(TokenKind::Word)?;
		check_not_reserved(field_name_token)?;
		let name = Node::from_token(field_name_token.text, field_name_token);

		tokenizer.expect(TokenKind::Colon)?;

		let type_path_segments = parse_path_segments(tokenizer)?;

		fields.push(Field {
			name,
			type_path_segments,
		});

		tokenizer.expect(TokenKind::Newline)?;
	}

	tokenizer.expect(TokenKind::CloseBrace)?;

	Ok(Struct { name, fields })
}

fn parse_const_statement<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Const<'a>> {
	tokenizer.expect_word("const")?;

	let name_token = tokenizer.expect(TokenKind::Word)?;
	check_not_reserved(name_token)?;
	let name = Node::from_token(name_token.text, name_token);

	let type_path_segments = if tokenizer.peek()?.kind == TokenKind::Colon {
		//Parse explicit type
		tokenizer.expect(TokenKind::Colon)?;
		Some(parse_path_segments(tokenizer)?)
	} else {
		None
	};

	tokenizer.expect(TokenKind::Equal)?;
	let expression = parse_expression(tokenizer)?;

	Ok(Const {
		name,
		type_path_segments,
		expression,
	})
}

fn parse_let_statement<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Let<'a>> {
	tokenizer.expect_word("let")?;

	let name_token = tokenizer.expect(TokenKind::Word)?;
	check_not_reserved(name_token)?;
	let name = Node::from_token(name_token.text, name_token);

	let type_path_segments = if tokenizer.peek()?.kind == TokenKind::Colon {
		//Parse explicit type
		tokenizer.expect(TokenKind::Colon)?;
		Some(parse_path_segments(tokenizer)?)
	} else {
		None
	};

	tokenizer.expect(TokenKind::Equal)?;
	let expression = parse_expression(tokenizer)?;

	Ok(Let {
		name,
		type_path_segments,
		expression,
	})
}

fn parse_return_statement<'a>(tokenizer: &mut Tokenizer<'a>) -> ParseResult<Return<'a>> {
	tokenizer.expect_word("return")?;

	let expression = parse_expression(tokenizer)?;

	Ok(Return { expression })
}

fn check_not_reserved(token: Token) -> ParseResult<()> {
	let is_reserved = matches!(
		token.text,
		"const" | "fn" | "let" | "module" | "return" | "struct" | "using"
	);

	if is_reserved {
		Err(ParseError {
			span: token.span,
			kind: ParseErrorKind::ReservedWord {
				word: token.text.to_owned(),
			},
		})
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
