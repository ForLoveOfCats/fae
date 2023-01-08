use crate::error::{Messages, ParseResult};
use crate::file::SourceFile;
use crate::span::Span;
use crate::tokenizer::{Token, TokenKind, Tokenizer};
use crate::tree::*;

pub fn parse_file<'a>(messages: &mut Messages, file: &'a SourceFile) -> File<'a> {
	let mut tokenizer = Tokenizer::new(&file.source);
	let block = parse_root_block(messages, &mut tokenizer);

	let module_path = &file.module_path;
	File {
		source_file: file,
		module_path,
		block,
	}
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

	while let Ok(token) = tokenizer.peek() {
		match token {
			Token { kind: TokenKind::Newline, .. } => {
				if tokenizer.next(messages).is_err() {
					return items;
				}
			}

			Token {
				kind: TokenKind::Word,
				text: "using",
				..
			} => {
				if let Ok(statement) = parse_using_statement(messages, tokenizer) {
					items.push(Statement::Using(statement));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token {
				kind: TokenKind::Word,
				text: "const",
				..
			} => {
				if let Ok(statement) = parse_const_statement(messages, tokenizer) {
					items.push(Statement::Const(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token {
				kind: TokenKind::Word,
				text: "let",
				..
			} => {
				if let Ok(statement) = parse_let_statement(messages, tokenizer) {
					items.push(Statement::Let(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token {
				kind: TokenKind::Word,
				text: "mut",
				..
			} => {
				if let Ok(statement) = parse_mut_statement(messages, tokenizer) {
					items.push(Statement::Mut(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token {
				kind: TokenKind::Word,
				text: "fn",
				..
			} => {
				if let Ok(statement) = parse_function_declaration(messages, tokenizer) {
					items.push(Statement::Function(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token {
				kind: TokenKind::Word,
				text: "struct",
				..
			} => {
				if let Ok(statement) = parse_struct_declaration(messages, tokenizer) {
					items.push(Statement::Struct(statement));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token {
				kind: TokenKind::Word,
				text: "return",
				..
			} => {
				if let Ok(statement) = parse_return_statement(messages, tokenizer) {
					items.push(Statement::Return(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token {
				kind: TokenKind::OpenBrace, ..
			} => {
				if let Ok(statement) = parse_block(messages, tokenizer) {
					items.push(Statement::Block(statement));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token {
				kind: TokenKind::CloseBrace, ..
			} => break,

			_ => {
				if let Ok(expression) = parse_expression(messages, tokenizer) {
					items.push(Statement::Expression(expression));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}
		}
	}

	items
}

fn parse_expression<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Expression<'a>>> {
	parse_expression_climb(messages, tokenizer, 0)
}

fn parse_expression_climb<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	min_precedence: u32,
) -> ParseResult<Node<Expression<'a>>> {
	let mut result = parse_expression_atom(messages, tokenizer)?;

	while let Some(op) = token_to_operator(tokenizer.peek()?) {
		let precedence = op.node.precedence();
		if precedence < min_precedence {
			break;
		}

		tokenizer.next(messages).expect("Known peeked token");

		let associativity = op.node.associativity();
		let next_min_precedence = match associativity {
			Associativity::Left => precedence + 1,
			Associativity::Right => precedence,
		};

		let right = parse_expression_climb(messages, tokenizer, next_min_precedence)?;
		let left_span = result.span;
		let right_span = right.span;

		let binary_operation = BinaryOperation { op, right, left: result };
		let boxed = Box::new(binary_operation);
		let expression = Expression::BinaryOperation(boxed);

		result = Node::new(expression, left_span + right_span);
	}

	Ok(result)
}

fn token_to_operator(token: Token) -> Option<Node<BinaryOperator>> {
	let operator = match token.kind {
		TokenKind::Equal => BinaryOperator::Assign,
		TokenKind::Add => BinaryOperator::Add,
		TokenKind::Sub => BinaryOperator::Sub,
		TokenKind::Mul => BinaryOperator::Mul,
		TokenKind::Div => BinaryOperator::Div,
		_ => return None,
	};

	Some(Node::new(operator, token.span))
}

fn parse_expression_atom<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<Expression<'a>>> {
	let peeked = tokenizer.peek()?;

	match peeked.kind {
		TokenKind::Sub => {
			let token = tokenizer.expect(messages, TokenKind::Sub)?;
			let op = Node::new(UnaryOperator::Negate, token.span);

			let expression = parse_expression(messages, tokenizer)?;

			let span = token.span + expression.span;
			let negate = Box::new(UnaryOperation { op, expression });

			Ok(Node::new(Expression::UnaryOperation(negate), span))
		}

		TokenKind::String => {
			let string_token = tokenizer.expect(messages, TokenKind::String)?;
			let value = Node::from_token(string_token.text, string_token);

			Ok(Node::from_token(
				Expression::StringLiteral(StringLiteral { value }),
				string_token,
			))
		}

		TokenKind::Char => {
			let char_token = tokenizer.expect(messages, TokenKind::Char)?;
			let value = Node::from_token(char_token.text.chars().next().unwrap(), char_token);

			Ok(Node::from_token(
				Expression::CharLiteral(CharLiteral { value }),
				char_token,
			))
		}

		TokenKind::Word => {
			if peeked.text.as_bytes()[0].is_ascii_digit() {
				return parse_number(messages, tokenizer);
			}

			let path_segments = parse_path_segments(messages, tokenizer)?;

			let (is_call, is_struct_literal) = match tokenizer.peek() {
				Ok(Token {
					kind: TokenKind::OpenParen, ..
				}) => (true, false),

				Ok(Token {
					kind: TokenKind::OpenBrace, ..
				}) => (false, true),

				_ => (false, false),
			};

			if is_call {
				let arguments = parse_arguments(messages, tokenizer)?;
				let span = path_segments.span + arguments.span;
				let call = Call { path_segments, arguments };

				return Ok(Node::new(Expression::Call(call), span));
			}

			if is_struct_literal {
				let initializer = parse_struct_initializer(messages, tokenizer)?;

				let span = path_segments.span + initializer.span;
				let struct_literal = StructLiteral { path_segments, initializer };

				return Ok(Node::new(Expression::StructLiteral(struct_literal), span));
			}

			let span = path_segments.span;
			let read = Read { path_segments };

			Ok(Node::new(Expression::Read(read), span))
		}

		TokenKind::OpenParen => parse_expression(messages, tokenizer),

		TokenKind::OpenBrace => {
			let parsed_block = parse_block(messages, tokenizer)?;

			let span = parsed_block.span;
			let block = parsed_block.node;

			Ok(Node::new(Expression::Block(block), span))
		}

		_ => {
			messages.error(
				message!(
					"Unexpected token {:?} while attempting to parse expression atom",
					peeked.text
				)
				.span(peeked.span),
			);
			Err(())
		}
	}
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

fn parse_number<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Expression<'a>>> {
	let first_number_token = tokenizer.expect(messages, TokenKind::Word)?;

	let followed_by_period = tokenizer
		.peek()
		.map(|peeked| peeked.kind == TokenKind::Period)
		.unwrap_or(false);

	if followed_by_period {
		tokenizer.expect(messages, TokenKind::Period)?;
		let second_number_token = tokenizer.expect(messages, TokenKind::Word)?;

		let combined_text = &tokenizer.source()[first_number_token.span.start..second_number_token.span.end];

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

		let span = first_number_token.span + second_number_token.span;
		return Ok(Node::new(
			Expression::FloatLiteral(FloatLiteral {
				value: Node::new(value, span),
			}),
			span,
		));
	} else {
		let value = match first_number_token.text.parse::<u64>() {
			Ok(value) => value,
			Err(_) => {
				messages.error(message!("Invalid integer literal").span(first_number_token.span));
				return Err(());
			}
		};

		return Ok(Node::from_token(
			Expression::IntegerLiteral(IntegerLiteral {
				value: Node::from_token(value, first_number_token),
			}),
			first_number_token,
		));
	}
}

fn parse_using_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Using<'a>>> {
	let using_token = tokenizer.expect_word(messages, "using")?;

	let path_segments = parse_path_segments(messages, tokenizer)?;

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = using_token.span + path_segments.span;
	let statement = Using { path_segments };
	Ok(Node { node: statement, span })
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

		if tokenizer.peek()?.kind == TokenKind::DoubleColon {
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

		Token {
			kind: TokenKind::Ampersand, ..
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

		_ => {
			let parsed_path = parse_path_segments(messages, tokenizer)?;
			let segments = parsed_path.node;

			let mut arguments = Vec::new();
			let span = match tokenizer.peek() {
				Ok(Token {
					kind: TokenKind::OpenBracket, ..
				}) => {
					tokenizer.expect(messages, TokenKind::OpenBracket)?;

					if tokenizer.peek()?.kind != TokenKind::CloseBracket {
						loop {
							arguments.push(parse_type(messages, tokenizer)?);
							if tokenizer.peek()?.kind != TokenKind::Comma {
								break;
							}
							tokenizer.expect(messages, TokenKind::Comma)?;
						}
					}

					let close_bracket = tokenizer.expect(messages, TokenKind::CloseBracket)?;
					parsed_path.span + close_bracket.span
				}

				_ => parsed_path.span,
			};

			Node::new(Type::Path { segments, arguments }, span)
		}
	};

	Ok(parsed_type)
}

fn parse_function_declaration<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Function<'a>> {
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

fn parse_struct_declaration<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Struct<'a>> {
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

fn parse_const_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Const<'a>>> {
	let const_token = tokenizer.expect_word(messages, "const")?;

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

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = const_token.span + expression.span;
	let statement = Const {
		name,
		parsed_type,
		expression,
	};
	Ok(Node { node: statement, span })
}

fn parse_let_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Let<'a>>> {
	let let_token = tokenizer.expect_word(messages, "let")?;

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

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = let_token.span + expression.span;
	let statement = Let {
		name,
		parsed_type,
		expression,
	};
	Ok(Node { node: statement, span })
}

fn parse_mut_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Mut<'a>>> {
	let mut_token = tokenizer.expect_word(messages, "mut")?;

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

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = mut_token.span + expression.span;
	let statement = Mut {
		name,
		parsed_type,
		expression,
	};
	Ok(Node { node: statement, span })
}

fn parse_return_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Return<'a>>> {
	let return_token = tokenizer.expect_word(messages, "return")?;

	let expression = parse_expression(messages, tokenizer)?;

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = return_token.span + expression.span;
	let statement = Return { expression };
	Ok(Node { node: statement, span })
}

fn check_not_reserved(messages: &mut Messages, token: Token) -> ParseResult<()> {
	let is_reserved = matches!(
		token.text,
		"const" | "fn" | "let" | "mut" | "return" | "struct" | "using"
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
			.next_optional_messages(&mut None)
			.expect("This should never fail");
	}

	//Reached end of file while unbalanced
	if brackets != 0 {
		messages.error(message!("Unbalanced brackets"));
	}
	if parens != 0 {
		messages.error(message!("Unbalanced parentheses"));
	}
	if braces != 0 {
		messages.error(message!("Unbalanced braces"));
	}
}
