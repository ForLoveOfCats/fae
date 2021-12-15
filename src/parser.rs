use crate::error::{ParseError, ParseErrorKind, ParseResult};
use crate::location::SourceLocation;
use crate::tokenizer::{Token, TokenKind, Tokenizer};
use crate::tree::{Node, NodeKind, Tree};

pub fn parse_block<'a>(
	tokenizer: &mut Tokenizer<'a>,
	tree: &mut Tree<'a>,
	is_root: bool,
) -> ParseResult<()> {
	if !is_root {
		tokenizer.expect(TokenKind::OpenBrace)?;
	}
	tree.push(Node::without_location(NodeKind::StartExpression));

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
				text: "use",
				..
			} => {
				parse_use_statement(tokenizer, tree)?;
			}

			Token {
				kind: TokenKind::Word,
				text: "const",
				..
			} => {
				parse_const_statement(tokenizer, tree)?;
			}

			Token {
				kind: TokenKind::Word,
				text: "let",
				..
			} => {
				parse_let_statement(tokenizer, tree)?;
			}

			Token {
				kind: TokenKind::Word,
				text: "fn",
				..
			} => {
				parse_function_declaration(tokenizer, tree)?;
			}

			Token {
				kind: TokenKind::Word,
				text: "struct",
				..
			} => {
				parse_struct_declaration(tokenizer, tree)?;
			}

			Token {
				kind: TokenKind::Word,
				text: "return",
				..
			} => {
				parse_return_statement(tokenizer, tree)?;
			}

			Token {
				kind: TokenKind::CloseBrace,
				..
			} => break,

			_ => {
				parse_expression(tokenizer, tree)?;
			}
		}
	}

	if !is_root {
		tokenizer.expect(TokenKind::CloseBrace)?;
	}
	tree.push(Node::without_location(NodeKind::EndExpression));

	Ok(())
}

fn parse_expression<'a>(tokenizer: &mut Tokenizer<'a>, tree: &mut Tree<'a>) -> ParseResult<()> {
	let peeked = tokenizer.peek()?;
	let is_block = peeked.kind == TokenKind::OpenBrace;
	let is_paren_enclosed = peeked.kind == TokenKind::OpenParen;

	if is_block {
		return parse_block(tokenizer, tree, false);
	} else if is_paren_enclosed {
		tokenizer.expect(TokenKind::OpenParen)?;
	}

	tree.push(Node::without_location(NodeKind::StartExpression));

	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	enum ItemKind {
		Expression,
		Operator,
	}

	let mut expected_next = ItemKind::Expression;

	fn check_expected_next(
		token: Token,
		expected: &mut ItemKind,
		actual: ItemKind,
	) -> ParseResult<()> {
		if *expected != actual {
			return match *expected {
				ItemKind::Expression => Err(ParseError {
					location: token.location,
					kind: ParseErrorKind::ExpectedExpression {
						found: format!("{:?}", token.text),
					},
				}),

				ItemKind::Operator => Err(ParseError {
					location: token.location,
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
					parse_number(tokenizer, tree)?;
					continue;
				}

				let operator_token = tokenizer.next()?;
				check_expected_next(operator_token, &mut expected_next, ItemKind::Operator)?;

				let kind = match operator_token.kind {
					TokenKind::Add => NodeKind::Add,
					TokenKind::Sub => NodeKind::Sub,
					TokenKind::Mul => NodeKind::Mul,
					TokenKind::Div => NodeKind::Div,
					_ => unreachable!(),
				};

				tree.push(Node::from_token(kind, operator_token));
			}

			TokenKind::String => {
				let string_token = tokenizer.expect(TokenKind::String)?;
				check_expected_next(string_token, &mut expected_next, ItemKind::Expression)?;

				tree.push(Node::from_token(
					NodeKind::StringLiteral {
						value: string_token.text,
					},
					string_token,
				));
			}

			TokenKind::Char => {
				let char_token = tokenizer.expect(TokenKind::Char)?;
				check_expected_next(char_token, &mut expected_next, ItemKind::Expression)?;

				tree.push(Node::from_token(
					NodeKind::CharLiteral {
						value: char_token.text.chars().next().unwrap(),
					},
					char_token,
				));
			}

			TokenKind::Word => {
				check_expected_next(peeked, &mut expected_next, ItemKind::Expression)?;

				if peeked.text.as_bytes()[0].is_ascii_digit() {
					parse_number(tokenizer, tree)?;
				} else {
					parse_path_segments(tokenizer, tree)?;

					let is_call = tokenizer
						.peek()
						.map(|peeked| peeked.kind == TokenKind::OpenParen)
						.unwrap_or(false);
					if is_call {
						parse_arguments(tokenizer, tree)?;
					}
				}
			}

			TokenKind::OpenParen => {
				check_expected_next(peeked, &mut expected_next, ItemKind::Expression)?;
				parse_expression(tokenizer, tree)?;
			}

			TokenKind::CloseParen | TokenKind::Comma | TokenKind::Newline => break,

			_ => {
				let token = tokenizer.next()?;
				return Err(ParseError {
					location: token.location,
					kind: ParseErrorKind::Unexpected {
						unexpected: token.text.to_owned(),
					},
				});
			}
		}
	}

	tree.push(Node::without_location(NodeKind::EndExpression));

	if is_paren_enclosed {
		tokenizer.expect(TokenKind::CloseParen)?;
	}

	Ok(())
}

fn parse_arguments<'a>(tokenizer: &mut Tokenizer<'a>, tree: &mut Tree<'a>) -> ParseResult<()> {
	tokenizer.expect(TokenKind::OpenParen)?;
	tree.push(Node::without_location(NodeKind::Call));

	while !reached_close_paren(tokenizer) {
		parse_expression(tokenizer, tree)?;

		if reached_close_paren(tokenizer) {
			break;
		}
		tokenizer.expect(TokenKind::Comma)?;
	}

	tokenizer.expect(TokenKind::CloseParen)?;
	tree.push(Node::without_location(NodeKind::EndCallArgs));

	Ok(())
}

fn parse_number<'a>(tokenizer: &mut Tokenizer<'a>, tree: &mut Tree<'a>) -> ParseResult<()> {
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

		let combined_text = &tokenizer.source()
			[first_number_token.location.start..second_number_token.location.end];

		let value = match combined_text.parse::<f64>() {
			Ok(value) => value,
			Err(_) => {
				return Err(ParseError {
					location: SourceLocation {
						start: first_number_token.location.start,
						end: second_number_token.location.end,
					},
					kind: ParseErrorKind::InvalidFloatLiteral,
				});
			}
		};

		let value = if is_negative { -value } else { value };

		tree.push(Node::new(
			NodeKind::FloatLiteral { value },
			first_number_token.location + second_number_token.location,
		));
	} else if is_negative {
		let value = match first_number_token.text.parse::<i64>() {
			Ok(value) => value,
			Err(_) => {
				return Err(ParseError {
					location: first_number_token.location,
					kind: ParseErrorKind::InvalidIntegerLiteral,
				});
			}
		};

		tree.push(Node::from_token(
			NodeKind::SignedIntegerLiteral { value: -value },
			first_number_token,
		));
	} else {
		let value = match first_number_token.text.parse::<u64>() {
			Ok(value) => value,
			Err(_) => {
				return Err(ParseError {
					location: first_number_token.location,
					kind: ParseErrorKind::InvalidIntegerLiteral,
				});
			}
		};

		tree.push(Node::from_token(
			NodeKind::UnsignedIntegerLiteral { value },
			first_number_token,
		));
	}

	Ok(())
}

fn parse_use_statement<'a>(tokenizer: &mut Tokenizer<'a>, tree: &mut Tree<'a>) -> ParseResult<()> {
	let use_token = tokenizer.expect_word("use")?;
	tree.push(Node::from_token(NodeKind::Use, use_token));

	parse_path_segments(tokenizer, tree)?;

	tokenizer.expect(TokenKind::Newline)?;

	Ok(())
}

fn parse_path_segments<'a>(tokenizer: &mut Tokenizer<'a>, tree: &mut Tree<'a>) -> ParseResult<()> {
	loop {
		let segment_token = tokenizer.expect(TokenKind::Word)?;
		tree.push(Node::from_token(
			NodeKind::PathSegment {
				text: segment_token.text,
			},
			segment_token,
		));

		if tokenizer.peek()?.kind == TokenKind::Colon {
			tokenizer.expect(TokenKind::Colon)?;
			tokenizer.expect(TokenKind::Colon)?;
		} else {
			return Ok(());
		}
	}
}

fn parse_function_declaration<'a>(
	tokenizer: &mut Tokenizer<'a>,
	tree: &mut Tree<'a>,
) -> ParseResult<()> {
	tokenizer.expect_word("fn")?;

	let name_token = tokenizer.expect(TokenKind::Word)?;
	tree.push(Node::from_token(
		NodeKind::Function {
			name: name_token.text,
		},
		name_token,
	));

	parse_parameters(tokenizer, tree)?;

	tokenizer.expect(TokenKind::Colon)?;
	parse_path_segments(tokenizer, tree)?;

	parse_block(tokenizer, tree, false)?;

	Ok(())
}

fn parse_parameters<'a>(tokenizer: &mut Tokenizer<'a>, tree: &mut Tree<'a>) -> ParseResult<()> {
	tokenizer.expect(TokenKind::OpenParen)?;

	while !reached_close_paren(tokenizer) {
		let name_token = tokenizer.expect(TokenKind::Word)?;
		tree.push(Node::from_token(
			NodeKind::Parameter {
				name: name_token.text,
			},
			name_token,
		));

		tokenizer.expect(TokenKind::Colon)?;

		parse_path_segments(tokenizer, tree)?;

		if reached_close_paren(tokenizer) {
			break;
		}
		tokenizer.expect(TokenKind::Comma)?;
	}

	tokenizer.expect(TokenKind::CloseParen)?;

	Ok(())
}

fn parse_struct_declaration<'a>(
	tokenizer: &mut Tokenizer<'a>,
	tree: &mut Tree<'a>,
) -> ParseResult<()> {
	tokenizer.expect_word("struct")?;

	let struct_name_token = tokenizer.expect(TokenKind::Word)?;
	tree.push(Node::from_token(
		NodeKind::Struct {
			name: struct_name_token.text,
		},
		struct_name_token,
	));

	tokenizer.expect(TokenKind::OpenBrace)?;
	tokenizer.expect(TokenKind::Newline)?;

	while !reached_close_brace(tokenizer) {
		let field_name_token = tokenizer.expect(TokenKind::Word)?;
		tree.push(Node::from_token(
			NodeKind::Field {
				name: field_name_token.text,
			},
			field_name_token,
		));

		tokenizer.expect(TokenKind::Colon)?;

		parse_path_segments(tokenizer, tree)?;

		tokenizer.expect(TokenKind::Newline)?;
	}

	tokenizer.expect(TokenKind::CloseBrace)?;

	Ok(())
}

fn parse_const_statement<'a>(
	tokenizer: &mut Tokenizer<'a>,
	tree: &mut Tree<'a>,
) -> ParseResult<()> {
	tokenizer.expect_word("const")?;

	let name_token = tokenizer.expect(TokenKind::Word)?;
	tree.push(Node::from_token(
		NodeKind::Const {
			name: name_token.text,
		},
		name_token,
	));

	if tokenizer.peek()?.kind == TokenKind::Colon {
		//Parse explicit type
		tokenizer.expect(TokenKind::Colon)?;
		parse_path_segments(tokenizer, tree)?;
	}

	tokenizer.expect(TokenKind::Equal)?;

	parse_expression(tokenizer, tree)
}

fn parse_let_statement<'a>(tokenizer: &mut Tokenizer<'a>, tree: &mut Tree<'a>) -> ParseResult<()> {
	tokenizer.expect_word("let")?;

	let name_token = tokenizer.expect(TokenKind::Word)?;
	tree.push(Node::from_token(
		NodeKind::Let {
			name: name_token.text,
		},
		name_token,
	));

	if tokenizer.peek()?.kind == TokenKind::Colon {
		//Parse explicit type
		tokenizer.expect(TokenKind::Colon)?;
		parse_path_segments(tokenizer, tree)?;
	}

	tokenizer.expect(TokenKind::Equal)?;

	parse_expression(tokenizer, tree)
}

fn parse_return_statement<'a>(
	tokenizer: &mut Tokenizer<'a>,
	tree: &mut Tree<'a>,
) -> ParseResult<()> {
	let return_token = tokenizer.expect_word("return")?;
	tree.push(Node::from_token(NodeKind::Return, return_token));

	parse_expression(tokenizer, tree)?;

	Ok(())
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
