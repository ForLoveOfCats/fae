use super::error::{ParseError, ParseErrorKind, ParserResult};
use super::location::SourceLocation;

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenKind {
	LineComment { following_content: bool },
	DelimitedComment { following_content: bool },

	Word,
	String,
	Char,

	OpenParen,
	CloseParen,

	OpenBrace,
	CloseBrace,

	OpenBracket,
	CloseBracket,

	Add,
	AddEqual,
	Sub,
	SubEqual,
	Mul,
	MulEqual,
	Div,
	DivEqual,

	Equal,
	CompEqual,
	CompNotEqual,

	CompGreater,
	CompGreaterEqual,
	CompLess,
	CompLessEqual,

	Colon,
	Semicolon,
	Period,
	Comma,

	Exclamation,
}

impl std::fmt::Display for TokenKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let text = match self {
			TokenKind::LineComment { .. } => "TokenKind::LineComment",
			TokenKind::DelimitedComment { .. } => "TokenKind::DelimitedComment",

			TokenKind::Word => "TokenKind::Word",
			TokenKind::String => "TokenKind::String",
			TokenKind::Char => "TokenKind::Char",

			TokenKind::OpenParen => "(",
			TokenKind::CloseParen => ")",

			TokenKind::OpenBrace => "{",
			TokenKind::CloseBrace => "}",

			TokenKind::OpenBracket => "[",
			TokenKind::CloseBracket => "]",

			TokenKind::Add => "+",
			TokenKind::AddEqual => "+=",
			TokenKind::Sub => "-",
			TokenKind::SubEqual => "-=",
			TokenKind::Mul => "*",
			TokenKind::MulEqual => "*=",
			TokenKind::Div => "/",
			TokenKind::DivEqual => "/=",

			TokenKind::Equal => "=",
			TokenKind::CompEqual => "==",
			TokenKind::CompNotEqual => "!=",

			TokenKind::CompGreater => ">",
			TokenKind::CompGreaterEqual => ">=",
			TokenKind::CompLess => "<",
			TokenKind::CompLessEqual => "<=",

			TokenKind::Colon => ":",
			TokenKind::Semicolon => ";",
			TokenKind::Period => ".",
			TokenKind::Comma => ",",

			TokenKind::Exclamation => "!",
		};

		write!(f, "{}", text)
	}
}

#[derive(Debug, Copy, Clone)]
pub struct Token<'a> {
	pub text: &'a str,
	pub kind: TokenKind,
	pub location: SourceLocation,
}

impl<'a> std::fmt::Display for Token<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.text)
	}
}

impl<'a> Token<'a> {
	pub fn expect(self, expected: TokenKind) -> ParserResult<Token<'a>> {
		if self.kind == expected {
			Ok(self)
		} else {
			Err(ParseError {
				location: self.location,
				kind: ParseErrorKind::Expected {
					expected: format!("{}", expected),
					found: format!("{}", self),
				},
			})
		}
	}

	pub fn expect_word(self, expected: &str) -> ParserResult<Token<'a>> {
		if self.kind == TokenKind::Word && self.text == expected {
			Ok(self)
		} else {
			Err(ParseError {
				location: self.location,
				kind: ParseErrorKind::Expected {
					expected: expected.to_string(),
					found: format!("{}", self),
				},
			})
		}
	}
}

#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
	source: &'a str,
	byte_index: usize,
}

impl<'a> Tokenizer<'a> {
	pub fn new(source: &'a str) -> Tokenizer<'a> {
		Tokenizer {
			source,
			byte_index: 0,
		}
	}

	pub fn has_next(&self, source: &str) -> bool {
		let mut index = self.byte_index;

		while index < source.len() {
			if !matches!(source.as_bytes()[index], b' ' | b'\t' | b'\n' | b'\r') {
				return true;
			}

			index += 1;
		}

		false
	}

	pub fn peek(&mut self) -> ParserResult<Token<'a>> {
		let byte_index = self.byte_index;
		let peeked = self.next();
		self.byte_index = byte_index;

		peeked
	}

	pub fn next(&mut self) -> ParserResult<Token<'a>> {
		while self.byte_index < self.source.len() {
			if matches!(
				self.source.as_bytes()[self.byte_index],
				b' ' | b'\t' | b'\n'
			) {
				self.byte_index += 1;
			} else {
				break;
			}
		}

		self.verify_not_eof()?;

		let token = match self.source.as_bytes()[self.byte_index..] {
			[b'(', ..] => Ok(self.create_token(
				"(",
				TokenKind::OpenParen,
				self.byte_index,
				self.byte_index + 1,
			)),

			[b')', ..] => Ok(self.create_token(
				")",
				TokenKind::CloseParen,
				self.byte_index,
				self.byte_index + 1,
			)),

			[b'{', ..] => Ok(self.create_token(
				"{",
				TokenKind::OpenBrace,
				self.byte_index,
				self.byte_index + 1,
			)),

			[b'}', ..] => Ok(self.create_token(
				"}",
				TokenKind::CloseBrace,
				self.byte_index,
				self.byte_index + 1,
			)),

			[b'[', ..] => Ok(self.create_token(
				"[",
				TokenKind::OpenBracket,
				self.byte_index,
				self.byte_index + 1,
			)),

			[b']', ..] => Ok(self.create_token(
				"]",
				TokenKind::CloseBracket,
				self.byte_index,
				self.byte_index + 1,
			)),

			[b'+', b'=', ..] => {
				self.byte_index += 1;
				Ok(self.create_token(
					"+=",
					TokenKind::AddEqual,
					self.byte_index - 1,
					self.byte_index + 1,
				))
			}

			[b'+', ..] => {
				Ok(self.create_token("+", TokenKind::Add, self.byte_index, self.byte_index + 1))
			}

			[b'-', b'=', ..] => {
				self.byte_index += 1;
				Ok(self.create_token(
					"-=",
					TokenKind::SubEqual,
					self.byte_index - 1,
					self.byte_index + 1,
				))
			}

			[b'-', ..] => {
				Ok(self.create_token("-", TokenKind::Sub, self.byte_index, self.byte_index + 1))
			}

			[b'*', b'=', ..] => {
				self.byte_index += 1;
				Ok(self.create_token(
					"*=",
					TokenKind::MulEqual,
					self.byte_index - 1,
					self.byte_index + 1,
				))
			}

			[b'*', ..] => {
				Ok(self.create_token("*", TokenKind::Mul, self.byte_index, self.byte_index + 1))
			}

			[b'/', b'/', ..] => {
				let following_content = self.is_following_content();

				let start_index = self.byte_index;
				loop {
					self.byte_index += 1;

					if self.byte_index >= self.source.len()
						|| matches!(self.source.as_bytes()[self.byte_index], b'\n')
					{
						break;
					}
				}

				Ok(self.create_token(
					&self.source[start_index..self.byte_index],
					TokenKind::LineComment { following_content },
					start_index,
					self.byte_index,
				))
			}

			[b'/', b'*', ..] => {
				let following_content = self.is_following_content();

				let start_index = self.byte_index;
				loop {
					self.byte_index += 1;
					self.verify_not_eof()?;

					if matches!(self.source.as_bytes()[self.byte_index..], [b'*', b'/', ..]) {
						self.byte_index += 2;
						break;
					}
				}

				Ok(self.create_token(
					&self.source[start_index..self.byte_index],
					TokenKind::DelimitedComment { following_content },
					start_index,
					self.byte_index,
				))
			}

			[b'/', b'=', ..] => {
				self.byte_index += 1;
				Ok(self.create_token(
					"/=",
					TokenKind::DivEqual,
					self.byte_index - 1,
					self.byte_index + 1,
				))
			}

			[b'/', ..] => {
				Ok(self.create_token("/", TokenKind::Div, self.byte_index, self.byte_index + 1))
			}

			[b'=', b'=', ..] => {
				self.byte_index += 1;
				Ok(self.create_token(
					"==",
					TokenKind::CompEqual,
					self.byte_index - 1,
					self.byte_index + 1,
				))
			}

			[b'=', ..] => {
				Ok(self.create_token("=", TokenKind::Equal, self.byte_index, self.byte_index + 1))
			}

			[b'!', b'=', ..] => {
				self.byte_index += 1;
				Ok(self.create_token(
					"!=",
					TokenKind::CompNotEqual,
					self.byte_index - 1,
					self.byte_index + 1,
				))
			}

			[b'!', ..] => Ok(self.create_token(
				"!",
				TokenKind::Exclamation,
				self.byte_index,
				self.byte_index + 1,
			)),

			[b'>', b'=', ..] => {
				self.byte_index += 1;
				Ok(self.create_token(
					">=",
					TokenKind::CompGreaterEqual,
					self.byte_index - 1,
					self.byte_index + 1,
				))
			}

			[b'>', ..] => Ok(self.create_token(
				">",
				TokenKind::CompGreater,
				self.byte_index,
				self.byte_index + 1,
			)),

			[b'<', b'=', ..] => {
				self.byte_index += 1;
				Ok(self.create_token(
					"<=",
					TokenKind::CompLessEqual,
					self.byte_index - 1,
					self.byte_index + 1,
				))
			}

			[b'<', ..] => Ok(self.create_token(
				"<",
				TokenKind::CompLess,
				self.byte_index,
				self.byte_index + 1,
			)),

			[b':', ..] => {
				Ok(self.create_token(":", TokenKind::Colon, self.byte_index, self.byte_index + 1))
			}

			[b';', ..] => Ok(self.create_token(
				";",
				TokenKind::Semicolon,
				self.byte_index,
				self.byte_index + 1,
			)),

			[b'.', ..] => {
				Ok(self.create_token(".", TokenKind::Period, self.byte_index, self.byte_index + 1))
			}

			[b',', ..] => {
				Ok(self.create_token(",", TokenKind::Comma, self.byte_index, self.byte_index + 1))
			}

			[b'\'', ..] => {
				let start_index = self.byte_index;
				self.advance_by_codepoint()?;
				self.expect_byte(b'\'')?;

				Ok(self.create_token(
					&self.source[start_index..self.byte_index],
					TokenKind::Char,
					start_index,
					self.byte_index,
				))
			}

			[b'\"', ..] => {
				let start_index = self.byte_index;
				loop {
					self.byte_index += 1;
					self.verify_not_eof()?;

					//TODO: Handle escaped double-quote
					if self.source.as_bytes()[self.byte_index] == b'\"' {
						self.byte_index += 1;
						break;
					}
				}

				Ok(self.create_token(
					&self.source[start_index..self.byte_index],
					TokenKind::String,
					start_index,
					self.byte_index,
				))
			}

			_ => {
				let start_index = self.byte_index;
				loop {
					self.byte_index += 1;

					if self.byte_index >= self.source.len()
						|| matches!(
							self.source.as_bytes()[self.byte_index],
							b' ' | b'\t'
								| b'\n' | b'\r' | b'(' | b')' | b'{'
								| b'}' | b'[' | b']' | b'+' | b'-'
								| b'*' | b'/' | b'=' | b'>' | b'<'
								| b':' | b';' | b'.' | b',' | b'\''
								| b'"' | b'!' | b'&' | b'|'
						) {
						break;
					}
				}

				self.byte_index -= 1;

				Ok(self.create_token(
					&self.source[start_index..self.byte_index + 1],
					TokenKind::Word,
					start_index,
					self.byte_index + 1,
				))
			}
		};

		self.byte_index += 1;

		token
	}

	fn advance_by_codepoint(&mut self) -> ParserResult<()> {
		self.verify_not_eof()?;

		let mut chars = self.source[self.byte_index..].chars();
		chars.next();
		self.byte_index = chars.as_str().as_ptr() as usize - self.source.as_ptr() as usize;

		Ok(())
	}

	fn expect_byte(&mut self, expected: u8) -> ParserResult<()> {
		self.verify_not_eof()?;

		self.byte_index += 1;

		let found = self.source.as_bytes()[self.byte_index];
		if found != expected {
			return Err(ParseError {
				location: SourceLocation {
					start: self.byte_index,
					end: self.byte_index + 1,
				},
				kind: ParseErrorKind::Expected {
					expected: format!("{}", expected),
					found: format!("{}", found),
				},
			});
		}

		Ok(())
	}

	fn verify_not_eof(&self) -> ParserResult<()> {
		if self.byte_index >= self.source.len() {
			Err(ParseError {
				location: SourceLocation {
					start: self.source.len().saturating_sub(1),
					end: self.source.len().saturating_sub(1),
				},
				kind: ParseErrorKind::UnexpectedEof,
			})
		} else {
			Ok(())
		}
	}

	fn create_token(
		&mut self,
		text: &'a str,
		kind: TokenKind,
		start: usize,
		end: usize,
	) -> Token<'a> {
		Token {
			text,
			kind,
			location: SourceLocation { start, end },
		}
	}

	fn is_following_content(&self) -> bool {
		let mut index = self.byte_index;
		while index > 0 {
			index -= 1;
			let byte = self.source.as_bytes()[index];

			if matches!(byte, b'\n') {
				return false;
			} else if !matches!(byte, b' ' | b'\t' | b'\r') {
				return true;
			}
		}

		false
	}

	pub fn expect(&mut self, expected: TokenKind) -> ParserResult<Token<'a>> {
		self.next()?.expect(expected)
	}

	pub fn expect_word(&mut self, expected: &str) -> ParserResult<Token<'a>> {
		self.next()?.expect_word(expected)
	}
}
