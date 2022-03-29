use super::error::{ParseError, ParseErrorKind, ParseResult};
use super::location::SourceLocation;

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenKind {
	LineComment { following_content: bool },
	DelimitedComment { following_content: bool },

	Newline,

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
	Period,
	Comma,

	Exclamation,
}

impl std::fmt::Display for TokenKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let text = match self {
			TokenKind::LineComment { .. } => "line comment",
			TokenKind::DelimitedComment { .. } => "delimited comment",

			TokenKind::Newline => "newline",

			TokenKind::Word => "word",
			TokenKind::String => "string literal",
			TokenKind::Char => "character literal",

			TokenKind::OpenParen => "'('",
			TokenKind::CloseParen => "')'",

			TokenKind::OpenBrace => "'{'",
			TokenKind::CloseBrace => "'}'",

			TokenKind::OpenBracket => "'['",
			TokenKind::CloseBracket => "']'",

			TokenKind::Add => "'+'",
			TokenKind::AddEqual => "'+='",
			TokenKind::Sub => "'-'",
			TokenKind::SubEqual => "'-='",
			TokenKind::Mul => "'*'",
			TokenKind::MulEqual => "'*='",
			TokenKind::Div => "'/'",
			TokenKind::DivEqual => "'/='",

			TokenKind::Equal => "'='",
			TokenKind::CompEqual => "'=='",
			TokenKind::CompNotEqual => "'!='",

			TokenKind::CompGreater => "'>'",
			TokenKind::CompGreaterEqual => "'>='",
			TokenKind::CompLess => "'<'",
			TokenKind::CompLessEqual => "'<='",

			TokenKind::Colon => "':'",
			TokenKind::Period => "'.'",
			TokenKind::Comma => "','",

			TokenKind::Exclamation => "'!'",
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

impl<'a> Token<'a> {
	pub fn expect(self, expected: TokenKind) -> ParseResult<Token<'a>> {
		if self.kind == expected {
			Ok(self)
		} else {
			Err(ParseError {
				location: self.location,
				kind: ParseErrorKind::Expected {
					expected: format!("{}", expected),
					found: format!("{:?}", self.text),
				},
			})
		}
	}

	pub fn expect_word(self, expected: &str) -> ParseResult<Token<'a>> {
		if self.kind == TokenKind::Word && self.text == expected {
			Ok(self)
		} else {
			Err(ParseError {
				location: self.location,
				kind: ParseErrorKind::Expected {
					expected: format!("{:?}", expected),
					found: format!("{:?}", self.text),
				},
			})
		}
	}
}

#[derive(Debug, Clone, Copy)]
struct PeekedInfo<'a> {
	token: Token<'a>,
	byte_index: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Tokenizer<'a> {
	source: &'a str,
	byte_index: usize,
	peeked: Option<PeekedInfo<'a>>,
	token_count: usize,
}

impl<'a> Tokenizer<'a> {
	pub fn new(source: &'a str) -> Tokenizer<'a> {
		Tokenizer {
			source,
			byte_index: 0,
			peeked: None,
			token_count: 0,
		}
	}

	pub fn source(&self) -> &str {
		self.source
	}

	//HACK: This exists just so `parse_block` can get an end location when not brace-ed
	pub fn byte_index(&self) -> usize {
		self.byte_index
	}

	pub fn token_count(&self) -> usize {
		self.token_count
	}

	pub fn has_next(&mut self) -> bool {
		self.peek().is_ok()
	}

	pub fn peek(&mut self) -> ParseResult<Token<'a>> {
		if let Some(peeked) = self.peeked {
			return Ok(peeked.token);
		}

		let mut local = *self;
		let peeked = local.next();

		if let Ok(peeked) = peeked {
			self.peeked = Some(PeekedInfo {
				token: peeked,
				byte_index: local.byte_index,
			});
		}

		peeked
	}

	pub fn next(&mut self) -> ParseResult<Token<'a>> {
		if let Some(peeked) = self.peeked.take() {
			self.byte_index = peeked.byte_index;
			self.token_count += 1;
			return Ok(peeked.token);
		}

		loop {
			let token = self.next_with_comments()?;
			match token.kind {
				TokenKind::LineComment { .. } | TokenKind::DelimitedComment { .. } => continue,
				_ => {
					self.token_count += 1;
					return Ok(token);
				}
			}
		}
	}

	pub fn next_with_comments(&mut self) -> ParseResult<Token<'a>> {
		if let Some(newline_token) = self.consume_leading_whitespace()? {
			return Ok(newline_token);
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
					&self.source[start_index + 1..self.byte_index],
					TokenKind::Char,
					start_index,
					self.byte_index + 1,
				))
			}

			[b'\"', ..] => {
				let start_index = self.byte_index;
				loop {
					self.byte_index += 1;
					self.verify_not_eof()?;

					//TODO: Handle escaped double-quote
					if self.source.as_bytes()[self.byte_index] == b'\"' {
						break;
					}
				}

				Ok(self.create_token(
					&self.source[start_index + 1..self.byte_index],
					TokenKind::String,
					start_index,
					self.byte_index + 1,
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

	fn advance_by_codepoint(&mut self) -> ParseResult<()> {
		self.verify_not_eof()?;

		let mut chars = self.source[self.byte_index..].chars();
		chars.next();
		self.byte_index = chars.as_str().as_ptr() as usize - self.source.as_ptr() as usize;

		Ok(())
	}

	fn expect_byte(&mut self, expected: u8) -> ParseResult<()> {
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

	fn consume_leading_whitespace(&mut self) -> ParseResult<Option<Token<'a>>> {
		let at_very_beginning = self.byte_index == 0;
		let mut newline_index = None;

		while self.byte_index < self.source.len() {
			let byte = self.source.as_bytes()[self.byte_index];
			if matches!(byte, b' ' | b'\t' | b'\r') {
				self.byte_index += 1;
			} else if byte == b'\n' {
				if newline_index.is_none() {
					newline_index = Some(self.byte_index);
				}

				self.byte_index += 1;
			} else {
				break;
			}
		}

		if let Some(newline_index) = newline_index {
			if !at_very_beginning {
				return Ok(Some(self.create_token(
					"\n",
					TokenKind::Newline,
					newline_index,
					newline_index + 1,
				)));
			}
		}

		Ok(None)
	}

	fn verify_not_eof(&self) -> ParseResult<()> {
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

	pub fn expect(&mut self, expected: TokenKind) -> ParseResult<Token<'a>> {
		self.next()?.expect(expected)
	}

	pub fn expect_word(&mut self, expected: &str) -> ParseResult<Token<'a>> {
		self.next()?.expect_word(expected)
	}
}
