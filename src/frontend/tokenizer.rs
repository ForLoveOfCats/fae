use crate::frontend::error::{Messages, ParseResult};
use crate::frontend::span::Span;

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenKind {
	Newline,

	Word,
	Number,
	String,
	Codepoint,

	OpenParen,
	CloseParen,

	OpenBrace,
	CloseBrace,

	OpenBracket,
	CloseBracket,

	OpenGeneric,

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
	DoubleColon,
	Period,
	Comma,
	Ampersand,

	Exclamation,
}

impl std::fmt::Display for TokenKind {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let text = match self {
			TokenKind::Newline => "newline",

			TokenKind::Word => "word",
			TokenKind::Number => "number",
			TokenKind::String => "string literal",
			TokenKind::Codepoint => "codepoint literal",

			TokenKind::OpenParen => "'('",
			TokenKind::CloseParen => "')'",

			TokenKind::OpenBrace => "'{'",
			TokenKind::CloseBrace => "'}'",

			TokenKind::OpenBracket => "'['",
			TokenKind::CloseBracket => "']'",

			TokenKind::OpenGeneric => "'<'",

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
			TokenKind::DoubleColon => "'::'",
			TokenKind::Period => "'.'",
			TokenKind::Comma => "','",
			TokenKind::Ampersand => "'&'",

			TokenKind::Exclamation => "'!'",
		};

		write!(f, "{}", text)
	}
}

#[derive(Debug, Copy, Clone)]
pub struct Token<'a> {
	pub text: &'a str,
	pub kind: TokenKind,
	pub span: Span,
}

impl<'a> Token<'a> {
	fn new(text: &'a str, kind: TokenKind, start: usize, end: usize, file_index: usize) -> Self {
		Token { text, kind, span: Span { start, end, file_index } }
	}
}

#[derive(Debug, Clone, Copy)]
struct PeekedInfo<'a> {
	token: Token<'a>,
	byte_index: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Tokenizer<'a> {
	pub file_index: usize,
	source: &'a str,
	bytes: &'a [u8],
	offset: usize,
	peeked: Option<PeekedInfo<'a>>,
	token_count: usize,
}

impl<'a> Tokenizer<'a> {
	pub fn new(file_index: usize, source: &'a str) -> Tokenizer<'a> {
		Tokenizer {
			file_index,
			source,
			bytes: source.as_bytes(),
			offset: 0,
			peeked: None,
			token_count: 0,
		}
	}

	// TODO: Add `peek_word`
	pub fn peek(&mut self) -> ParseResult<Token<'a>> {
		if let Some(peeked) = self.peeked {
			return Ok(peeked.token);
		}

		let mut local = *self;
		let peeked = local.next_with_optional_messages(&mut None);

		if let Ok(peeked) = peeked {
			self.peeked = Some(PeekedInfo { token: peeked, byte_index: local.offset });
		}

		peeked
	}

	pub fn peek_kind(&mut self) -> ParseResult<TokenKind> {
		self.peek().map(|token| token.kind)
	}

	pub fn next(&mut self, messages: &mut Messages) -> ParseResult<Token<'a>> {
		self.next_with_optional_messages(&mut Some(messages))
	}

	pub fn next_with_optional_messages(&mut self, messages: &mut Option<&mut Messages>) -> ParseResult<Token<'a>> {
		use TokenKind::*;

		if let Some(peeked) = self.peeked.take() {
			self.offset = peeked.byte_index;
			self.token_count += 1;
			return Ok(peeked.token);
		}

		let pre_whitespace_offset = self.offset;
		if let Some(newline_token) = self.consume_leading_whitespace()? {
			return Ok(newline_token);
		}

		self.verify_not_eof(messages)?;

		let token = match self.bytes[self.offset..] {
			[b'(', ..] => Ok(Token::new("(", OpenParen, self.offset, self.offset + 1, self.file_index)),

			[b')', ..] => Ok(Token::new(")", CloseParen, self.offset, self.offset + 1, self.file_index)),

			[b'{', ..] => Ok(Token::new("{", OpenBrace, self.offset, self.offset + 1, self.file_index)),

			[b'}', ..] => Ok(Token::new("}", CloseBrace, self.offset, self.offset + 1, self.file_index)),

			[b'[', ..] => Ok(Token::new("[", OpenBracket, self.offset, self.offset + 1, self.file_index)),

			[b']', ..] => Ok(Token::new("]", CloseBracket, self.offset, self.offset + 1, self.file_index)),

			[b'+', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("+=", AddEqual, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'+', ..] => Ok(Token::new("+", Add, self.offset, self.offset + 1, self.file_index)),

			[b'-', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("-=", SubEqual, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'-', ..] => Ok(Token::new("-", Sub, self.offset, self.offset + 1, self.file_index)),

			[b'*', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("*=", MulEqual, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'*', ..] => Ok(Token::new("*", Mul, self.offset, self.offset + 1, self.file_index)),

			[b'/', b'/', ..] => {
				self.offset += 2;

				loop {
					if self.offset >= self.source.len() || self.bytes[self.offset] == b'\n' {
						break;
					}
					self.offset += 1;
				}

				return self.next_with_optional_messages(messages);
			}

			[b'/', b'*', ..] => {
				loop {
					self.offset += 1;
					self.verify_not_eof(messages)?;

					if matches!(self.bytes[self.offset..], [b'*', b'/', ..]) {
						self.offset += 2;
						break;
					}
				}

				return self.next_with_optional_messages(messages);
			}

			[b'/', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("/=", DivEqual, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'/', ..] => Ok(Token::new("/", Div, self.offset, self.offset + 1, self.file_index)),

			[b'=', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("==", CompEqual, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'=', ..] => Ok(Token::new("=", Equal, self.offset, self.offset + 1, self.file_index)),

			[b'!', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("!=", CompNotEqual, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'!', ..] => Ok(Token::new("!", Exclamation, self.offset, self.offset + 1, self.file_index)),

			[b'>', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new(">=", CompGreaterEqual, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'>', ..] => Ok(Token::new(">", CompGreater, self.offset, self.offset + 1, self.file_index)),

			[b'<', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("<=", CompLessEqual, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'<', ..] => {
				if pre_whitespace_offset < self.offset {
					// There is whitespace before this token
					Ok(Token::new("<", CompLess, self.offset, self.offset + 1, self.file_index))
				} else {
					Ok(Token::new("<", OpenGeneric, self.offset, self.offset + 1, self.file_index))
				}
			}

			[b':', b':', ..] => {
				self.offset += 1;
				Ok(Token::new("::", DoubleColon, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b':', ..] => Ok(Token::new(":", Colon, self.offset, self.offset + 1, self.file_index)),

			[b'.', ..] => Ok(Token::new(".", Period, self.offset, self.offset + 1, self.file_index)),

			[b',', ..] => Ok(Token::new(",", Comma, self.offset, self.offset + 1, self.file_index)),

			[b'&', ..] => Ok(Token::new("&", Ampersand, self.offset, self.offset + 1, self.file_index)),

			[b'\'', ..] => {
				let start_index = self.offset;
				self.advance_by_codepoint(messages)?;
				self.expect_byte(messages, b'\'')?;

				Ok(Token::new(
					&self.source[start_index + 1..self.offset],
					Codepoint,
					start_index,
					self.offset + 1,
					self.file_index,
				))
			}

			[b'\"', ..] => {
				let start_index = self.offset;
				loop {
					self.offset += 1;
					self.verify_not_eof(messages)?;

					if matches!(self.bytes[self.offset..], [b'\\', b'\\', ..] | [b'\\', b'"', ..]) {
						self.offset += 1;
						continue;
					}

					if self.bytes[self.offset] == b'\"' {
						break;
					}
				}

				Ok(Token::new(
					&self.source[start_index + 1..self.offset],
					String,
					start_index,
					self.offset + 1,
					self.file_index,
				))
			}

			_ => {
				let start_index = self.offset;
				loop {
					self.offset += 1;

					if self.offset >= self.source.len() {
						break;
					}

					if matches!(
						self.bytes[self.offset],
						b' ' | b'\t'
							| b'\n' | b'\r' | b'(' | b')'
							| b'{' | b'}' | b'[' | b']' | b'+'
							| b'-' | b'*' | b'/' | b'=' | b'>'
							| b'<' | b':' | b';' | b'.' | b','
							| b'\'' | b'"' | b'!' | b'&' | b'|'
					) {
						let is_numeral = self.bytes[start_index].is_ascii_digit();
						let on_period = self.bytes[self.offset] == b'.';
						let has_next = self.offset + 1 < self.source.len();
						if is_numeral && on_period && has_next && self.bytes[self.offset + 1].is_ascii_digit() {
							loop {
								self.offset += 1;
								if self.offset >= self.source.len() || !self.bytes[self.offset].is_ascii_digit() {
									break;
								}
							}

							return Ok(Token::new(
								&self.source[start_index..self.offset],
								Number,
								start_index,
								self.offset,
								self.file_index,
							));
						} else if is_numeral {
							return Ok(Token::new(
								&self.source[start_index..self.offset],
								Number,
								start_index,
								self.offset,
								self.file_index,
							));
						}

						break;
					}
				}

				self.offset -= 1;

				Ok(Token::new(
					&self.source[start_index..self.offset + 1],
					Word,
					start_index,
					self.offset + 1,
					self.file_index,
				))
			}
		};

		self.offset += 1;
		token
	}

	//TODO: Remove this
	fn advance_by_codepoint(&mut self, messages: &mut Option<&mut Messages>) -> ParseResult<()> {
		self.verify_not_eof(messages)?;

		let mut chars = self.source[self.offset..].chars();
		chars.next();
		self.offset = chars.as_str().as_ptr() as usize - self.source.as_ptr() as usize;

		Ok(())
	}

	//TODO: Remove this
	fn expect_byte(&mut self, messages: &mut Option<&mut Messages>, expected: u8) -> ParseResult<()> {
		self.verify_not_eof(messages)?;

		self.offset += 1;

		let found = self.bytes[self.offset];
		if found != expected {
			if let Some(messages) = messages {
				let err = error!("Expected {:?} but found {:?}", expected as char, found as char);
				messages.message(err.span(Span {
					start: self.offset,
					end: self.offset + 1,
					file_index: self.file_index,
				}));
			}

			return Err(());
		}

		Ok(())
	}

	fn consume_leading_whitespace(&mut self) -> ParseResult<Option<Token<'a>>> {
		while self.offset < self.source.len() {
			let byte = self.bytes[self.offset];

			if byte == b'\n' {
				let index = self.offset;
				self.offset += 1;

				return Ok(Some(Token::new("\n", TokenKind::Newline, index, index + 1, self.file_index)));
			} else if matches!(byte, b' ' | b'\t' | b'\r') {
				self.offset += 1;
			} else {
				break;
			}
		}

		Ok(None)
	}

	fn verify_not_eof(&self, messages: &mut Option<&mut Messages>) -> ParseResult<()> {
		if self.offset >= self.source.len() {
			if let Some(messages) = messages {
				messages.message(error!("Unexpected end of file").span(Span {
					start: self.source.len().saturating_sub(1),
					end: self.source.len().saturating_sub(1),
					file_index: self.file_index,
				}));
			}
			Err(())
		} else {
			Ok(())
		}
	}

	pub fn expect(&mut self, messages: &mut Messages, expected: TokenKind) -> ParseResult<Token<'a>> {
		let token = self.next(messages)?;
		if token.kind == expected {
			return Ok(token);
		}

		let message = error!("Expected {expected} but found {:?}", token.text);
		messages.message(message.span(token.span));
		Err(())
	}

	pub fn expect_word(&mut self, messages: &mut Messages, expected: &str) -> ParseResult<Token<'a>> {
		let token = self.expect(messages, TokenKind::Word)?;
		if token.text == expected {
			return Ok(token);
		}

		let message = error!("Expected word {expected:?} but found word {:?}", token.text);
		messages.message(message.span(token.span));
		Err(())
	}
}
