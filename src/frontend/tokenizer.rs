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
	ByteCodepoint,

	OpenParen,
	CloseParen,

	OpenBrace,
	CloseBrace,

	OpenBracket,
	CloseBracket,

	OpenGeneric,

	Add,
	AddAssign,
	Sub,
	SubAssign,
	Mul,
	MulAssign,
	Div,
	DivAssign,
	Modulo,
	ModuloAssign,

	BitshiftLeft,
	BitshiftLeftAssign,
	BitshiftRight,
	BitshiftRightAssign,

	Equal,
	CompEqual,
	CompNotEqual,

	CompGreater,
	CompGreaterEqual,
	CompLess,
	CompLessEqual,

	Ampersand,
	AmpersandAssign,
	Pipe,
	PipeAssign,
	Caret,
	CaretAssign,

	Colon,
	DoubleColon,
	Period,
	Comma,
	PoundSign,
	FatArrow,

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
			TokenKind::ByteCodepoint => "byte codepoint literal",

			TokenKind::OpenParen => "'('",
			TokenKind::CloseParen => "')'",

			TokenKind::OpenBrace => "'{'",
			TokenKind::CloseBrace => "'}'",

			TokenKind::OpenBracket => "'['",
			TokenKind::CloseBracket => "']'",

			TokenKind::OpenGeneric => "'<'",

			TokenKind::Add => "'+'",
			TokenKind::AddAssign => "'+='",
			TokenKind::Sub => "'-'",
			TokenKind::SubAssign => "'-='",
			TokenKind::Mul => "'*'",
			TokenKind::MulAssign => "'*='",
			TokenKind::Div => "'/'",
			TokenKind::DivAssign => "'/='",
			TokenKind::Modulo => "'%'",
			TokenKind::ModuloAssign => "'%='",

			TokenKind::BitshiftLeft => "'<<'",
			TokenKind::BitshiftLeftAssign => "'<<='",
			TokenKind::BitshiftRight => "'>>'",
			TokenKind::BitshiftRightAssign => "'>>=='",

			TokenKind::Equal => "'='",
			TokenKind::CompEqual => "'=='",
			TokenKind::CompNotEqual => "'!='",

			TokenKind::CompGreater => "'>'",
			TokenKind::CompGreaterEqual => "'>='",
			TokenKind::CompLess => "'<'",
			TokenKind::CompLessEqual => "'<='",

			TokenKind::Ampersand => "'&'",
			TokenKind::AmpersandAssign => "'&='",
			TokenKind::Pipe => "'|'",
			TokenKind::PipeAssign => "'|='",
			TokenKind::Caret => "'^'",
			TokenKind::CaretAssign => "'^='",

			TokenKind::Colon => "':'",
			TokenKind::DoubleColon => "'::'",
			TokenKind::Period => "'.'",
			TokenKind::Comma => "','",
			TokenKind::PoundSign => "'#'",
			TokenKind::FatArrow => "'=>'",

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

pub struct Tokens<'a> {
	index: usize,
	tokens: Vec<Token<'a>>,
}

impl<'a> Tokens<'a> {
	pub fn tear_down(self) -> Vec<Token<'a>> {
		self.tokens
	}

	#[inline]
	pub fn peek(&mut self) -> ParseResult<Token<'a>> {
		if self.index >= self.tokens.len() {
			return Err(());
		}

		let index = self.index;
		Ok(self.tokens[index])
	}

	#[inline]
	pub fn peek_kind(&mut self) -> ParseResult<TokenKind> {
		self.peek().map(|token| token.kind)
	}

	#[inline]
	pub fn previous_kind(&mut self) -> Option<TokenKind> {
		if self.index == 0 || self.index - 1 >= self.tokens.len() {
			return None;
		}

		Some(self.tokens[self.index - 1].kind)
	}

	#[inline]
	pub fn next(&mut self) -> ParseResult<Token<'a>> {
		if self.index >= self.tokens.len() {
			return Err(());
		}

		let index = self.index;
		self.index += 1;
		Ok(self.tokens[index])
	}

	#[inline]
	pub fn consume_newlines(&mut self) {
		while self.peek_kind() == Ok(TokenKind::Newline) {
			self.index += 1;
		}
	}

	#[inline]
	pub fn expect(&mut self, messages: &mut Messages, expected: TokenKind) -> ParseResult<Token<'a>> {
		let token = self.next()?;
		if token.kind == expected {
			return Ok(token);
		}

		let message = error!("Expected {expected} but found {:?}", token.text);
		messages.message(message.span(token.span));
		Err(())
	}

	#[inline]
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

#[derive(Debug, Clone, Copy)]
pub struct Tokenizer<'a> {
	pub file_index: usize,
	source: &'a str,
	bytes: &'a [u8],
	offset: usize,
}

impl<'a> Tokenizer<'a> {
	pub fn new(file_index: usize, source: &'a str) -> Tokenizer<'a> {
		Tokenizer { file_index, source, bytes: source.as_bytes(), offset: 0 }
	}

	pub fn tokenize(&mut self, mut tokens: Vec<Token<'a>>, messages: &mut Messages) -> Tokens<'a> {
		tokens.clear();

		while let Ok(token) = self.next(messages) {
			tokens.push(token);
		}

		Tokens { index: 0, tokens }
	}

	// Do not call directly, will not update previous token
	#[inline]
	fn next(&mut self, messages: &mut Messages) -> ParseResult<Token<'a>> {
		use TokenKind::*;

		let pre_whitespace_offset = self.offset;
		if let Some(newline_token) = self.consume_leading_whitespace()? {
			return Ok(newline_token);
		}

		if self.offset >= self.source.len() {
			return Err(());
		}

		let token = match self.bytes[self.offset..] {
			[b'(', ..] => Ok(Token::new("(", OpenParen, self.offset, self.offset + 1, self.file_index)),

			[b')', ..] => Ok(Token::new(")", CloseParen, self.offset, self.offset + 1, self.file_index)),

			[b'{', ..] => Ok(Token::new("{", OpenBrace, self.offset, self.offset + 1, self.file_index)),

			[b'}', ..] => Ok(Token::new("}", CloseBrace, self.offset, self.offset + 1, self.file_index)),

			[b'[', ..] => Ok(Token::new("[", OpenBracket, self.offset, self.offset + 1, self.file_index)),

			[b']', ..] => Ok(Token::new("]", CloseBracket, self.offset, self.offset + 1, self.file_index)),

			[b'+', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("+=", AddAssign, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'+', ..] => Ok(Token::new("+", Add, self.offset, self.offset + 1, self.file_index)),

			[b'-', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("-=", SubAssign, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'-', ..] => Ok(Token::new("-", Sub, self.offset, self.offset + 1, self.file_index)),

			[b'*', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("*=", MulAssign, self.offset - 1, self.offset + 1, self.file_index))
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

				return self.next(messages);
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

				return self.next(messages);
			}

			[b'/', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("/=", DivAssign, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'/', ..] => Ok(Token::new("/", Div, self.offset, self.offset + 1, self.file_index)),

			[b'%', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("%=", ModuloAssign, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'%', ..] => Ok(Token::new("%", Modulo, self.offset, self.offset + 1, self.file_index)),

			[b'<', b'<', b'=', ..] => {
				self.offset += 2;
				Ok(Token::new("<<=", BitshiftLeftAssign, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'<', b'<', ..] => {
				self.offset += 1;
				Ok(Token::new("<<", BitshiftLeft, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'>', b'>', b'=', ..] => {
				self.offset += 2;
				Ok(Token::new(">>=", BitshiftRightAssign, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'>', b'>', ..] if pre_whitespace_offset < self.offset => {
				self.offset += 1;
				Ok(Token::new(">>", BitshiftRight, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'=', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("==", CompEqual, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'=', b'>', ..] => {
				self.offset += 1;
				Ok(Token::new("=>", FatArrow, self.offset - 1, self.offset + 1, self.file_index))
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

			[b'&', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("&=", AmpersandAssign, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'&', ..] => Ok(Token::new("&", Ampersand, self.offset, self.offset + 1, self.file_index)),

			[b'|', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("|=", PipeAssign, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'|', ..] => Ok(Token::new("|", Pipe, self.offset, self.offset + 1, self.file_index)),

			[b'^', b'=', ..] => {
				self.offset += 1;
				Ok(Token::new("^=", CaretAssign, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b'^', ..] => Ok(Token::new("^", Caret, self.offset, self.offset + 1, self.file_index)),

			[b':', b':', ..] => {
				self.offset += 1;
				Ok(Token::new("::", DoubleColon, self.offset - 1, self.offset + 1, self.file_index))
			}

			[b':', ..] => Ok(Token::new(":", Colon, self.offset, self.offset + 1, self.file_index)),

			[b'.', ..] => Ok(Token::new(".", Period, self.offset, self.offset + 1, self.file_index)),

			[b',', ..] => Ok(Token::new(",", Comma, self.offset, self.offset + 1, self.file_index)),

			[b'#', ..] => Ok(Token::new("#", PoundSign, self.offset, self.offset + 1, self.file_index)),

			[b'b', b'\'', ..] => {
				let start_index = self.offset;
				self.offset += 2;
				self.verify_not_eof(messages)?;

				if self.bytes[self.offset] == b'\\' {
					self.offset += 2;
				} else {
					let before_advance = self.offset;
					self.advance_by_codepoint(messages)?;
					if self.offset - before_advance > 1 {
						let error = error!("Byte codepoint literal may not contain a multi-byte codepoint");
						let span = Span {
							start: before_advance,
							end: before_advance + 1, // Only produce a single underscore
							file_index: self.file_index,
						};
						messages.message(error.span(span));
						return Err(());
					}
				}
				self.expect_byte(messages, b'\'')?;

				Ok(Token::new(
					&self.source[start_index + 2..self.offset],
					ByteCodepoint,
					start_index,
					self.offset + 1,
					self.file_index,
				))
			}

			[b'\'', ..] => {
				let start_index = self.offset;
				self.offset += 1;
				self.verify_not_eof(messages)?;

				if self.bytes[self.offset] == b'\\' {
					self.offset += 2;
				} else {
					self.advance_by_codepoint(messages)?;
				}
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

	// This is such a hack
	fn advance_by_codepoint(&mut self, messages: &mut Messages) -> ParseResult<()> {
		self.verify_not_eof(messages)?;

		let mut chars = self.source[self.offset..].char_indices();
		chars.next().ok_or(())?;
		self.offset += chars.next().ok_or(())?.0;

		Ok(())
	}

	//TODO: Remove this
	fn expect_byte(&mut self, messages: &mut Messages, expected: u8) -> ParseResult<()> {
		self.verify_not_eof(messages)?;

		let found = self.bytes[self.offset];
		if found != expected {
			let err = error!("Expected {:?} but found {:?}", expected as char, found as char);
			messages.message(err.span(Span {
				start: self.offset,
				end: self.offset + 1,
				file_index: self.file_index,
			}));

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

	fn verify_not_eof(&self, messages: &mut Messages) -> ParseResult<()> {
		if self.offset >= self.source.len() {
			let error = error!("Unexpected end of file");
			let span = Span {
				start: self.source.len().saturating_sub(1),
				end: self.source.len().saturating_sub(1),
				file_index: self.file_index,
			};
			messages.message(error.span(span));

			Err(())
		} else {
			Ok(())
		}
	}
}
