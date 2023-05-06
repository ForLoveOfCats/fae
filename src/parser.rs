use crate::error::{Messages, ParseResult};
use crate::file::SourceFile;
use crate::span::Span;
use crate::tokenizer::{Token, TokenKind, Tokenizer};
use crate::tree::*;

pub fn parse_file<'a>(messages: &mut Messages, file: &'a SourceFile) -> File<'a> {
	let mut tokenizer = Tokenizer::new(&file.source);
	let block = parse_root_block(messages, &mut tokenizer);

	let module_path = &file.module_path;
	File { source_file: file, module_path, block }
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

		match token {
			Token { kind: TokenKind::Word, text: "using", .. } => {
				disallow_attributes(messages, attributes, token.span, "A using statement");
				if let Ok(statement) = parse_using_statement(messages, tokenizer) {
					items.push(Statement::Using(statement));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::Word, text: "const", .. } => {
				disallow_attributes(messages, attributes, token.span, "A const definition");
				if let Ok(statement) = parse_const_statement(messages, tokenizer) {
					items.push(Statement::Const(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::Word, text: "let" | "mut", .. } => {
				disallow_attributes(messages, attributes, token.span, "A let statement");
				if let Ok(statement) = parse_binding_statement(messages, tokenizer) {
					items.push(Statement::Binding(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::Word, text: "fn", .. } => {
				if let Ok(statement) = parse_function_declaration(messages, tokenizer, attributes) {
					items.push(Statement::Function(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::Word, text: "struct", .. } => {
				if let Ok(statement) = parse_struct_declaration(messages, tokenizer, attributes) {
					items.push(Statement::Struct(statement));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::Word, text: "return", .. } => {
				disallow_attributes(messages, attributes, token.span, "A return statement");
				if let Ok(statement) = parse_return_statement(messages, tokenizer) {
					items.push(Statement::Return(Box::new(statement)));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::OpenBrace, .. } => {
				disallow_attributes(messages, attributes, token.span, "A block");
				if let Ok(statement) = parse_block(messages, tokenizer) {
					items.push(Statement::Block(statement));
				} else {
					consume_error_syntax(messages, tokenizer);
				}
			}

			Token { kind: TokenKind::CloseBrace, .. } => break,

			_ => {
				disallow_attributes(messages, attributes, token.span, "An expression");
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

//TODO: Add function to only disallow specific attribute kinds
fn disallow_attributes(messages: &mut Messages, attributes: Attributes, span: Span, label: &str) {
	let mut buffer = [Span::zero(); Attributes::FIELD_COUNT];
	let spans = attributes.attribute_spans(&mut buffer);

	if !spans.is_empty() {
		let mut message = message!("{label} does not allow attributes").span(span);
		for &span in spans {
			message = message.note(note!(span, messages.current_file_index(), "Attribute here"));
		}

		messages.error(message);
	}
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

	while let Some(operator) = tokenizer.peek().ok().and_then(|token| token_to_operator(token)) {
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

		let right = parse_expression_climb(messages, tokenizer, next_min_precedence)?;
		let left_span = result.span;
		let right_span = right.span;

		let binary_operation = BinaryOperation { op: operator, right, left: result };
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

			Ok(Node::from_token(Expression::StringLiteral(StringLiteral { value }), string_token))
		}

		TokenKind::Codepoint => {
			let codepoint_token = tokenizer.expect(messages, TokenKind::Codepoint)?;
			let value = Node::from_token(codepoint_token.text.chars().next().unwrap(), codepoint_token);

			Ok(Node::from_token(
				Expression::CodepointLiteral(CodepointLiteral { value }),
				codepoint_token,
			))
		}

		TokenKind::Word => {
			if peeked.text.as_bytes()[0].is_ascii_digit() {
				return parse_number(messages, tokenizer);
			}

			let path_segments = parse_path_segments(messages, tokenizer)?;

			let (is_call, is_struct_literal) = match tokenizer.peek() {
				Ok(Token { kind: TokenKind::OpenParen | TokenKind::OpenBracket, .. }) => (true, false),
				Ok(Token { kind: TokenKind::OpenBrace, .. }) => (false, true),
				_ => (false, false),
			};

			if is_call {
				let type_arguments = parse_type_arguments(messages, tokenizer)?;
				let arguments_node = parse_arguments(messages, tokenizer)?;
				let arguments = arguments_node.item;
				let span = path_segments.span + arguments_node.span;
				let call = Call { path_segments, type_arguments, arguments };

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
			let block = parsed_block.item;

			Ok(Node::new(Expression::Block(block), span))
		}

		_ => {
			messages.error(
				message!("Unexpected token {:?} while attempting to parse expression atom", peeked.text)
					.span(peeked.span),
			);
			Err(())
		}
	}
}

fn parse_type_arguments<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Vec<Type<'a>>> {
	tokenizer.expect(messages, TokenKind::OpenBracket)?;

	let mut types = Vec::new();
	while !reached_close_bracket(tokenizer) {
		types.push(parse_type(messages, tokenizer)?.item);

		if reached_close_bracket(tokenizer) {
			break;
		}

		tokenizer.expect(messages, TokenKind::Comma)?;
	}

	tokenizer.expect(messages, TokenKind::CloseBracket)?;

	Ok(types)
}

fn parse_arguments<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<Vec<Node<Expression<'a>>>>> {
	let open_paren_token = tokenizer.expect(messages, TokenKind::OpenParen)?;

	let mut expressions = Vec::new();
	while !reached_close_paren(tokenizer) {
		expressions.push(parse_expression(messages, tokenizer)?);

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

	while tokenizer.peek_kind() != Ok(TokenKind::CloseBrace) {
		let name_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, name_token, "field name")?;
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
		return Ok(Node::new(Expression::FloatLiteral(FloatLiteral { value: Node::new(value, span) }), span));
	} else {
		let value = match first_number_token.text.parse::<u64>() {
			Ok(value) => value,
			Err(_) => {
				messages.error(message!("Invalid integer literal").span(first_number_token.span));
				return Err(());
			}
		};

		return Ok(Node::from_token(
			Expression::IntegerLiteral(IntegerLiteral { value: Node::from_token(value, first_number_token) }),
			first_number_token,
		));
	}
}

fn parse_attributes<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Attributes<'a>> {
	fn check_duplicate_attribute<T>(
		messages: &mut Messages,
		attribute: &Option<Node<T>>,
		name: &str,
		duplicate_span: Span,
	) -> ParseResult<()> {
		if let Some(attribute) = attribute {
			messages.error(
				message!("Duplicate attribute {name:?}")
					.span(duplicate_span)
					.note(note!(attribute.span, messages.current_file_index(), "Original here")),
			);
			return Err(());
		}

		Ok(())
	}

	let mut attributes = Attributes::blank();

	while let Ok(peeked) = tokenizer.peek() {
		match peeked.text {
			"generic" => {
				check_duplicate_attribute(messages, &attributes.generic_attribute, "generic", peeked.span)?;
				attributes.generic_attribute = Some(parse_generic_attribute(messages, tokenizer)?);
			}

			_ => break,
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

fn parse_using_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Using<'a>>> {
	let using_token = tokenizer.expect_word(messages, "using")?;

	let path_segments = parse_path_segments(messages, tokenizer)?;

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = using_token.span + path_segments.span;
	let item = Using { path_segments };
	Ok(Node { item, span })
}

fn parse_path_segments<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Node<PathSegments<'a>>> {
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

			if tokenizer.peek_kind() == Ok(TokenKind::OpenBracket) {
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
			let segments = parsed_path.item;

			let mut arguments = Vec::new();
			let span = match tokenizer.peek() {
				Ok(Token { kind: TokenKind::OpenBracket, .. }) => {
					tokenizer.expect(messages, TokenKind::OpenBracket)?;

					if tokenizer.peek_kind() != Ok(TokenKind::CloseBracket) {
						loop {
							arguments.push(parse_type(messages, tokenizer)?);
							if tokenizer.peek_kind() != Ok(TokenKind::Comma) {
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

fn parse_function_declaration<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
	attributes: Attributes<'a>,
) -> ParseResult<Function<'a>> {
	let generics = match attributes.generic_attribute {
		Some(attribute) => attribute.item.names,
		None => Vec::new(),
	};

	tokenizer.expect_word(messages, "fn")?;

	let name_token = tokenizer.expect(messages, TokenKind::Word)?;
	check_not_reserved(messages, name_token, "function name")?;
	let name = Node::from_token(name_token.text, name_token);

	let parameters = parse_parameters(messages, tokenizer)?;

	tokenizer.expect(messages, TokenKind::Colon)?;
	let parsed_type = parse_type(messages, tokenizer)?;

	let block = parse_block(messages, tokenizer)?;

	Ok(Function { generics, name, parameters, parsed_type, block })
}

fn parse_parameters<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
) -> ParseResult<Vec<Node<Parameter<'a>>>> {
	tokenizer.expect(messages, TokenKind::OpenParen)?;

	let mut parameters = Vec::new();

	while !reached_close_paren(tokenizer) {
		let is_mutable = match tokenizer.peek() {
			Ok(Token { text: "mut", .. }) => {
				tokenizer.expect_word(messages, "mut")?;
				true
			}

			_ => false,
		};

		let name_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, name_token, "parameter name")?;
		let name = Node::from_token(name_token.text, name_token);

		tokenizer.expect(messages, TokenKind::Colon)?;

		let parsed_type = parse_type(messages, tokenizer)?;

		let span = name_token.span + parsed_type.span;
		let parameter = Parameter { name, parsed_type, is_mutable };
		parameters.push(Node::new(parameter, span));

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
	attributes: Attributes<'a>,
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
	tokenizer.expect(messages, TokenKind::Newline)?;

	let mut fields = Vec::new();

	while !reached_close_brace(tokenizer) {
		let field_name_token = tokenizer.expect(messages, TokenKind::Word)?;
		check_not_reserved(messages, field_name_token, "struct field")?;
		let name = Node::from_token(field_name_token.text, field_name_token);

		tokenizer.expect(messages, TokenKind::Colon)?;

		let parsed_type = parse_type(messages, tokenizer)?;

		fields.push(Field { name, parsed_type });

		tokenizer.expect(messages, TokenKind::Newline)?;
	}

	tokenizer.expect(messages, TokenKind::CloseBrace)?;

	Ok(Struct { generics, name, fields })
}

fn parse_const_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Const<'a>>> {
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

	let expression = parse_expression(messages, tokenizer)?;

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = const_token.span + expression.span;
	let item = Const { name, parsed_type, expression };
	Ok(Node { item, span })
}

fn parse_binding_statement<'a>(
	messages: &mut Messages,
	tokenizer: &mut Tokenizer<'a>,
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

	let expression = parse_expression(messages, tokenizer)?;

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = keyword_token.span + expression.span;
	let item = Binding { name, parsed_type, expression, is_mutable };
	Ok(Node { item, span })
}

fn parse_return_statement<'a>(messages: &mut Messages, tokenizer: &mut Tokenizer<'a>) -> ParseResult<Node<Return<'a>>> {
	let return_token = tokenizer.expect_word(messages, "return")?;

	let expression = match tokenizer.peek_kind() {
		Ok(TokenKind::Newline) => None,
		_ => Some(parse_expression(messages, tokenizer)?),
	};

	tokenizer.expect(messages, TokenKind::Newline)?;

	let span = match &expression {
		Some(expression) => return_token.span + expression.span,
		None => return_token.span,
	};

	let item = Return { expression };
	Ok(Node { item, span })
}

fn check_not_reserved(messages: &mut Messages, token: Token, use_as: &str) -> ParseResult<()> {
	let is_reserved = matches!(token.text, "const" | "fn" | "let" | "mut" | "return" | "struct" | "using" | "generic");

	if is_reserved {
		messages.error(message!("Cannot use reserved word {:?} as {use_as}", token.text).span(token.span));
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

fn reached_close_bracket(tokenizer: &mut Tokenizer) -> bool {
	tokenizer
		.peek()
		.map(|peeked| peeked.kind == TokenKind::CloseBracket)
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
