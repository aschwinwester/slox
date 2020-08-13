package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.scanner.TokenType

sealed trait LoxTokenType extends TokenType
sealed trait SingleSymbolToken extends LoxTokenType {def character: Char;}

object LoxTokenType {

  // Single-character tokens.
  case object LEFT_PAREN extends SingleSymbolToken {val character='('}

  case object RIGHT_PAREN extends LoxTokenType

  case object LEFT_BRACE extends LoxTokenType

  case object RIGHT_BRACE extends LoxTokenType

  case object COMMA extends LoxTokenType

  case object DOT extends LoxTokenType

  case object MINUS extends LoxTokenType

  case object PLUS extends LoxTokenType

  case object SEMICOLON extends LoxTokenType

  case object SLASH extends LoxTokenType

  case object STAR extends LoxTokenType

  case object BANG extends LoxTokenType

  case object BANG_EQUAL extends LoxTokenType

  case object EQUAL extends LoxTokenType

  case object EQUAL_EQUAL extends LoxTokenType

  case object GREATER extends LoxTokenType

  case object GREATER_EQUAL extends LoxTokenType

  case object LESS extends LoxTokenType

  case object LESS_EQUAL extends LoxTokenType

  // Literals.
  case object IDENTIFIER extends LoxTokenType

  case object STRING extends LoxTokenType

  case object NUMBER extends LoxTokenType

  // Keywords.
  case object AND extends LoxTokenType

  case object CLASS extends LoxTokenType

  case object ELSE extends LoxTokenType

  case object FALSE extends LoxTokenType

  case object FUN extends LoxTokenType

  case object FOR extends LoxTokenType

  case object IF extends LoxTokenType

  case object NIL extends LoxTokenType

  case object OR extends LoxTokenType

  case object PRINT extends LoxTokenType

  case object RETURN extends LoxTokenType

  case object SUPER extends LoxTokenType

  case object THIS extends LoxTokenType

  case object TRUE extends LoxTokenType

  case object VAR extends LoxTokenType

  case object WHILE extends LoxTokenType

  case object EOF extends LoxTokenType

}
