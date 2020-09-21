package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.scanner.TokenType

sealed trait LoxTokenType extends TokenType
sealed trait SingleSymbolToken extends LoxTokenType {def character: Char;}
sealed trait NonSingleSymbolToken extends LoxTokenType {def characters: String;}
sealed trait KeywordToken extends LoxTokenType {def characters: String;}

case class Keyword(characters:String) extends KeywordToken
case class SingleSymbol(character:Char) extends SingleSymbolToken
case class NonSingleSymbol(characters: String) extends NonSingleSymbolToken

object LoxTokenType {

  // Single-character tokens.

  val LEFT_BRACE:SingleSymbol = SingleSymbol('{')

  val RIGHT_BRACE:SingleSymbol = SingleSymbol('}')

  val COMMA:SingleSymbol = SingleSymbol(',')

  val DOT:SingleSymbol = SingleSymbol('.')

  val SEMICOLON:SingleSymbol = SingleSymbol(';')

  val BANG:SingleSymbol = SingleSymbol('!')

  // mathematical operators

  val MINUS:SingleSymbol = SingleSymbol('-')

  val PLUS:SingleSymbol = SingleSymbol('+')

  val SLASH:SingleSymbol = SingleSymbol('/')

  val STAR:SingleSymbol = SingleSymbol('*')

  // grouping
  val LEFT_PAREN:SingleSymbol = SingleSymbol('(')

  val RIGHT_PAREN:SingleSymbol = SingleSymbol(')')


  // equality operators
  val BANG_EQUAL:NonSingleSymbol = NonSingleSymbol("!=")

  val EQUAL:SingleSymbol = SingleSymbol('=')

  val EQUAL_EQUAL:NonSingleSymbol = NonSingleSymbol("==")

  val GREATER:SingleSymbol = SingleSymbol('>')

  val GREATER_EQUAL:NonSingleSymbol = NonSingleSymbol(">=")

  val LESS:SingleSymbol = SingleSymbol('<')

  val LESS_EQUAL:NonSingleSymbol = NonSingleSymbol("<=")

  // Literals.
  case object IDENTIFIER extends LoxTokenType

  case object STRING extends LoxTokenType

  case object NUMBER extends LoxTokenType

  // Keywords.
  val AND:Keyword = Keyword("and")

  val CLASS:Keyword = Keyword("class")

  val ELSE:Keyword = Keyword("else")

  val FALSE:Keyword = Keyword("false")

  val FUN:Keyword = Keyword("fun")

  val FOR:Keyword = Keyword("for")

  val IF:Keyword = Keyword("if")

  val NIL:Keyword = Keyword("nil")

  val OR:Keyword = Keyword("or")

  val PRINT:Keyword = Keyword("print")

  val RETURN:Keyword = Keyword("return")

  val SUPER:Keyword = Keyword("super")

  val THIS:Keyword = Keyword("this")

  val TRUE:Keyword = Keyword("true")

  val VAR:Keyword = Keyword("var")

  val WHILE:Keyword = Keyword("while")

  // other

  val EOF:Keyword = Keyword("eof")

}
