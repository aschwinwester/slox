package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.{MessageListener, MessagePublisher}
import com.craftinginterpreters.scanner.exceptions.IllegalTokenException
import com.craftinginterpreters.scanner.{Lexer, ScannerPosition, SourceCode, Token}

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

class SymbolLexer (val messageListeners: List[MessageListener]) extends Lexer with MessagePublisher {
  override def pattern: Regex = "[{}\\)\\(+*-/,;.><=!]".r

  private def createSingleToken(s: SingleSymbol, pos:ScannerPosition):Token =
    Token(s, s.character.toString, Nil, pos.line, pos.currentOffset)

  final def getToken(code: SourceCode, pos:ScannerPosition):(Try[Token], ScannerPosition) = {
    val c:Char = code.peek(pos.currentOffset)
    val sy = SingleSymbol(c)
    val tryToken:(Try[Token], ScannerPosition) = sy match {
      case LoxTokenType.LEFT_BRACE => (Success(createSingleToken(LoxTokenType.LEFT_BRACE, pos)), pos.nextPosition)
      case LoxTokenType.RIGHT_BRACE => (Success(createSingleToken(LoxTokenType.RIGHT_BRACE, pos)), pos.nextPosition)
      case LoxTokenType.LEFT_PAREN => (Success(createSingleToken(LoxTokenType.LEFT_PAREN, pos)), pos.nextPosition)
      case LoxTokenType.RIGHT_PAREN => (Success(createSingleToken(LoxTokenType.RIGHT_PAREN, pos)), pos.nextPosition)
      case LoxTokenType.COMMA => (Success(createSingleToken(LoxTokenType.COMMA, pos)), pos.nextPosition)
      case LoxTokenType.DOT => (Success(createSingleToken(LoxTokenType.DOT, pos)), pos.nextPosition)
      case LoxTokenType.SEMICOLON => (Success(createSingleToken(LoxTokenType.SEMICOLON, pos)), pos.nextPosition)
      case LoxTokenType.PLUS => (Success(createSingleToken(LoxTokenType.PLUS, pos)), pos.nextPosition)
      case LoxTokenType.MINUS => (Success(createSingleToken(LoxTokenType.MINUS, pos)), pos.nextPosition)
      case LoxTokenType.STAR => (Success(createSingleToken(LoxTokenType.STAR, pos)), pos.nextPosition)
      case LoxTokenType.SLASH => (Success(createSingleToken(LoxTokenType.SLASH, pos)), pos.nextPosition)
      case LoxTokenType.BANG =>
        if (nextCharMatchesSymbol(LoxTokenType.EQUAL, code, pos)) (Success(createToken(LoxTokenType.BANG_EQUAL, pos)), pos.nextPosition.nextPosition)
        else (Success(createSingleToken(LoxTokenType.BANG, pos)), pos.nextPosition)
      case LoxTokenType.EQUAL =>
        if (nextCharMatchesSymbol(LoxTokenType.EQUAL, code, pos)) (Success(createToken(LoxTokenType.EQUAL_EQUAL, pos)), pos.nextPosition.nextPosition)
        else (Success(createSingleToken(LoxTokenType.EQUAL, pos)), pos.nextPosition)
      case LoxTokenType.GREATER =>
        if (nextCharMatchesSymbol(LoxTokenType.EQUAL, code, pos)) (Success(createToken(LoxTokenType.GREATER_EQUAL, pos)), pos.nextPosition.nextPosition)
        else (Success(createSingleToken(LoxTokenType.GREATER, pos)), pos.nextPosition)
      case LoxTokenType.LESS =>
        if (nextCharMatchesSymbol(LoxTokenType.EQUAL, code, pos)) (Success(createToken(LoxTokenType.LESS_EQUAL, pos)), pos.nextPosition.nextPosition)
        else (Success(createSingleToken(LoxTokenType.LESS, pos)), pos.nextPosition)

      case _ => (Failure(IllegalTokenException("Illegal token found during creating identifier or keyword token.", pos.currentOffset, pos.line)), pos)
    }
    tryToken
  }

  private def createToken(text: NonSingleSymbol, pos:ScannerPosition): Token =
    Token(text, text.characters, Nil, pos.line, pos.currentOffset)


  private def nextCharMatchesSymbol(symbol:SingleSymbolToken, code:SourceCode, pos:ScannerPosition): Boolean =
    code.peek(pos.currentOffset + 1) == symbol.character

}
