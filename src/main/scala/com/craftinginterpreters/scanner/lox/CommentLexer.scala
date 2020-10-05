package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.{MessageListener, MessagePublisher}
import com.craftinginterpreters.scanner.exceptions.IllegalTokenException
import com.craftinginterpreters.scanner.{Lexer, ScannerPosition, SourceCode, Token}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex
object CommentLexer {
  def isComment(code:SourceCode, pos:ScannerPosition):Boolean =
    code.peek(pos.currentOffset) == '/' && code.peek(pos.currentOffset + 1) == '/'
}
class CommentLexer (val messageListeners: List[MessageListener]) extends Lexer with MessagePublisher {
  override def pattern: Regex = "/".r

  private def isStarting(pos:ScannerPosition):Boolean = pos.startOffset == pos.currentOffset

  @tailrec
  final def getToken(code: SourceCode, pos: ScannerPosition): (Try[Token], ScannerPosition) = {
    val c: Char = code.peek(pos.currentOffset)
    if (isStarting(pos)) {
      if (CommentLexer.isComment(code, pos)) getToken(code, pos.nextPosition.nextPosition)
      else (Failure(IllegalTokenException("Not starting with double slash.", pos.currentOffset, pos.line)), pos)
    } else if (isNewLine(c)) {
      val text = getComment(code, pos)
      (Success(Token(LoxTokenType.COMMENT, text,Nil, pos.line, pos.startOffset)), pos)
    } else if (isAtEnd(pos.currentOffset, code)) {
      val text = getComment(code, pos)
      (Success(Token(LoxTokenType.COMMENT, text,Nil, pos.line, pos.startOffset)), pos)
    } else {
      getToken(code, pos.nextPosition)
    }
  }

  private def getComment(code: SourceCode, pos: ScannerPosition) =
    code.source.substring(pos.startOffset + 2, pos.currentOffset)
  private def isNewLine(c:Char):Boolean = c.==('\n') || c.==('\r')
}
