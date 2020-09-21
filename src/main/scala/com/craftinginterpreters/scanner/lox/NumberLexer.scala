package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.{MessageListener, MessagePublisher}
import com.craftinginterpreters.scanner.exceptions.IllegalTokenException
import com.craftinginterpreters.scanner.{ScannerPosition, SourceCode, Token, Lexer}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

class NumberLexer(val messageListeners: List[MessageListener]) extends Lexer with MessagePublisher {

  override def pattern: Regex = "[0-9]".r

  @tailrec
  final def getToken(code: SourceCode, pos:ScannerPosition):(Try[Token], ScannerPosition) = {
    val c:Char = code.peek(pos.currentOffset)

    if (isScanningDone(code, pos, c, pattern)) {
      val value = code.text(pos.startOffset, pos.currentOffset)
      val token = Token(LoxTokenType.NUMBER, value, Nil, pos.line, pos.startOffset)
      (Success(token), pos)
    } else if (matches(c, pattern)) {
      getToken(code, pos.nextPosition)
    } else {
      (Failure(IllegalTokenException("Illegal token found during creating number token.", pos.currentOffset, pos.line)), pos)
    }
  }


}
