package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.{MessageListener, MessagePublisher}
import com.craftinginterpreters.scanner.exceptions.IllegalTokenException
import com.craftinginterpreters.scanner.{ScannerPosition, SourceCode, Token, Lexer}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

class StringLexer(val messageListeners: List[MessageListener]) extends Lexer with MessagePublisher {

 override def pattern:Regex = """"""".r
 private def isStarting(pos:ScannerPosition) = pos.startOffset == pos.currentOffset

 @tailrec
 final def getToken(code: SourceCode, pos:ScannerPosition):(Try[Token], ScannerPosition) = {
  val c:Char = code.peek(pos.currentOffset)
  val notClosing = c.!=('"')
  val notAtEnd = !code.isAtEnd(pos.currentOffset)
  if (isStarting(pos) ) {
   if (c.==('"')) {
    getToken(code, pos.nextPosition)
   } else {
     (Failure(IllegalTokenException("Not starting with double quote.", pos.currentOffset, pos.line)), pos)
   }
  } else if (notClosing && notAtEnd) {
   if (code.peek(pos.currentOffset).==('\n')) {
    val newpos = pos.nextLine
    getToken(code, newpos.nextPosition)
   } else {
    getToken(code, pos.nextPosition)
   }

  } else {
   // we have stopped either at the end or double quote

   // Unterminated string while at end
   if (code.isAtEnd(pos.currentOffset)) {
    return (Failure(IllegalTokenException("Unterminated string. End reached without closing string.", pos.currentOffset, pos.line)), pos)
   }

   // not at end, but char is not double quote
   // The closing ".

   if (c != '"') {
    return (Failure(IllegalTokenException("Unterminated string.", pos.currentOffset, pos.line)), pos)
   }

   // consume
   val newpos = pos.nextPosition
   // Trim the surrounding quotes.
   val value = code.text(newpos.startOffset + 1, newpos.currentOffset - 1)
   val stringToken:Token = Token(LoxTokenType.STRING, value, Nil, pos.line, newpos.startOffset)
   (Success(stringToken), newpos)
  }
 }
}
