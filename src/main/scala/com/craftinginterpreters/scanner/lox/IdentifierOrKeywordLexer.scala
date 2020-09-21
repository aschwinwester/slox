package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.{MessageListener, MessagePublisher}
import com.craftinginterpreters.scanner.exceptions.IllegalTokenException
import com.craftinginterpreters.scanner.lox.LoxTokenType.{AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR}
import com.craftinginterpreters.scanner.lox.LoxTokenType.{PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE}
import com.craftinginterpreters.scanner.{Lexer, ScannerPosition, SourceCode, Token}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

class IdentifierOrKeywordLexer(val messageListeners: List[MessageListener]) extends Lexer with MessagePublisher {
  override def pattern: Regex = "[a-zA-Z_]".r

  @tailrec
  final def getToken(code: SourceCode, pos:ScannerPosition):(Try[Token], ScannerPosition) = {
    val c:Char = code.peek(pos.currentOffset)
    if (isScanningDone(code, pos, c, pattern)) {
      val value = code.text(pos.startOffset, pos.currentOffset)
      // if keyword is found, take keyword, otherwise it is a identifier.
      val result = keyword(value) match {
        case Some(keywordToken) =>  (Success(Token(keywordToken, value, Nil, pos.line, pos.startOffset)), pos)
        case None => (Success(Token(LoxTokenType.IDENTIFIER, value, Nil, pos.line, pos.startOffset)), pos)
      }
      result
    } else if (matches(c, pattern)) {
      getToken(code, pos.nextPosition)
    } else {
      (Failure(IllegalTokenException("Illegal token found during creating identifier or keyword token.", pos.currentOffset, pos.line)), pos)
    }
  }

  def keyword(value:String):Option[KeywordToken]= {
    val k = Keyword(value)
    k match {
      case AND => Some(AND)
      case CLASS => Some(CLASS)
      case ELSE => Some(ELSE)
      case FALSE => Some(FALSE)
      case FOR => Some(FOR)
      case IF => Some(IF)
      case FUN => Some(FUN)
      case NIL => Some(NIL)
      case OR => Some(OR)
      case PRINT => Some(PRINT)
      case RETURN => Some(RETURN)
      case SUPER => Some(SUPER)
      case THIS => Some(THIS)
      case TRUE => Some(TRUE)
      case VAR => Some(VAR)
      case WHILE => Some(WHILE)

      case _ => None
    }
  }

}
