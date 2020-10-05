package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.{MessageListener, MessagePublisher}
import com.craftinginterpreters.scanner
import com.craftinginterpreters.scanner.{Scanner, ScannerPosition, SourceCode, Token}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Scans a file and creates a SourceCode instance.
 * Return a List of tokens
 */
class LoxScanner(val messageListeners: List[MessageListener]) extends Scanner with MessagePublisher {

  private val stringLexer = new StringLexer(messageListeners)
  private val numberLexer = new NumberLexer(messageListeners)
  private val wordLexer = new IdentifierOrKeywordLexer(messageListeners)
  private val symbolLexer = new SymbolLexer(messageListeners)
  private val commentLexer = new CommentLexer(messageListeners)

  def scan(sourceCode: SourceCode): Try[List[Token]] = {

    @tailrec
    def scanFromPosition(code: SourceCode, tokens:List[Token], pos: ScannerPosition) : Try[List[Token]] = {
      if (isAtEnd(pos.currentOffset, sourceCode)) {
        report(pos.line, pos.currentOffset.toString, "At end")
        Success(tokens :+ scanner.Token(LoxTokenType.EOF, "", Nil, pos.line, pos.startOffset))
      } else {
        val resetPos = ScannerPosition(pos.currentOffset, pos.currentOffset, pos.line)
        val tuple = scanToken(sourceCode, resetPos)
        val possibleToken:Try[Token] = tuple._1
        val newPos = tuple._2

        possibleToken match {
          case Success(token) =>
            report(token.line, "position %s".format(token.position),
              "type %s value [%s]".format(token.tokenType.toString, token.lexeme))
          case Failure(exception) =>
            error(resetPos.line, exception.getMessage)
        }

        val newTokenList:List[Token] = possibleToken.map(token => tokens :+ token).getOrElse(tokens)
        scanFromPosition(code, newTokenList, newPos)
      }
    }

    scanFromPosition(sourceCode, List(), ScannerPosition.start)
  }

  private def scanToken(sourceCode: SourceCode, pos:ScannerPosition):(Try[Token], ScannerPosition) = {

    val char:Char = getChar(sourceCode, pos.currentOffset)
    report(pos.line, "position %s".format(pos.currentOffset), "found char [%s]".format(char))
    val p = numberLexer.pattern
    val s = stringLexer.pattern
    val k = wordLexer.pattern
    val y = symbolLexer.pattern
    val m = commentLexer.pattern
    val tokenWithPos:(Try[Token], ScannerPosition) = char match {
      case s() => stringLexer.getToken(sourceCode, pos)
      case p() => numberLexer.getToken(sourceCode, pos)
      case k() => wordLexer.getToken(sourceCode, pos)
      case m() => 
        if (CommentLexer.isComment(sourceCode, pos)) {

          val (tryToken, newPos) = commentLexer.getToken(sourceCode, pos)
          tryToken match {
            case Success(_) => scanToken(sourceCode, newPos)
            case _ => return (tryToken, newPos)
          }
        } else symbolLexer.getToken(sourceCode, pos)
      case y() => symbolLexer.getToken(sourceCode, pos)
      case ' ' => skipToken(' ', "space", pos); scanToken(sourceCode, pos.skipPosition)
      case '\t' => skipToken('\t', "tab", pos); scanToken(sourceCode, pos.skipPosition)
      case '\r' => skipToken('\r', "read", pos); scanToken(sourceCode, pos.skipPosition)
      case '\n' => skipToken('\n', "newline", pos); scanToken(sourceCode, pos.nextLine.skipPosition)


      case _ => (Success(Token(LoxTokenType.EOF, "", Nil, pos.line, pos.currentOffset)), pos.nextLine)
    }

    tokenWithPos
  }

  private def skipToken(c:Char, name:String, pos:ScannerPosition):Unit =
    report(pos.line, "position %s".format(pos.currentOffset), "skipping %s token [%s] ".format(name, c))

  private def readFile(fileName: String): String = {
    val sourceFile = Source.fromResource(fileName)
    val source = sourceFile.getLines().mkString("\n")
    sourceFile.close()
    source
  }
  def scanFile(fileName: String):Try[List[Token]] = {
    val source:String = readFile(fileName)
    val sourceCode:SourceCode = SourceCode(source)
    scan(sourceCode)
  }
}
