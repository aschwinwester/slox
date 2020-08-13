package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.scanner
import com.craftinginterpreters.scanner.{Scanner, SourceCode, Token, TokenType}

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.{Success, Try}

/**
 * Scans a file and creates a SourceCode instance.
 * Return a List of tokens
 */
class LoxScanner extends Scanner {


  def report(line: Int, where:String, message:String):Unit = {
    System.err.println("""Compile error at line %s in %s with the message %s""".format(line, where, message))
  }
  def error(line:Int, message: String): Unit = report(line, "", message)


  def scan(sourceCode: SourceCode): Try[List[Token]] = {

    val tokens:ListBuffer[Token] = ListBuffer()

    while (!isAtEnd(currentOffset, sourceCode)) {
      startOffset = currentOffset
      val someToken:Option[Token] = scanToken(sourceCode)
      someToken.map(token => tokens :+ token)
    }
     tokens :+ scanner.Token(LoxTokenType.EOF, "", Nil, line, startOffset)
     Success(tokens.toList)
  }

  def nextLine(): Unit = {
    line += 1
  }


  def scanToken(sourceCode: SourceCode):Option[Token] = {

    def peek: Char = if (isAtEnd(currentOffset, sourceCode)) '\0' else getChar(sourceCode, currentOffset)

    def nextPosition():Unit = {
      currentOffset = currentOffset + 1
    }
    def createToken(tokenType:TokenType, literal:Object):Token = {
      val text = sourceCode.text(startOffset, currentOffset)
      scanner.Token(tokenType, text, literal, line, startOffset)
    }

    def matchesNextChar(c:Char): Boolean = {
      if (isAtEnd(currentOffset, sourceCode)) return false
      else if (getChar(sourceCode, currentOffset) != c) return false

      // It’s like a conditional nextChar().
      // It only consumes the current character if it’s what we’re looking for.
      nextPosition()
      true
    }
    def handleSlash:Option[Token] = {
      // A comment goes until the end of the line.
      if (matchesNextChar('/')) {
        while ((peek.!=('\n')) && !isAtEnd(currentOffset, sourceCode)) {
          nextChar()
        }
        Option.empty
      } else addToken(LoxTokenType.SLASH)

    }

    def string:Option[Token] = {
      val currentStart:Int = currentOffset
      while ((peek.!=('"')) && !isAtEnd(currentOffset, sourceCode)) {
        if (peek.==('\n')) nextLine()
        nextChar()
      }

      // Unterminated string.
      if (isAtEnd(currentOffset, sourceCode)) {
        error(line, "Unterminated string.")
        return Option.empty
      }

      // The closing ".
      val doubleQuote:Char = nextChar()
      if (doubleQuote != '"') {
        error(line, "Unterminated string.")
      }

      // Trim the surrounding quotes.
      val value = sourceCode.text(startOffset + 1, currentOffset - 1)
      val stringToken:Token = Token(LoxTokenType.STRING, value, Nil, line, currentStart)
      Some(stringToken)
    }

   def isAlpha(c:Char):Boolean =
     (c >= 'a' && c <= 'z') ||
       (c >= 'A' && c <= 'Z') ||
       c == '_'

    def number:Option[Token] = ???

    def identifier:Option[Token] = ???


    /**
     * Also the character at position 0 must be read
     * So we read the current char and move the position
     *
     * @return
     */
    def nextChar():Char = {

       val char:Char = getChar(sourceCode, currentOffset)
       nextPosition()
       char
    }
    def addToken(tokenType:TokenType): Option[Token] = Some(createToken(tokenType, Nil))

    // this does not look very logic
    val character = nextChar()
    val token:Option[Token] = character match {
      case '(' => addToken(LoxTokenType.LEFT_PAREN)

      case ')' => addToken(LoxTokenType.RIGHT_PAREN)

      case '{' => addToken(LoxTokenType.LEFT_BRACE)

      case '}' => addToken(LoxTokenType.RIGHT_BRACE)

      case ',' => addToken(LoxTokenType.COMMA)

      case '.' => addToken(LoxTokenType.DOT)
      case '-' => addToken(LoxTokenType.MINUS)

      case '+' => addToken(LoxTokenType.PLUS)

      case ';' => addToken(LoxTokenType.SEMICOLON)

      case '*' => addToken(LoxTokenType.STAR)
      case '!' => addToken(if (matchesNextChar('=')) LoxTokenType.BANG_EQUAL else LoxTokenType.BANG)
      case '<' => addToken(if (matchesNextChar('=')) LoxTokenType.LESS_EQUAL else LoxTokenType.LESS)
      case '>' => addToken(if (matchesNextChar('=')) LoxTokenType.GREATER_EQUAL else LoxTokenType.GREATER)
      case '=' => addToken(if (matchesNextChar('=')) LoxTokenType.EQUAL_EQUAL else LoxTokenType.EQUAL)
      case '/' => handleSlash
      case ' ' => Option.empty
      case '\t' => Option.empty
      case '\r' => Option.empty
      case '\n' =>
        nextLine()
        Option.empty
      case '"' => string
      case someChar =>
        if (isDigit(someChar)) number
        else if (isAlpha(someChar)) identifier
        else {
        error(line, "Unknown char")
        Option.empty
      }
    }

    token

  }

   private def readFile(fileName: String): String = {
    val sourceFile = Source.fromFile(fileName)
    val source = sourceFile.getLines().mkString
    sourceFile.close()
    source
  }
  def scanFile(fileName: String):Try[List[Token]] = {
    val source:String = readFile(fileName)
    val sourceCode:SourceCode = new SourceCode(source)
    scan(sourceCode)
  }
}
