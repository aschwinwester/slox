package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.scanner
import com.craftinginterpreters.scanner.lox.TokenType.TokenType
import com.craftinginterpreters.scanner.{Scanner, SourceCode, Token}

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Scans a file and creates a SourceCode instance.
 * Return a List of tokens
 */
class LoxScanner extends Scanner {


  def report(line: Int, where:String, message:String):Unit = {
    System.err.println("""Compile error at line %s in %s with the message %s""".format(line, where, message))
  }
  def error(line:Int, message: String): Unit = report(line, "", message)


  def scan(sourceCode: SourceCode): List[Token] = {

    val tokens:ListBuffer[Token] = ListBuffer()

    while (!isAtEnd(currentOffset, sourceCode)) {
      startOffset = currentOffset
      val someToken:Option[Token] = scanToken(sourceCode)
      someToken.map(token => tokens :+ token)
    }
     tokens :+ scanner.Token(TokenType.EOF, "", Nil, line, startOffset)
     tokens.toList
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
    def handleSlash():Option[Token] = {
      // A comment goes until the end of the line.
      if (matchesNextChar('/')) {
        while ((peek ne '\n') && !isAtEnd(currentOffset, sourceCode)) {
          nextChar()
        }
        Option.empty
      } else addToken(TokenType.SLASH)

    }

    def string():Option[Token] = {
      val currentStart:Int = currentOffset
      while ((peek ne '"') && !isAtEnd(currentOffset, sourceCode)) {
        if (peek eq '\n') nextLine()
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
      val stringToken:Token = Token(TokenType.STRING, value, Nil, line, currentStart)
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
      case '(' => addToken(TokenType.LEFT_PAREN)

      case ')' => addToken(TokenType.RIGHT_PAREN)

      case '{' => addToken(TokenType.LEFT_BRACE)

      case '}' => addToken(TokenType.RIGHT_BRACE)

      case ',' => addToken(TokenType.COMMA)

      case '.' => addToken(TokenType.DOT)
      case '-' => addToken(TokenType.MINUS)

      case '+' => addToken(TokenType.PLUS)

      case ';' => addToken(TokenType.SEMICOLON)

      case '*' => addToken(TokenType.STAR)
      case '!' => addToken(if (matchesNextChar('=')) TokenType.BANG_EQUAL else TokenType.BANG)
      case '<' => addToken(if (matchesNextChar('=')) TokenType.LESS_EQUAL else TokenType.LESS)
      case '>' => addToken(if (matchesNextChar('=')) TokenType.GREATER_EQUAL else TokenType.GREATER)
      case '=' => addToken(if (matchesNextChar('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL)
      case '/' => handleSlash()
      case ' ' => Option.empty
      case '\t' => Option.empty
      case '\r' => Option.empty
      case '\n' =>
        nextLine()
        Option.empty
      case '"' => string()
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
  def scanFile(fileName: String):List[Token] = {
    val source:String = readFile(fileName)
    val sourceCode:SourceCode = new SourceCode(source)
    scan(sourceCode)
  }
}
