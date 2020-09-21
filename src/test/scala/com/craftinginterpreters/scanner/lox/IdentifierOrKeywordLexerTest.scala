package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.ConsoleMessageListener
import com.craftinginterpreters.scanner.{ScannerPosition, SourceCode}
import org.scalatest.funsuite.AnyFunSuite


class IdentifierOrKeywordLexerTest extends AnyFunSuite {
  val pos:ScannerPosition = ScannerPosition.start
  val identifierOrKeywordLexer = new IdentifierOrKeywordLexer(List(ConsoleMessageListener))
  test("should be able to recognize this keyword") {
    val code: SourceCode = SourceCode("this   ")
    val tuple = identifierOrKeywordLexer.getToken(code, pos)
    val token = tuple._1.get
    val newPosition = tuple._2
    assert(token.tokenType == LoxTokenType.THIS)
    assert(token.lexeme == "this")
    assert(newPosition.currentOffset == 4)
  }

  test("should be able to recognize thisthat as identifier") {
    val code: SourceCode = SourceCode("thisthat   ")
    val tuple = identifierOrKeywordLexer.getToken(code, pos)
    val token = tuple._1.get
    val newPosition = tuple._2
    assert(token.tokenType == LoxTokenType.IDENTIFIER)
    assert(token.lexeme == "thisthat")
    assert(newPosition.currentOffset == 8)
  }
}
