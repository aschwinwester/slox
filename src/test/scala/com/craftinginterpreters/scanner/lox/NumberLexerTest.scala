package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.ConsoleMessageListener
import com.craftinginterpreters.scanner.{ScannerPosition, SourceCode, Token}
import org.scalatest.funsuite.AnyFunSuite

class NumberLexerTest extends AnyFunSuite {
  val numberLexer = new NumberLexer(List(ConsoleMessageListener))
  val pos:ScannerPosition = ScannerPosition.start
  test("Should find number 56") {
    val s:SourceCode = SourceCode("56")
    val (l, newPos) = numberLexer.getToken(s, pos)
    val token:Token = l.get
    assert(token.tokenType == LoxTokenType.NUMBER)
    assert(token.lexeme == "56")
    assert(newPos.currentOffset == 2)
  }

  test("Should find number 56 within text") {
    val s:SourceCode = SourceCode("56 + ")
    val (l, newPos) = numberLexer.getToken(s, pos)
    val token:Token = l.get
    assert(token.tokenType == LoxTokenType.NUMBER)
    assert(token.lexeme == "56")
    assert(newPos.currentOffset == 2)
  }

}
