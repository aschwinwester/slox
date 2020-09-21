package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.ConsoleMessageListener
import com.craftinginterpreters.scanner.{ScannerPosition, SourceCode, Token}
import org.scalatest.funsuite.AnyFunSuite

class SymbolLexerTest extends AnyFunSuite {
  val symbolLexer = new SymbolLexer(List(ConsoleMessageListener))
  val pos:ScannerPosition = ScannerPosition.start
  test("Should find number >=") {
    val s:SourceCode = SourceCode(">=")
    val (l, newPos) = symbolLexer.getToken(s, pos)
    val token:Token = l.get
    assert(token.tokenType == LoxTokenType.GREATER_EQUAL)
    assert(token.lexeme == ">=")
    assert(newPos.currentOffset == 2)
  }

  test("Should find number >") {
    val s:SourceCode = SourceCode("> 5")
    val (l, newPos) = symbolLexer.getToken(s, pos)
    val token:Token = l.get
    assert(token.tokenType == LoxTokenType.GREATER)
    assert(token.lexeme == ">")
    assert(newPos.currentOffset == 1)
  }

}
