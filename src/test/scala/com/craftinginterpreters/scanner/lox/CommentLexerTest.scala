package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.ConsoleMessageListener
import com.craftinginterpreters.scanner.{ScannerPosition, SourceCode, Token}
import org.scalatest.funsuite.AnyFunSuite

class CommentLexerTest extends AnyFunSuite {

  private val tok:CommentLexer = new CommentLexer(List(ConsoleMessageListener))
  private val pos = ScannerPosition.start
  test("Should capture comment") {
    val s:SourceCode = SourceCode("// this is a comment")
    val (t, newPos) = tok.getToken(s, pos)
    val token:Token = t.get
    assert(token.tokenType == LoxTokenType.COMMENT)
    assert(token.lexeme == " this is a comment")
    assert(token.position == 0)
    assert(newPos.line == 0)
    assert(newPos.startOffset == 0)
    assert(newPos.currentOffset == 20)
  }

  test("Should capture comment with newline") {
    val s:SourceCode = SourceCode("// this is a comment\nHallo")
    val (t, newPos) = tok.getToken(s, pos)
    val token:Token = t.get
    assert(token.tokenType == LoxTokenType.COMMENT)
    assert(token.lexeme == " this is a comment")
    assert(token.position == 0)
    assert(newPos.line == 0)
    assert(newPos.startOffset == 0)
    assert(newPos.currentOffset == 20)
  }
}
