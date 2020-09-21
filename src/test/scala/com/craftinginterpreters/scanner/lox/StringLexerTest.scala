package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.ConsoleMessageListener
import com.craftinginterpreters.scanner.{ScannerPosition, SourceCode, Token}
import org.scalatest.funsuite.AnyFunSuite

import scala.util.matching.Regex

class StringLexerTest extends AnyFunSuite {

  private val tok:StringLexer = new StringLexer(List(ConsoleMessageListener))
  private val pos = ScannerPosition.start
  test("Should capture string") {
    val s:SourceCode = SourceCode(""""text with spaces" """)
    val (t, newPos) = tok.getToken(s, pos)
    val token:Token = t.get
    assert(token.tokenType == LoxTokenType.STRING)
    assert(token.lexeme == "text with spaces")
    assert(token.position == 0)
    assert(newPos.line == 0)
    assert(newPos.startOffset == 0)
    assert(newPos.currentOffset == 18)

  }

  test("Should capture this is a string") {
    val s:SourceCode = SourceCode(""""this is a string"""")
    val (t, newPos) = tok.getToken(s, pos)
    val token:Token = t.get
    assert(token.tokenType == LoxTokenType.STRING)
    assert(token.lexeme == "this is a string")
    assert(token.position == 0)
    assert(newPos.line == 0)
    assert(newPos.startOffset == 0)
    assert(newPos.currentOffset == 18)

  }

  test("Should capture string simple string") {
    val s:SourceCode = SourceCode(""""a" """)
    val (t, newPos) = tok.getToken(s, pos)
    val token:Token = t.get
    assert(token.tokenType == LoxTokenType.STRING)
    assert(token.lexeme == "a")
    assert(token.position == 0)
    assert(newPos.line == 0)
    assert(newPos.startOffset == 0)
    assert(newPos.currentOffset == 3)

  }

  test("Should not find end of string") {
    val s:SourceCode = SourceCode(""""text with no end """)
    val (t, newPos) = tok.getToken(s, pos)
    assert(t.isFailure)
    assert(newPos.line == 0)
    assert(newPos.startOffset == 0)

  }

  test("Test regex") {
    val numberPattern:Regex = "[0-9]".r
    val keywordPattern:Regex = "[a-zA-Z_]".r
    val c:Char = '_'
    val m = c match {
      case numberPattern() => true
      case keywordPattern() => true
      case _ => false
    }
    assert(m)


  }


}
