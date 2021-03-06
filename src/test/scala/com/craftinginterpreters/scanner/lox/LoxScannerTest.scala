package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.ConsoleMessageListener
import com.craftinginterpreters.scanner.Token
import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success, Try}

class LoxScannerTest extends AnyFunSuite {

  test("Should match string") {
    val tlt:Try[List[Token]] = new LoxScanner(List(ConsoleMessageListener)).scanFile("teststring.lox")
    tlt match {
      case Success(lt) => assert(lt.exists(t => t.tokenType == LoxTokenType.STRING))
      case Failure(_) => assert(false)
    }
  }

  test("Should match if statement") {
    val tlt:Try[List[Token]] = new LoxScanner(List(ConsoleMessageListener)).scanFile("test_if_statement.lox")
    tlt match {
      case Success(lt) => assert(lt.exists(t => t.tokenType == LoxTokenType.IF))
      case Failure(_) => assert(false)
    }
  }
  test("Should match function") {
    val tlt:Try[List[Token]] = new LoxScanner(List(ConsoleMessageListener)).scanFile("test_function.lox")
    tlt match {
      case Success(lt) =>
        assert(lt.exists(t => t.tokenType == LoxTokenType.FUN))
        assert(lt.exists(t => t.tokenType == LoxTokenType.IF))
        assert(lt.exists(t => t.tokenType == LoxTokenType.PRINT))
        assert(lt.exists(t => t.tokenType == LoxTokenType.LEFT_BRACE))

      case Failure(_) => assert(false)
    }
  }

  test("Should handle comment") {
    val tlt:Try[List[Token]] = new LoxScanner(List(ConsoleMessageListener)).scanFile("test_comment.lox")
    tlt match {
      case Success(lt) =>
        assert(lt.exists(t => t.tokenType == LoxTokenType.IF && t.line == 1))
        assert(lt.exists(t => t.tokenType == LoxTokenType.LEFT_BRACE && t.line == 1))
        assert(!lt.exists(t => t.tokenType == LoxTokenType.COMMENT && t.line == 0))
        assert(lt.exists(t => t.tokenType == LoxTokenType.RIGHT_BRACE && t.line == 3))

      case Failure(_) => assert(false)
    }
  }

}
