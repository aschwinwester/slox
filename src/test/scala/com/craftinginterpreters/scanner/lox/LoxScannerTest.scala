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
}
