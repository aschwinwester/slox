package com.craftinginterpreters.scanner.lox

import com.craftinginterpreters.message.ConsoleMessageListener
import com.craftinginterpreters.scanner.Token
import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success, Try}

class LoxScannerTest extends AnyFunSuite {

  test("Should match string") {
    val tlt:Try[List[Token]] = new LoxScanner(List(ConsoleMessageListener)).scanFile("teststring.lox")
    tlt match {
      case Success(lt) => lt.foreach(println)
      case Failure(exception) => print(exception)
    }
  }

}
