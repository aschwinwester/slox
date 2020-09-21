import com.craftinginterpreters.message.{Message, MessageListener}
import com.craftinginterpreters.scanner.lox.LoxScanner

import scala.util.{Failure, Success}

object Main extends App with MessageListener {
  println("slox started")

  if (args.isEmpty) {
    println("no file")
  } else {
    val fileName:String = args(0)
    println("parsing file " + fileName)
    val ml:List[MessageListener] = List(this)
    val scanner = new LoxScanner(ml)

    val tryTokens = scanner.scanFile("teststring.lox")
    tryTokens match {
      case Success(tokens) => tokens.foreach(t => println(t.toString))
      case Failure(exception) => System.err.println(exception)
    }
  }

  def receiveMessage(message: Message)= println(message.text)

}

