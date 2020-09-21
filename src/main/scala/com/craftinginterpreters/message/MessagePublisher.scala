package com.craftinginterpreters.message

trait MessagePublisher {

  val messageListeners:List[MessageListener]

  def sendMessage(message: Message):Unit = messageListeners.foreach(m => m.receiveMessage(message))

  def report(line: Int, where:String, text:String):Unit = {
    sendMessage(Message("""at line %s in %s %s""".format(line + 1, where, text)))
  }
  def error(line:Int, message: String): Unit = report(line, "", message)

}
