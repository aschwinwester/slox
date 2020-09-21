package com.craftinginterpreters.message

object ConsoleMessageListener extends MessageListener {
  override def receiveMessage(message: Message): Unit = println(message.text)
}
