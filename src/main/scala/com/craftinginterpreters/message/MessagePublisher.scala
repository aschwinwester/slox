package com.craftinginterpreters.message

import scala.collection.mutable.ListBuffer

trait MessagePublisher {

  val messageListeners:ListBuffer[MessageListener] = new ListBuffer[MessageListener]

  def addMessageListener(messageListener: MessageListener): Unit = messageListeners += messageListener

  def removeMessageListener(messageListener: MessageListener): Unit = messageListeners -= messageListener

  def sendMessage(message: Message):Unit = messageListeners.foreach(m => m.receiveMessage(message))
}
