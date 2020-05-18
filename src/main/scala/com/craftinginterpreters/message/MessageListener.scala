package com.craftinginterpreters.message

trait MessageListener {

  def receiveMessage(message: Message):Unit

}
