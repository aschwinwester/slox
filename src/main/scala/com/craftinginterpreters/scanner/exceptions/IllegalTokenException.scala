package com.craftinginterpreters.scanner.exceptions

case class IllegalTokenException(message:String, pos:Int, line:Int) extends Exception(message) {

}
