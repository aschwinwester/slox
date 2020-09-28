package com.craftinginterpreters.scanner

case class SourceCode(source:String) {

  def length:Int = source.length

  def text(from:Int, to:Int):String = source.substring(from, to)

  def charAt(index:Int): Char = source.charAt(index)

  def isAtEnd(offset:Int): Boolean = offset >= source.length

  def peek(currentOffset:Int): Char = if (isAtEnd(currentOffset)) '\u0000' else charAt(currentOffset)

}
