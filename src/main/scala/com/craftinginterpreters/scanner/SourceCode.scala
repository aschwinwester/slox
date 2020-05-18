package com.craftinginterpreters.scanner

class SourceCode(val source:String) {

  def length:Int = source.length

  def text(from:Int, to:Int):String = source.substring(from, to)

  def charAt(index:Int) = source.charAt(index)
}
