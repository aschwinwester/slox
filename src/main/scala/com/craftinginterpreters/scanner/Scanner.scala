package com.craftinginterpreters.scanner

trait Scanner {

  var currentOffset:Int = 0
  var startOffset:Int = 0
  var line:Int = 0
  def getChar(sourceCode: SourceCode, currentIndex:Int):Char = sourceCode.charAt(currentIndex)
  def isAtEnd(offset:Int, sourceCode: SourceCode): Boolean =  offset >= sourceCode.length

  def isDigit(c: Char):Boolean = c >= '0' && c <= '9'

}
