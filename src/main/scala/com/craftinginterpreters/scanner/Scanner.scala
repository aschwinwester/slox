package com.craftinginterpreters.scanner

trait Scanner {

  def getChar(sourceCode: SourceCode, currentIndex:Int):Char = sourceCode.charAt(currentIndex)
  def isAtEnd(offset:Int, sourceCode: SourceCode): Boolean = sourceCode.isAtEnd(offset)

  def isDigit(c: Char):Boolean = c >= '0' && c <= '9'

  def isSlash(c: Char):Boolean = c.==('/')

}
