package com.craftinginterpreters.scanner

import scala.util.matching.Regex

trait Lexer extends Scanner {

  def pattern:Regex

  def matches(c:Char, regex:Regex):Boolean = regex.findFirstIn(c.toString) match {
    case Some(_) => true
    case None => false
  }

  def isScanningDone(code:SourceCode, pos:ScannerPosition, c:Char, pattern:Regex) =
    isAtEnd(pos.currentOffset, code) || !matches(c, pattern)
}
