package com.craftinginterpreters.scanner

object ScannerPosition {
  def start = ScannerPosition(0,0,0)
}
case class ScannerPosition(currentOffset:Int, startOffset:Int, line:Int) {
  def nextLine:ScannerPosition = ScannerPosition(currentOffset, startOffset, line + 1)

  def nextPosition = ScannerPosition(currentOffset + 1, startOffset, line)

  def skipPosition = ScannerPosition(currentOffset + 1, currentOffset + 1, line)

  override def toString: String = "line %s start %s current offset %s".format(line + 1, currentOffset, startOffset)


}
