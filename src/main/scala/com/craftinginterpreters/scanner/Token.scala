package com.craftinginterpreters.scanner

case class Token(tokenType: TokenType, lexeme:String, literal:Object, line: Int, position:Int ) {

}
