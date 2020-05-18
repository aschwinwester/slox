package com.craftinginterpreters.scanner

import com.craftinginterpreters.scanner.lox.TokenType.TokenType

case class Token(tokenType: TokenType, lexeme:String, literal:Object, line: Int, position:Int ) {

}
