package compilers.compilers.lexer_stuff

sealed trait TokenClass

case object NUM extends TokenClass
case object ID extends TokenClass
case object WHITESPACE extends TokenClass
case object IF extends TokenClass
case object ERROR extends TokenClass
