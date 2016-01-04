package compilers.compilers.lexer_stuff

/**
  * Example Lexer that can parse source code.
  *
  * Language in source code can be defined with following
  * regular 
  * ID [a-z][a-z0-9]*
  * NUM [0-9]+
  * no token (" "|"\t"|"\n"|"\r")+
  */
class SampleLexer {

  def parse(sourceCode:Iterator[Char]): List[Token] = {
    if(sourceCode.isEmpty) List[Token]()
    else parseNonEmptyInput(sourceCode)
  }

  private def parseNonEmptyInput(sourceCode:Iterator[Char]) = {
    val builder = new TokenBuilder()
    var currentState:ParseState = StartState
    sourceCode.foreach( next =>
      currentState = currentState.processInput(next, builder)
    )
    builder.finish()
  }
}