package compilers.compilers.lexer_stuff

import scala.collection.mutable

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

  private def parseNonEmptyInput(sourceCode:Iterator[Char]):List[Token] = {
    var currentState:ParseState = StartState
    val data = new ParseData(None, mutable.ListBuffer[Token](), new StringBuilder)
    while (sourceCode.hasNext) {
      val next = sourceCode.next()
      val nexState = currentState.parse(next, data)
      if(nexState.tokenClass.orNull == WHITESPACE && data.lastAcceptedState.isDefined) {
        if(data.lastAcceptedState.get.tokenClass.orNull != WHITESPACE) {
          data.tokens.append(new Token(data.lexemSoFar.toString, data.lastAcceptedState.get.tokenClass.get))
          data.lexemSoFar.clear()
        }
      }
      if(nexState.isFinal) {
        data.lastAcceptedState = Some(nexState)
      }
      currentState = nexState
      if(data.lastAcceptedState.get.tokenClass.orNull != WHITESPACE)
        data.lexemSoFar.append(next)
    }
    if(currentState.tokenClass.isDefined && currentState.tokenClass.orNull != WHITESPACE) {
      data.tokens.append(new Token(data.lexemSoFar.toString, currentState.tokenClass.get))
    }
    data.tokens.toList
  }
}

case class ParseData(var lastAcceptedState:Option[ParseState], tokens:mutable.ListBuffer[Token], lexemSoFar:StringBuilder)

trait ParseState {
  def parse(input:Char,parseState:ParseData): ParseState = {
    nextState(input)
  }

  def nextState(input:Char): ParseState

  def isFinal:Boolean

  def tokenClass: Option[TokenClass]
}

// start state 1
// od digit go to 7 (firstLetterOfNumber)
// on character go to state 4 (start of identifier)
// on whitespace => 12 (whitespace)
// on other => state 13 (error)
case object StartState extends ParseState {

  override def isFinal:Boolean = false
  def tokenClass: Option[TokenClass] = None

  override def nextState(input:Char): ParseState = {
    if(input >= '0' && input <= '9') StateNumber
    else if(input >= 'a' && input <= 'z') StateIdentifier
    else if(input == '\t' || input == '\n' || input == ' ' || input == '\r') StateWhiteSpace
    else StateError
  }
}

// state 4 ID
// od digit loop (continueIdentifier)
// on character loop (continueIdentifier)
// on whitespace => 12 (whitespace)
// on other => state 13 (error)
case object StateIdentifier extends ParseState {

  override def isFinal:Boolean = true

  override def tokenClass = Some(ID)

  override def nextState(input:Char): ParseState = {
    if(input >= '0' && input <= '9') StateIdentifier
    else if(input >= 'a' && input <= 'z') StateIdentifier
    else if(input == '\t' || input == '\n' || input == ' ' || input == '\r') StateWhiteSpace
    else StateError
  }
}

// state 7 NUM
// on digit loop (continueInteger)
// on whitespace => 12 (whitespace)
// on other => state 13 (error)
case object StateNumber extends ParseState {

  override def isFinal:Boolean = true

  override def tokenClass = Some(NUM)

  override def nextState(input:Char): ParseState = {
    if(input >= '0' && input <= '9') StateNumber
    else if(input == '\t' || input == '\n' || input == ' ' || input == '\r') StateWhiteSpace
    else StateError
  }

}

// state 12 whitespace
// od digit go to 7 (firstLetterOfNumber)
// on character go to state 4 (start of identifier)
// on whitespace => loop (continueWhitespace)
// on other => state 13 (error)
case object StateWhiteSpace extends ParseState {

  override def tokenClass = Some(WHITESPACE)

  override def isFinal:Boolean = true

  override def nextState(input:Char): ParseState =
    if(input >= '0' && input <= '9') StateNumber
    else if(input >= 'a' && input <= 'z') StateIdentifier
    else if(input == '\t' || input == '\n' || input == ' ' || input == '\r') StateWhiteSpace
    else StateError
}

case object StateError extends ParseState {

  override def tokenClass = Some(ERROR)

  override def isFinal:Boolean = true

  override def nextState(input:Char): ParseState =
    if(input == '\t' || input == '\n' || input == ' ' || input == '\r') StateWhiteSpace
    else StateError
}