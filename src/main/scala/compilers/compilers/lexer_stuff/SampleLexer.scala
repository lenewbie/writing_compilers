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
    val builder = new TokenBuilder()
    sourceCode.foreach( next =>
      currentState = currentState.processInput(next, builder)
    )
    builder.toList
  }
}

case class TokenBuilder() {

  def onFinalState(next: Char, state: FinalParseState): Unit = {
    update(state)
    if (hasNotPunctuation)
      addToLexem(next)
  }

  def onNext(next: Char): Unit =
    if (hasNotPunctuation)
      addToLexem(next)

  def onPunctuation(next: Char, state:ParseState): Any = {
    if (hasNotPunctuation)
      finishToken()
    update(state)
    if (hasNotPunctuation)
      addToLexem(next)
  }

  private var lastAcceptedState:Option[ParseState] = None
  private val tokens = mutable.ListBuffer[Token]()
  private val lexemSoFar = new StringBuilder

  def finishToken(): Unit = {
    tokens.append(new Token(lexemSoFar.toString, lastAcceptedState.get.tokenClass.get))
    lexemSoFar.clear()
  }

  def addToLexem(next: Char) =
    lexemSoFar.append(next)

  def hasNotPunctuation: Boolean =
    lastAcceptedState.isDefined && !lastAcceptedState.get.isPunctuation

  def update(currentState: ParseState): Unit =
    lastAcceptedState = Some(currentState)

  def toList = {
    if(hasNotPunctuation)
      finishToken()
    tokens.toList
  }

}

trait ParseState {

  def tokenClass: Option[TokenClass]

  def isPunctuation: Boolean

  def processInput(input:Char, builder: TokenBuilder): ParseState = {
    val nextState = getNextState(input)
    nextState.updateBuilder(builder, input)
    nextState
  }

  protected def updateBuilder(builder: TokenBuilder, next: Char): Unit

  protected def getNextState(input:Char): ParseState
}

trait FinalParseState extends ParseState {

  override def isPunctuation: Boolean = false

  override def updateBuilder(builder: TokenBuilder, next: Char): Unit =
    builder.onFinalState(next, this)
}

trait NotFinalParseState extends ParseState {

  override def tokenClass: Option[TokenClass] = None

  override def isPunctuation: Boolean = false

  override def updateBuilder(builder: TokenBuilder, next: Char): Unit =
    builder.onNext(next)
}

trait PunctuationParseState extends FinalParseState {

  override def isPunctuation: Boolean = true

  override def updateBuilder(builder: TokenBuilder, next: Char): Unit =
    builder.onPunctuation(next, this)
}

// start state 1
// od digit go to 7 (firstLetterOfNumber)
// on character go to state 4 (start of identifier)
// on whitespace => 12 (whitespace)
// on other => state 13 (error)
case object StartState extends NotFinalParseState {

  override def getNextState(input:Char): ParseState = {
    if(input >= '0' && input <= '9') NumberState
    else if(input >= 'a' && input <= 'z') IdentifierState
    else if(input == '\t' || input == '\n' || input == ' ' || input == '\r') WhiteSpaceState
    else ErrorState
  }
}

// state 4 ID
// od digit loop (continueIdentifier)
// on character loop (continueIdentifier)
// on whitespace => 12 (whitespace)
// on other => state 13 (error)
case object IdentifierState extends FinalParseState {

  override def tokenClass = Some(ID)

  override def getNextState(input:Char): ParseState = {
    if(input >= '0' && input <= '9') IdentifierState
    else if(input >= 'a' && input <= 'z') IdentifierState
    else if(input == '\t' || input == '\n' || input == ' ' || input == '\r') WhiteSpaceState
    else ErrorState
  }
}

// state 7 NUM
// on digit loop (continueInteger)
// on whitespace => 12 (whitespace)
// on other => state 13 (error)
case object NumberState extends FinalParseState {

  override def tokenClass = Some(NUM)

  override def getNextState(input:Char): ParseState = {
    if(input >= '0' && input <= '9') NumberState
    else if(input == '\t' || input == '\n' || input == ' ' || input == '\r') WhiteSpaceState
    else ErrorState
  }

}

// state 12 whitespace
// od digit go to 7 (firstLetterOfNumber)
// on character go to state 4 (start of identifier)
// on whitespace => loop (continueWhitespace)
// on other => state 13 (error)
case object WhiteSpaceState extends PunctuationParseState {

  override def tokenClass = Some(WHITESPACE)

  override def getNextState(input:Char): ParseState =
    if(input >= '0' && input <= '9') NumberState
    else if(input >= 'a' && input <= 'z') IdentifierState
    else if(input == '\t' || input == '\n' || input == ' ' || input == '\r') WhiteSpaceState
    else ErrorState
}

case object ErrorState extends FinalParseState {

  override def tokenClass = Some(ERROR)

  override def getNextState(input:Char): ParseState =
    if(input == '\t' || input == '\n' || input == ' ' || input == '\r') WhiteSpaceState
    else ErrorState
}