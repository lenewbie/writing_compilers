package compilers.compilers.lexer_stuff

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
