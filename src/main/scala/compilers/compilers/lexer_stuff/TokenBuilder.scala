package compilers.compilers.lexer_stuff

import scala.collection.mutable

case class TokenBuilder() {
  private var lastAcceptedState:Option[ParseState] = None
  private val tokens = mutable.ListBuffer[Token]()
  private val lexemSoFar = new StringBuilder

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

  def finish(): List[Token] = {
    if(hasNotPunctuation)
      finishToken()
    tokens.toList
  }

  private def finishToken(): Unit = {
    tokens.append(new Token(lexemSoFar.toString, lastAcceptedState.get.tokenClass.get))
    lexemSoFar.clear()
  }

  private def addToLexem(next: Char) =
    lexemSoFar.append(next)

  private def hasNotPunctuation: Boolean =
    lastAcceptedState.isDefined && !lastAcceptedState.get.isPunctuation

  private def update(currentState: ParseState): Unit =
    lastAcceptedState = Some(currentState)

}
