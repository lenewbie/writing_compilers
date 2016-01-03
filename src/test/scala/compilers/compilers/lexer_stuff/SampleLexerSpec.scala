package compilers.compilers.lexer_stuff

import org.scalatest.{MustMatchers, FunSpec}

class SampleLexerSpec extends FunSpec with MustMatchers {

  val lexer:SampleLexer = new SampleLexer()

  describe("SampleLexer") {
    it("accepts empty string") {
      assertParse("")
    }

    it("accepts single digit integer") {
      assertParse("0", Token("0", NUM))
    }

    it("accepts multiple digit integer") {
      assertParse("429", Token("429", NUM))
    }

    it("accepts identifier") {
      assertParse("az", Token("az", ID))
    }

    it("accepts multiple character identifier") {
      assertParse("foo", Token("foo", ID))
    }

    it("accepts identifier with digit") {
      assertParse("adez90", Token("adez90", ID))
    }

    it("do not report space") {
      assertParse(" ")
    }

    it("do not report multiple spaces") {
      assertParse("  ")
    }

    it("do not report space before token") {
      assertParse(" foo", Token("foo", ID))
    }

    it("do not report space after integer") {
      assertParse("1 ", Token("1", NUM))
    }

    it("do not report space after identifier") {
      assertParse("foo ", Token("foo", ID))
    }

    it("do not report space between tokens") {
      assertParse("foo 1", Token("foo", ID), Token("1", NUM))
    }

    it("do not report tab") {
      assertParse("\t")
    }

    it("do not report linux new line") {
      assertParse("\n")
    }

    it("do not report windows new line") {
      assertParse("\r\n")
    }

    it("recognize some complicated string of new lines identifiers and numbers") {
      assertParse("\r\nfoo1 foo2 12 4 8   9 stuff ",
        Token("foo1", ID),
        Token("foo2", ID),
        Token("12", NUM),
        Token("4", NUM),
        Token("8", NUM),
        Token("9", NUM),
        Token("stuff", ID))
    }

    it("recognizes error") {
      assertParse("1foo", Token("1foo", ERROR))
    }

    it("recognizes error and number") {
      assertParse("1foo 10", Token("1foo", ERROR), Token("10", NUM))
    }

    it("recognizes identifier error and number") {
      assertParse(" x 9i 10 y ", Token("x", ID), Token("9i", ERROR), Token("10", NUM), Token("y", ID))
    }
  }

  private def assertParse(inputString: String, token: Token*): Unit = {
    val input = inputString.iterator
    val expectedOutput = token.toList
    lexer.parse(input) mustBe expectedOutput
  }
}
