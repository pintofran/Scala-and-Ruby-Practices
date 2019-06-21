import ParserCombinator.{CharResult, ParseError, Parser}
import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Success, Try}

class ParserCombinatorTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe expectedResult
  }

  "Parsers" - {
    "AnyChar Parser" - {

      "Fails when fed empty text" in {
        val parseador :Parser = ParserCombinator.anyChar
        assertThrows[ParseError] (parseador.parse("").get)
      }

      "Successful when parses the first char when fed with text" in {
        assert(ParserCombinator.anyChar.parser("Some Test Text") == Try(CharResult('S', "ome Test Text")))
      }

      "Successful when parses the first char when fed with only one char text" in {
        assert(ParserCombinator.anyChar.parser("A") == Try(CharResult('A',"")))
      }

    }

    "Char Parser" - {

      "Fails when fed empty text" in {
        val parseador :Parser = ParserCombinator.char
        assertThrows[ParseError] (parseador.parse("").get)
      }

      "Fails when parses another character than expected" in {
        assert(ParserCombinator.char.parser('A', "Some Test Text") == Try(CharResult('S', "ome Test Text")))
      }

      "Successful when parses the expected character of the given text" in {
        assert(ParserCombinator.char.parser('S', "Some Test Text") == Try(CharResult('S', "ome Test Text")))
      }

    }

    "Void Parser" - {

      "Fails when fed empty text" in {
        val parseador :Parser = ParserCombinator.void
        assertThrows[ParseError] (parseador.parse("").get)
      }

      "Successful when parses as Unit the first char of the given text" in {
        assert(ParserCombinator.void.parser("Some Test Text") == Try(UnitResult("ome Test Text")))
      }

      "Successful when parses as Unit the first char when fed with only one char text" in {
        assert(ParserCombinator.void.parser("A") == Try(UnitResult("")))
      }

    }

    "Letter Parser" - {

      "Fails when fed empty text" in {
        val parseador :Parser = ParserCombinator.letter
        assertThrows[ParseError] (parseador.parse("").get)
      }

      "Fails when parses the first char as digit with mixed fed text" in {
        val parseador :Parser = ParserCombinator.letter
        assertThrows[ParseError] (parseador.parse("5ome Test Text").get)
      }

      "Fails when parses the first char as special character with mixed fed text" in {
        val parseador :Parser = ParserCombinator.letter
        assertThrows[ParseError] (parseador.parse("$0M3 73$7 73X7").get)
      }

      "Successful when parses the first char as letter of the given text" in {
        assert(ParserCombinator.letter.parser("Some Test Text") == Try(CharResult('S', "ome Test Text")))
      }

      "Successful when parses the first char when fed with only one letter char text" in {
        assert(ParserCombinator.letter.parser("A") == Try(CharResult('A', "")))
      }

      "Successful when parses the first char as letter with mixed fed text" in {
        assert(ParserCombinator.letter.parser("S0M3 73$7 73X7") == Try(CharResult('S', "0M3 73$7 73X7")))
      }

    }

    "Digit Parser" - {

      "Fails when fed empty text" in {
        val parseador :Parser = ParserCombinator.digit
        assertThrows[ParseError] (parseador.parse("").get)
      }

      "Fails when parses the first char as letter with mixed fed text" in {
        val parseador :Parser = ParserCombinator.digit
        assertThrows[ParseError] (parseador.parse("S0M3 73$7 73X7").get)
      }

      "Fails when parses the first char as special character with mixed fed text" in {
        val parseador :Parser = ParserCombinator.digit
        assertThrows[ParseError] (parseador.parse("$0M3 73$7 73X7").get)
      }

      "Successful when parses the first char as digit of the given text" in {
        assert(ParserCombinator.digit.parser("1234") == Try(CharResult('1', "234")))
      }

      "Successful when parses the first char when fed with only one digit char text" in {
        assert(ParserCombinator.digit.parser("1") == Try(CharResult('1', "")))
      }

      "Successful when parses the first char as digit with mixed fed text" in {
        assert(ParserCombinator.digit.parser("5ome Test Text") == Try(CharResult('5', "ome Test Text")))
      }

    }

    "AlphaNum Parser" - {

      "Fails when fed empty text" in {
        val parseador :Parser = ParserCombinator.alphaNum
        assertThrows[ParseError] (parseador.parse("").get)
      }

      "Fails when parses the first char as special character with mixed fed text" in {
        val parseador :Parser = ParserCombinator.alphaNum
        assertThrows[ParseError] (parseador.parse("$0M3 73$7 73X7").get)
      }

      "Successful when parses the first char as digit of the given text" in {
        assert(ParserCombinator.alphaNum.parser("1234") == Try(CharResult('1', "234")))
      }

      "Successful when parses the first char when fed with only one digit char text" in {
        assert(ParserCombinator.alphaNum.parser("1") == Try(CharResult('1', "")))
      }

      "Successful when parses the first char as digit with mixed fed text" in {
        assert(ParserCombinator.alphaNum.parser("5ome Test Text") == Try(CharResult('5', "ome Test Text")))
      }

      "Successful when parses the first char as letter of the given text" in {
        assert(ParserCombinator.alphaNum.parser("Some Test Text") == Try(CharResult('S', "ome Test Text")))
      }

      "Successful when parses the first char when fed with only one letter char text" in {
        assert(ParserCombinator.alphaNum.parser("A") == Try(CharResult('A', "")))
      }

      "Successful when parses the first char as letter with mixed fed text" in {
        assert(ParserCombinator.alphaNum.parser("S0M3 73$7 73X7") == Try(CharResult('S', "0M3 73$7 73X7")))
      }

    }

    "String Parser" - {

      "Fails when fed empty text" in {
        val parseador :Parser = ParserCombinator.string
        assertThrows[ParseError] (parseador.parse("").get)
      }

      "Fails when parses another string than expected" in {
        val parseador :Parser = ParserCombinator.string
        assertThrows[ParseError] (parseador.parse("Some Failure Text", "Some Test Text").get)
      }

      "Successful when parses the expected string of a exact given text" in {
        assert(ParserCombinator.string.parser("Some", "Some Test Text") == Try(StringResult("Some", " Test Text")))
      }

      "Successful when parses the expected string of the given text" in {
        assert(ParserCombinator.string.parser("Some Test Text", "Some Test Text") == Try(StringResult("Some Test Text", "")))
      }

    }

  }

  "Combinators" - {

    "OR <|>" - {   
     val aob: Parser = ParserCombinator.char('a') <|> ParserCombinator.char('b')

      "Fails when both parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.aob.parser("que tal").get
        }
      }

      "Successful when first parser is successful" in {
        assert(ParserCombinator.aob.parser("arbol") == Try(CharResult('a',"rbol")))
      }

      "Successful when first parser fails but second parser is successful" in {
        assert(ParserCombinator.aob.parser("bort") == Try(CharResult('b',"ort")))
      }
    }

    "CONCAT <>" - {
     val holaMundo = ParserCombinator.string("hola") <> ParserCombinator.string("mundo")

      "Fails when first parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundo.parser("holimundo").get
        }
      }

      "Fails when second parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundo.parser("holamundu").get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.holaMundo.parser("holamundo") == Try(StringResult("holamundo","")))
      }

      "Successful when both parsers success with remaining text" in {
        assert(ParserCombinator.holaMundo.parser("holamundo!") == Try(StringResult("holamundo","!")))
      }

    }

    "Rightmost ~>" - {
     val holaMundo = ParserCombinator.string("hola") ~> ParserCombinator.string("mundo")

      "Fails when first parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundo.parser("holimundo").get
        }
      }

      "Fails when second parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundo.parser("holamundu").get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.holaMundo.parser("holamundo") == Try(StringResult("mundo","")))
      }

      "Successful when both parsers success with remaining text" in {
        assert(ParserCombinator.holaMundo.parser("holamundo!") == Try(StringResult("mundo","!")))
      }

    }

    "Leftmost <~" - {
     val holaMundo = ParserCombinator.string("hola") ~> ParserCombinator.string("mundo")

      "Fails when first parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundo.parser("holimundo").get
        }
      }

      "Fails when second parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundo.parser("holamundu").get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.holaMundo.parser("holamundo") == Try(StringResult("hola","mundo")))
      }

      "Successful when both parsers success with remaining text" in {
        assert(ParserCombinator.holaMundo.parser("holamundo!") == Try(StringResult("hola","mundo!")))
      }

    }

    "Separated-by sepBy" - {
     val inicialesNombresConBarras = ParserCombinator.letter.sepBy(char('/'))

      "Fails when separator parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.inicialesNombresConBarras.parser("Juan-Perez").get
        }
      }

      "Fails when container parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.inicialesNombresConBarras.parser("Juan/1erez").get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.inicialesNombresConBarras.parser("Juan/Perez") == [Try(CharResult('J',"uan")), Try(CharResult('P',"erez")) ])
      }

      "Successful with one iterations" in {
        assert(ParserCombinator.inicialesNombresConBarras.parser("Juan") == [Try(CharResult('J',"uan"))])
      }

      "Successful with multiple iterations" in {
        assert(ParserCombinator.inicialesNombresConBarras.parser("Juan/Martin/Perez/Dias") == [Try(CharResult('J',"uan")), Try(CharResult('M',"artin")), Try(CharResult('P',"erez")), Try(CharResult('D',"ias")) ])
      }
    }

  }

  "Operations" - {

  }

  "Interation" - {

    "Caso práctico: Musiquita" - {

    }
    
  }

}