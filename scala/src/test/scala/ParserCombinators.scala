import ParserCombinator._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Success, Try}

class ParserCombinatorTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe expectedResult
  }

  "Parsers" - {
    "AnyChar Parser" - {

      "Fails when fed empty text" in {
        val parseador = ParserCombinator.anyChar
        assertThrows[ParseError](parseador.parse("").result.get)
      }

      "Successful when parses the first char when fed with text" in {
        assert(ParserCombinator.anyChar.parse("Some Test Text").result.get == ('S', "ome Test Text"))
      }

      "Successful when parses the first char when fed with only one char text" in {
        assert(ParserCombinator.anyChar.parse("A").result.get == ('A', ""))
      }

    }

    "Char Parser" - {

      "Fails when fed empty text" in {
        val parseador = ParserCombinator.anyChar
        assertThrows[ParseError](parseador.parse("").result.get)
      }

      "Fails when parses another character than expected" in {
        assertThrows[ParseError] {
          char('A').parse("Some Test Text").result.get
        }
      }

      "Successful when parses the expected character of the given text" in {
        assert(ParserCombinator.char('S').parse("Some Test Text").result.get == ('S', "ome Test Text"))
      }

    }

    "Void Parser" - {

      "Fails when fed empty text" in {
        val parseador = ParserCombinator.void
        assertThrows[ParseError](parseador.parse("").result.get)
      }

      "Successful when parses as Unit the first char of the given text" in {
        assert(ParserCombinator.void.parser("Some Test Text").result.get == ((), "ome Test Text"))
      }

      "Successful when parses as Unit the first char when fed with only one char text" in {
        assert(ParserCombinator.void.parser("A").result.get == ((), ""))
      }

    }

    "Letter Parser" - {

      "Fails when fed empty text" in {
        val parseador = ParserCombinator.letter
        assertThrows[ParseError](parseador.parse("").result.get)
      }

      "Fails when parses the first char as digit with mixed fed text" in {
        val parseador = ParserCombinator.letter
        assertThrows[ParseError](parseador.parse("5ome Test Text").result.get)
      }

      "Fails when parses the first char as special character with mixed fed text" in {
        val parseador = ParserCombinator.letter
        assertThrows[ParseError](parseador.parse("$0M3 73$7 73X7").result.get)
      }

      "Successful when parses the first char as letter of the given text" in {
        assert(ParserCombinator.letter.parser("Some Test Text").result.get == ('S', "ome Test Text"))
      }

      "Successful when parses the first char when fed with only one letter char text" in {
        assert(ParserCombinator.letter.parser("A").result.get == ('A', ""))
      }

      "Successful when parses the first char as letter with mixed fed text" in {
        assert(ParserCombinator.letter.parser("S0M3 73$7 73X7").result.get == ('S', "0M3 73$7 73X7"))
      }

    }
    "Digit Parser" - {

      "Fails when fed empty text" in {
        val parseador = ParserCombinator.digit
        assertThrows[ParseError](parseador.parse("").result.get)
      }

      "Fails when parses the first char as letter with mixed fed text" in {
        val parseador = ParserCombinator.digit
        assertThrows[ParseError](parseador.parse("S0M3 73$7 73X7").result.get)
      }

      "Fails when parses the first char as special character with mixed fed text" in {
        val parseador = ParserCombinator.digit
        assertThrows[ParseError](parseador.parse("$0M3 73$7 73X7").result.get)
      }

      "Successful when parses the first char as digit of the given text" in {
        assert(ParserCombinator.digit.parser("1234").result.get == ('1', "234"))
      }

      "Successful when parses the first char when fed with only one digit char text" in {
        assert(ParserCombinator.digit.parser("1").result.get == ('1', ""))
      }

      "Successful when parses the first char as digit with mixed fed text" in {
        assert(ParserCombinator.digit.parser("5ome Test Text").result.get == ('5', "ome Test Text"))
      }

    }
    "AlphaNum Parser" - {

      "Fails when fed empty text" in {
        val parseador = ParserCombinator.alphaNum
        assertThrows[ParseError](parseador.parse("").result.get)
      }

      "Fails when parses the first char as special character with mixed fed text" in {
        val parseador = ParserCombinator.alphaNum
        assertThrows[ParseError](parseador.parse("$0M3 73$7 73X7").result.get)
      }

      "Successful when parses the first char as digit of the given text" in {
        assert(ParserCombinator.alphaNum.parser("1234").result.get == ('1', "234"))
      }

      "Successful when parses the first char when fed with only one digit char text" in {
        assert(ParserCombinator.alphaNum.parser("1").result.get == ('1', ""))
      }

      "Successful when parses the first char as digit with mixed fed text" in {
        assert(ParserCombinator.alphaNum.parser("5ome Test Text").result.get == ('5', "ome Test Text"))
      }

      "Successful when parses the first char as letter of the given text" in {
        assert(ParserCombinator.alphaNum.parser("Some Test Text").result.get == ('S', "ome Test Text"))
      }

      "Successful when parses the first char when fed with only one letter char text" in {
        assert(ParserCombinator.alphaNum.parser("A").result.get == ('A', ""))
      }

      "Successful when parses the first char as letter with mixed fed text" in {
        assert(ParserCombinator.alphaNum.parser("S0M3 73$7 73X7").result.get == ('S', "0M3 73$7 73X7"))
      }

    }

    "String Parser" - {

      "Fails when fed empty text" in {
        val parseador = ParserCombinator.string("test")
        assertThrows[ParseError](parseador.parse("").result.get)
      }

      "Fails when parses another string than expected" in {
        val parseador = ParserCombinator.string
        assertThrows[ParseError](parseador("Some Failure Text").parse("Some Test Text").result.get)
      }

      "Successful when parses the expected string of a exact given text" in {
        assert(ParserCombinator.string("Some").parse("Some Test Text").result.get == ("Some", " Test Text"))
      }

      "Successful when parses the expected string of the given text" in {
        assert(ParserCombinator.string("Some Test Text").parse("Some Test Text").result.get == ("Some Test Text", ""))
      }

    }
  }

  "Combinators" - {

    "OR <|>" - {

      "Fails when both parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.aob.parser("cdefghi").result.get
        }
      }

      "Successful when first parser is successful" in {
        assert(ParserCombinator.aob.parser("arbol").result.get == ('a', "rbol"))
      }

      "Successful when first parser fails but second parser is successful" in {
        assert(ParserCombinator.aob.parser("bort").result.get == ('b', "ort"))
      }
    }

    "CONCAT <>" - {

      "Fails when first parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoConcat.parser("holimundo").result.get
        }
      }

      "Fails when second parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoConcat.parser("holamundu").result.get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.holaMundoConcat.parser("holamundo").result.get == (("hola","mundo"),""))
      }

      "Successful when both parsers success with remaining text" in {
        assert(ParserCombinator.holaMundoConcat.parser("holamundo!").result.get == (("hola","mundo"),"!"))
      }

    }
    "Rightmost ~>" - {

      "Fails when first parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoRightmost.parser("holimundo").result.get
        }
      }

      "Fails when second parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoRightmost.parser("holamundu").result.get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.holaMundoRightmost.parser("holamundo").result.get == ("mundo",""))
      }

      "Successful when both parsers success with remaining text" in {
        assert(ParserCombinator.holaMundoRightmost.parser("holamundo!").result.get == ("mundo","!"))
      }

    }

    "Leftmost <~" - {

      "Fails when first parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoLeftmost.parser("holimundo").result.get
        }
      }

      "Fails when second parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoLeftmost.parser("holamundu").result.get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.holaMundoLeftmost.parser("holamundo").result.get == ("hola","mundo"))
      }

      "Successful when both parsers success with remaining text" in {
        assert(ParserCombinator.holaMundoLeftmost.parser("holamundo!").result.get == ("hola","mundo!"))
      }

    }

    /*"Separated-by sepBy" - {
     val inicialesNombresConBarras = ParserCombinator.letter.sepBy(char('/'))

      "Fails when separator parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.inicialesNombresConBarras.parser("Juan-Perez").result.get
        }
      }

      "Fails when container parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.inicialesNombresConBarras.parser("Juan/1erez").result.get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.inicialesNombresConBarras.parser("Juan/Perez").result.get == ([('J',"uan"), ('P',"erez") ], ""))
      }

      "Successful with one iterations" in {
        assert(ParserCombinator.inicialesNombresConBarras.parser("Juan").result.get == ([('J',"uan")], ""))
      }

      "Successful with multiple iterations" in {
        assert(ParserCombinator.inicialesNombresConBarras.parser("Juan/Martin/Perez/Dias").result.get == ([('J',"uan"), ('M',"artin"), ('P',"erez"), ('D',"ias"), "")])
      }
    }*/

  }

  "Operations" - {

  }

  "Interation" - {

    "Caso práctico: Musiquita" - {

    }

  }

}