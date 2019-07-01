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
        assert(ParserCombinator.void.parse("Some Test Text").result.get == ((), "ome Test Text"))
      }

      "Successful when parses as Unit the first char when fed with only one char text" in {
        assert(ParserCombinator.void.parse("A").result.get == ((), ""))
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
        assert(ParserCombinator.letter.parse("Some Test Text").result.get == ('S', "ome Test Text"))
      }

      "Successful when parses the first char when fed with only one letter char text" in {
        assert(ParserCombinator.letter.parse("A").result.get == ('A', ""))
      }

      "Successful when parses the first char as letter with mixed fed text" in {
        assert(ParserCombinator.letter.parse("S0M3 73$7 73X7").result.get == ('S', "0M3 73$7 73X7"))
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
        assert(ParserCombinator.digit.parse("1234").result.get == ('1', "234"))
      }

      "Successful when parses the first char when fed with only one digit char text" in {
        assert(ParserCombinator.digit.parse("1").result.get == ('1', ""))
      }

      "Successful when parses the first char as digit with mixed fed text" in {
        assert(ParserCombinator.digit.parse("5ome Test Text").result.get == ('5', "ome Test Text"))
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
        assert(ParserCombinator.alphaNum.parse("1234").result.get == ('1', "234"))
      }

      "Successful when parses the first char when fed with only one digit char text" in {
        assert(ParserCombinator.alphaNum.parse("1").result.get == ('1', ""))
      }

      "Successful when parses the first char as digit with mixed fed text" in {
        assert(ParserCombinator.alphaNum.parse("5ome Test Text").result.get == ('5', "ome Test Text"))
      }

      "Successful when parses the first char as letter of the given text" in {
        assert(ParserCombinator.alphaNum.parse("Some Test Text").result.get == ('S', "ome Test Text"))
      }

      "Successful when parses the first char when fed with only one letter char text" in {
        assert(ParserCombinator.alphaNum.parse("A").result.get == ('A', ""))
      }

      "Successful when parses the first char as letter with mixed fed text" in {
        assert(ParserCombinator.alphaNum.parse("S0M3 73$7 73X7").result.get == ('S', "0M3 73$7 73X7"))
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
          ParserCombinator.aob.parse("cdefghi").result.get
        }
      }

      "Successful when first parser is successful" in {
        assert(ParserCombinator.aob.parse("arbol").result.get == ('a', "rbol"))
      }

      "Successful when first parser fails but second parser is successful" in {
        assert(ParserCombinator.aob.parse("bort").result.get == ('b', "ort"))
      }
    }

    "CONCAT <>" - {

      "Fails when first parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoConcat.parse("holimundo").result.get
        }
      }

      "Fails when second parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoConcat.parse("holamundu").result.get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.holaMundoConcat.parse("holamundo").result.get == (("hola","mundo"),""))
      }

      "Successful when both parsers success with remaining text" in {
        assert(ParserCombinator.holaMundoConcat.parse("holamundo!").result.get == (("hola","mundo"),"!"))
      }

    }
    "Rightmost ~>" - {

      "Fails when first parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoRightmost.parse("holimundo").result.get
        }
      }

      "Fails when second parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoRightmost.parse("holamundu").result.get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.holaMundoRightmost.parse("holamundo").result.get == ("mundo",""))
      }

      "Successful when both parsers success with remaining text" in {
        assert(ParserCombinator.holaMundoRightmost.parse("holamundo!").result.get == ("mundo","!"))
      }

    }

    "Leftmost <~" - {

      "Fails when first parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoLeftmost.parse("holimundo").result.get
        }
      }

      "Fails when second parser fails" in {
        assertThrows[ParseError] {
          ParserCombinator.holaMundoLeftmost.parse("holamundu").result.get
        }
      }

      "Successful when both parsers success" in {
        assert(ParserCombinator.holaMundoLeftmost.parse("holamundo").result.get == ("hola",""))
      }

      "Successful when both parsers success with remaining text" in {
        assert(ParserCombinator.holaMundoLeftmost.parse("holamundo!").result.get == ("hola","!"))
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

      "Operación opt" - {

        "Parses infija" in {
          assert(ParserCombinator.precedencia.parse("infija").result.get == ((Some("in"),"fija"),""))
        }

        "Parses fija" in {
          assert(ParserCombinator.precedencia.parse("fija").result.get == ((None,"fija"),""))
        }

      }

    "Kleene * operator" - {

      "No result parsing no C chars" in {
          assert(ParserCombinator.kleeneDeC.parse("hola").result.get == (List(),"hola"))
      }

      "Parses C lot of times" in {
        assert(ParserCombinator.kleeneDeC.parse("CCChola").result.get == (List('C','C','C'),"hola"))
      }
    }

    "Kleene + operator" - {

      "Fails parsing no hola" in {
        assertThrows[ParseError]{
          ParserCombinator.kleenePositivaDeHola.parse("chauchau").result.get
        }
      }

      "Parses hola lot of times" in {
        assert(ParserCombinator.kleenePositivaDeHola.parse("holaholaholaholachau").result.get == (List("hola","hola","hola","hola"),"chau"))
      }
    }

  }

  "Interation" - {

    "Caso práctico: Musiquita" - {

    }

  }

}