import Musica._
import org.scalatest.{FreeSpec, Matchers}

class MusicParserTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe(expectedResult)
  }

  def assertParseFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParserException](actualResult)
  }

  "Parseo Compuesto" - {
    "Parseo Multiple" - {
      "Ok" in {
        assert(new MusicParser("2x(A B 3x(F G 2x(A))) F B E").parse() == List(A, B, F, G, A, A, F, G, A, A, F, G, A, A, A, B, F, G, A, A, F, G, A, A, F, G, A, A, F, B, E))
      }
    }
    "Parseo Multiple 2digitos" - {
      "Ok" in {
        assert(new MusicParser("F B 10x(A) B B E").parse() == List(F, B, A, A, A, A, A, A, A, A, A, A, B, B, E))
      }
    }
    "Parseo Multiple con error de sintaxis de patron" - {
      "Not Ok" in {
        assertThrows[NotOpenBracketException](new MusicParser("2xx(A B 3x(F G 2x(A))) F B E").parse())
      }
    }
    "Parseo Multiple con error sin x" - {
      "Not Ok" in {
        assertThrows[NotXException](new MusicParser("2(A B 3x(F G 2x(A))) F B E").parse())
      }
    }
    "Parseo Multiple sin cerrar parentesis" - {
      "Not Ok" in {
        assertThrows[NotCloseBracketException](new MusicParser("2x((A B 3x(F G 2x(A))) F B E").parse())
      }
    }
    "error de sintaxis" - {
      "Not Ok" in {
        assertThrows[NotANoteException](new MusicParser("(as)d456lsjndfnwf").parse())
      }
    }
  }

  "MusicParser" - {
    "when fed empty text" - {
      "parses an empty list of notes" in {
        assertParsesSucceededWithResult(new MusicParser("").parse(), Nil)
      }
    }



    "when fed a text that is a note letter" - {
      "parses a list with that one note" in {
        assertParsesSucceededWithResult(new MusicParser("A").parse(), List(A))
      }
    }

    "when it is fed a text that ends in a space" - {
      "it parses it as it that space wasn't there" in {
        assertParsesSucceededWithResult(new MusicParser("A ").parse(), List(A))
      }
    }

    "when fed a text that is a non valid letter for a note" - {
      "it fails" in {
        assertParseFailed(new MusicParser("J").parse(), List(A))
      }
    }

    "when fed a text that is a note letter followed by something that is not a note" - {
      "it fails" in {
        assertParseFailed(new MusicParser("A J").parse(), List(A))
      }
    }

    "when it is fed a text that is several notes in a row" - {
      "parses a list with the different notes in order" in {
        assertParsesSucceededWithResult(new MusicParser("AB").parse(), List(A, B))
      }

      "even when the notes are separated by a space" - {
        "parses a list with the different notes in order" in {
          assertParsesSucceededWithResult(new MusicParser("A B").parse(), List(A, B))
        }
      }

      "even when the notes are separated by several spaces" - {
        "parses a list with the different notes in order" in {
          assertParsesSucceededWithResult(new MusicParser("A  B").parse(), List(A, B))
        }
      }
    }
  }
}
