import java.io.{PushbackReader, StringReader}
import Musica._
import scala.collection.mutable.ListBuffer

case class Note(name: String)

class MusicParser(input: String) {
  protected val inputStream = new PushbackReader(new StringReader(input))

  protected def parseChar(): Char = {
    val parsed = inputStream.read()
    if (parsed == -1) throw new EOIParserException
    return parsed.toChar
  }

  protected def parseNote(next: Char): Nota = {
    Nota.notas.find(_.toString == next.toString()).getOrElse(throw new NotANoteException(next))
  }

  protected def isNote(char: Char): Boolean = {
    Nota.notas.toString.contains(char)
  }

  protected def parseMelody(): List[Nota] = {
    var next: Char = ' '
    do next = parseChar() while (next == ' ')
    if (next.isDigit) {
      return parsePattern(next)
    }
    if (isNote(next)) {
      val note = parseNote(next)
      return List[Nota](note)
    }
    throw new NotANoteOrDigitException(next)
  }

  protected def parsePattern(firstDigit: Char): List[Nota] = {
    try {
      var next: Char = ' '
      val repetitionCharList: ListBuffer[Char] = ListBuffer()
      repetitionCharList += firstDigit
      do { 
        do next = parseChar() while (next == ' ')
        repetitionCharList += next
      }
      while (!next.isDigit)
      val repetitionString: String = repetitionCharList.mkString
      if (next == 'x') {
        val repetitionNumber: Int = repetitionString.toInt
        if (repetitionNumber == 0) return []
        return parsePatternWithRepetitionNumber(repetitionNumber)
      }
      throw new NotXException(next)
    } catch {
      case _: EOIParserException => 
    }
    throw new SintaxException()
  }

  protected def parsePatternWithRepetitionNumber(repetitionNumber: Int): List[Nota] = {
    try {
      var next: Char = ' '
      do next = parseChar() while (next == ' ')
      if (next.toString !== "(") {
        throw new NotOpenBracketException(next)
      }
      var patternDeepLevel: Int = 1
      val bracketPatternCharList: ListBuffer[Char] = ListBuffer()
      do { 
        do next = parseChar() while (next == ' ') 
        if (next.toString == ")") patternDeepLevel --
        if (next.toString == "(") patternDeepLevel ++
        if (patternDeepLevel != 0) bracketPatternCharList += next
      }
      while (patternDeepLevel != 0) 
      val bracketPattern: String = bracketPatternCharList.mkString
      var completeBracketPattern: String = ""
      for (i <- 1 to repetitionNumber){
        completeBracketPattern = completeBracketPattern ++ bracketPattern
      }
      return new MusicParser(completeBracketPattern).parse()
    } catch {
      case _: EOIParserException => 
    }
    throw new NotCloseBracketException()
  }

def parse(): List[Nota] = {
    val result: ListBuffer[Nota] = ListBuffer()
    try while (true)
      result += parseNote()
    catch {
      case _: EOIParserException =>
    }
    return result.toList
  }

class ParserException(reason: String) extends Exception(reason)
class EOIParserException extends ParserException("reached end of input")
class NotANoteException(val read: Char) extends ParserException(s"Expected [A|B|C|D|E|F|G] but got $read")
class NotANoteOrDigitException(val read: Char) extends ParserException(s"Expected [A|B|C|D|E|F|G] or DIGIT but got $read")
class NotXException(val read: Char) extends ParserException(s"Expected x but got $read")
class NotOpenBracketException(val read: Char) extends ParserException(s"Expected open bracket but got $read")
class NotCloseBracketException() extends ParserException(s"Unexpected end of bracket, missing closing bracket")
class SintaxException() extends ParserException(s"Sintax Error")