import java.io.{PushbackReader, StringReader}
import Musica._
import scala.collection.mutable.ListBuffer

case class Note(name: String)

class MusicParser(input: String) {
  protected val inputStream = new PushbackReader(new StringReader(parsear(input)))

  protected def parseChar(): Char = {
    val parsed = inputStream.read()
    if (parsed == -1) throw new EOIParserException
    return parsed.toChar
  }

  protected def parseNote(): Nota = {
    var next: Char = ' '
    do next = parseChar() while (next == ' ')
    Nota.notas.find(_.toString == next.toString()).getOrElse(throw new NotANoteException(next))
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

  def parsearN(i: Int, string: String) : String = {
    if(i == 1){
      if(string.length <= 3)
        return string
      else if("ABCDEFG".contains(string.head))
        return string.head.toString ++ parsearN(1,string.tail)
      else if("(x)".contains(string.head) || string.head.isDigit){
        var temp = string.drop(3)
        return parsearN(string.head.asDigit,obtenerPrincipio(temp)) ++ parsearN(1,temp.drop(obtenerPrincipio(temp).length + 1))
      }else
        throw new NotANoteException(string.head)
    }else{
      return parsearN(1,string) ++ parsearN(i-1,string)
    }
    return string
  }

  def parsear(string : String) = parsearN(1,string.filter(_ != ' '))

  def obtenerPrincipio(input : String): String = {
    var resultado: String = ""
    var parentesisA: Int = 1

    var i = 0
    while(parentesisA != 0){
      if(input.charAt(i) == '(')
        parentesisA += 1
      else if(input.charAt(i) == ')')
        parentesisA -= 1

      resultado ++= input.charAt(i).toString
      i+=1
    }
    return resultado.init
  }

}



class ParserException(reason: String) extends Exception(reason)
class EOIParserException extends ParserException("reached end of input")
class NotANoteException(val read: Char) extends ParserException(s"Expected [A|B|C|D|E|F|G] but got $read")