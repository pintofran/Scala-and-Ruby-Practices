import Musica.Nota
import com.sun.org.apache.xpath.internal.operations.Bool

import scala.collection.mutable.ListBuffer
import scala.util.{Success, Try}

object ParserCombinator {

  type GenericResult[T] = (T,String)

  case class NewResult[T] (result :Try[GenericResult[T]]){

    def getResult(): T = result.get._1
    def getRemaining(): String = result.get._2

  }

  type UnitResult = NewResult[Unit]
  type CharResult = NewResult[Char]
  type StringResult = NewResult[String]
  type TupleResult[T,OT] = NewResult[(T,OT)]
  type ListResult[T] = NewResult[List[T]]

  type CharParser = String => CharResult
  type StringParser = String => StringResult
  type VoidParser = String => UnitResult

  type SpecificParser[T] = String => NewResult[T]


  val anyChar = Parser(anyCharFunc)
  val char: Char => Parser[Char] = (char :Char) => Parser(charFunc(char,_))
  val void = Parser(voidFunc)
  val letter = Parser(letterFunc)
  val digit = Parser(digitFunc)
  val alphaNum = Parser(alphaNumFunc)
  val string: String => Parser[String] = (expected :String) => Parser[String](stringFunc(expected,_))

  //case objects que heredan de parser
  //extend Function

  class ParseError extends RuntimeException

  case class Parser[T](parser :SpecificParser[T]){

    def parse(input :String) :NewResult[T] ={
      if (input.isEmpty)
        NewResult[T](Try(throw new ParseError))
      else
        parser(input)
    }

    def <|>(otherParser :Parser[T]) = {
      Parser(
        (input :String) => NewResult(parser(input).result.orElse(otherParser.parser(input).result))
      )
    }

    def <>[U](otherParser: Parser[U]): Parser[(T, U)] =
    {
      Parser(
        (input :String) => NewResult[(T,U)](Try(

          parser(input).result.get match {
            case (result,remaining) => ((result,otherParser.parser(remaining).getResult()),otherParser.parser(remaining).getRemaining())
          }

        ))
      )
    }

    def ~> (otherParser :Parser[T]) = {
      Parser(
        (input :String) => NewResult[T](Try(
          parser(input).result.get match {
            case (_,remaining) => (otherParser.parser(remaining).getResult(),otherParser.parser(remaining).getRemaining())
          }
        ))
      )
    }

    def <~ (otherParser :Parser[T]) = {
      Parser(
        (input :String) => NewResult[T](Try(
          parser(input).result.get match {
            case (result,remaining) => (result,otherParser.parser(remaining).getRemaining())
          }
        ))
      )
    }

    def satisfies (condition : T => Boolean) = {
      Parser(
        (input :String) => NewResult[T](Try(
          parser(input).result.get match {
          case (result,remaining) if condition(result) => (result,remaining)
          case _ => throw new ParseError
        }
      )))
    }

    def opt  = {
      Parser(
        (input :String) => NewResult[Option[T]](Try(
          parser(input).result match {
            case Success((result,remaining)) => (Some(result),remaining)
            case _ => (None,input)
          }
        )))
    }

    def +  = {
      Parser(
        (input :String) => applyWhilePosible(input).result.get match {
          case (List(),_) => NewResult[List[T]](Try(throw new ParseError))
          case _ => applyWhilePosible(input)
        }
      )
    }

    def * = {
      Parser(
        (input :String) => applyWhilePosible(input)
      )
    }

    def sepBy[U](sepParser: Parser[U]) = {
      Parser(
        (input :String) => applyWithSeparator(input,List(),sepParser)
      )
    }

    def const [U](constant :U) = {
      Parser(
      (input :String) => NewResult[U](Try(
          parser(input).result.get match {
            case (_, rem) => (constant,rem)
          }
        ))
      )
    }

    def map [U](maperFunc : T => U ) = {
      Parser(
      (input :String) => NewResult[U](Try(
          parser(input).result.get match {
            case (result, rem) => (maperFunc(result),rem)
          }
        ))
      )
    }

    def applyWithSeparator [U](input:String,resultList: List[T] = List(), separatorParser :Parser[U]):NewResult[List[T]] = parser(input).result match {
      case Success((result,"")) => NewResult(Try((resultList ++ List(result),"")))
      case Success((result,remaining)) => separatorParser.parser(remaining).result match {
        case Success((_,rem)) => applyWithSeparator(rem,resultList ++ List(result),separatorParser)
        case _ => NewResult(Try((resultList ++ List(result),remaining)))
      }
      case _ => NewResult(Try(throw new ParseError))
    }

    def applyWhilePosible(input:String,resultList: List[T] = List()):NewResult[List[T]] = parser(input).result match {
      case Success((result,"")) => NewResult(Try((resultList ++ List(result),"")))
      case Success((result,remaining)) => applyWhilePosible(remaining,resultList ++ List(result))
      case _ => NewResult(Try((resultList,input)))
      }

  }

  def charListToInt(list :List[Char]) : Int ={
   list.mkString.toInt
  }

  val integer = digit.+.map(charListToInt)

  val tel = integer.sepBy(char('-'))


  def parseSuccessfullChar(input: String): GenericResult[Char] = (input.head,input.tail)

  def anyCharFunc(input: String): CharResult = NewResult[Char](Try(parseSuccessfullChar(input)))

  def charFunc(expectedHead: Char, input: String): CharResult = isExpectedChar(input.head == expectedHead, input)

  def isExpectedChar(condition: Boolean, input: String): CharResult = {
    if (condition)
      anyCharFunc(input)
    else
      NewResult[Char](Try(throw new ParseError))
  }

  def voidFunc(input: String): UnitResult = NewResult[Unit](Try(
    (Unit,input.tail)
  ))

  def letterFunc(input: String): CharResult = isExpectedChar(input.head.isLetter, input)

  def digitFunc(input: String): CharResult = isExpectedChar(input.head.isDigit, input)

  def alphaNumFunc(input: String): CharResult = NewResult[Char](letterFunc(input).result.orElse(digitFunc(input).result))

  def stringFunc(expectedString: String, input: String): StringResult = {
    if (expectedString == input.take(expectedString.length))
      NewResult[String](Try(
        (expectedString,input.drop(expectedString.length))
      ))
    else
      NewResult[String](Try(throw new ParseError))
  }

  //For testing

  val aob: Parser[Char] = char('a') <|> char('b')
  val holaMundoConcat: Parser[(String, String)] = string("hola") <> string("mundo")
  val holaMundoRightmost: Parser[String] = string("hola") ~> string("mundo")
  val holaMundoLeftmost: Parser[String] = string("hola") <~ string("mundo")

  val talVezIn: Parser[Option[String]] = string("in").opt
  // precedencia parsea exitosamente las palabras "infija" y "fija"
  val precedencia: Parser[(Option[String], String)] = talVezIn <> string("fija")

  val kleenePositivaDeHola: Parser[List[String]] = string("hola").+
  val kleeneDeC: Parser[List[Char]] = char('C').*

  val sepByDigit: Parser[List[Char]] = digit.sepBy(char('-'))

  val inicialesNombresConBarras: Parser[List[Char]] = letter.sepBy(char('/'))

  val constDeHolaAOtraCosa: Parser[String] = string("hola").const("otraCosa")

  val constDeHolaATrue: Parser[Boolean] = string("hola").const(true)

  val mapDeHolaAOtraCosa: Parser[String] = string("hola").map { case "hola" => "otraCosa" }

  //Caso practico


  val silencio : Parser[Musica.Figura] = char('_').map(_ => Musica.Blanca) <|> char('-').map(_ => Musica.Negra) <|> char('~').map(_ => Musica.Corchea)

  val nombreNota = char('A') <|> char('B') <|> char('C') <|> char('D') <|> char('E') <|> char('F') <|> char('G')
  val modificador = char('#') <|> char('b')

  val nota = nombreNota <> modificador.opt

  val tono = digit <> nota

  val figura = string("1/1") <|> string("1/2") <|> string("1/4") <|> string("1/8") <|> string("1/16")

  val sonido = tono <> figura

  val acordeExplicito = tono.sepBy(char('+')) <> figura

  val acordeMenorOMayor = (tono <> (char('m') <|> char('M'))) <> figura

  val acorde = acordeExplicito <|> acordeMenorOMayor

  val tocable = silencio <|> sonido <|> acorde

  val parserDeMelodia = tocable.sepBy(char(' '))

}