import scala.util.{Success, Try}

object ParserCombinator {

  case class GenericResult[+T] (result :T, remaining :String)

  case class NewResult[+T] (result :Try[GenericResult[T]]){

    def getResult(): T = result.get.result
    def getRemaining(): String = result.get.remaining

  }

  type UnitResult = NewResult[Unit]
  type CharResult = NewResult[Char]
  type StringResult = NewResult[String]
  type TupleResult[T,OT] = NewResult[(T,OT)]
  type ListResult[T] = NewResult[List[T]]

  type CharParser = String => CharResult
  type StringParser = String => StringResult
  type VoidParser = String => UnitResult

  type SpecificParser[+T] = String => NewResult[T]


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

  case class Parser[+T](parser :SpecificParser[T]){

    def parse(input :String) :NewResult[T] ={
      if (input.isEmpty)
        NewResult[T](Try(throw new ParseError))
      else
        parser(input)
    }

    def <|>[U >: T](otherParser :Parser[U]) :Parser[U] = {
      Parser(
        (input :String) => NewResult(parser(input).result.orElse(otherParser.parser(input).result))
      )
    }


    def <>[U](otherParser: Parser[U]): Parser[(T, U)] =
    {
      Parser(
        (input :String) => NewResult[(T,U)](Try(

          parser(input).result.get match {
            case GenericResult(result,remaining) => GenericResult((result,otherParser.parser(remaining).getResult()),otherParser.parser(remaining).getRemaining())
          }

        ))
      )
    }

    def ~> [U >: T](otherParser :Parser[U]) :Parser[U] = {
      Parser(
        (input :String) => NewResult[U](Try(
          parser(input).result.get match {
            case GenericResult(_,remaining) => GenericResult(otherParser.parser(remaining).getResult(),otherParser.parser(remaining).getRemaining())
          }
        ))
      )
    }

    def <~ [U >: T](otherParser :Parser[U]) = {
      Parser(
        (input :String) => NewResult[U](Try(
          parser(input).result.get match {
            case GenericResult(result,remaining) => GenericResult(result,otherParser.parser(remaining).getRemaining())
          }
        ))
      )
    }

    def satisfies (condition : T => Boolean) = {
      Parser(
        (input :String) => NewResult[T](Try(
          parser(input).result.get match {
          case GenericResult(result,remaining) if condition(result) => GenericResult(result,remaining)
          case _ => throw new ParseError
        }
      )))
    }

    def opt  = {
      Parser(
        (input :String) => NewResult[Option[T]](Try(
          parser(input).result match {
            case Success(GenericResult(result,remaining)) => GenericResult(Some(result),remaining)
            case _ => GenericResult(None,input)
          }
        )))
    }

    def +  = {
      Parser(
        (input :String) => applyWhilePosible(input).result.get match {
          case GenericResult(List(),_) => NewResult[List[T]](Try(throw new ParseError))
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
            case GenericResult(_, rem) => GenericResult(constant,rem)
          }
        ))
      )
    }

    def map [U](maperFunc : T => U ) = {
      Parser(
      (input :String) => NewResult[U](Try(
          parser(input).result.get match {
            case GenericResult(result, rem) => GenericResult(maperFunc(result),rem)
          }
        ))
      )
    }

    def applyWithSeparator [U,V >: T](input:String,resultList: List[V] = List(), separatorParser :Parser[U]):NewResult[List[V]] = parser(input).result match {
      case Success(GenericResult(result,"")) => NewResult(Try(GenericResult(resultList ++ List(result),"")))
      case Success(GenericResult(result,remaining)) => separatorParser.parser(remaining).result match {
        case Success(GenericResult(_,rem)) => applyWithSeparator(rem,resultList ++ List(result),separatorParser)
        case _ => NewResult(Try(GenericResult(resultList ++ List(result),remaining)))
      }
      case _ => NewResult(Try(throw new ParseError))
    }

    def applyWhilePosible[U >: T](input:String,resultList: List[U] = List()):NewResult[List[U]] = parser(input).result match {
      case Success(GenericResult(result,"")) => NewResult(Try(GenericResult(resultList ++ List(result),"")))
      case Success(GenericResult(result,remaining)) => applyWhilePosible(remaining,resultList ++ List(result))
      case _ => NewResult(Try(GenericResult(resultList,input)))
      }

  }

  def charListToInt(list :List[Char]) : Int ={
   list.mkString.toInt
  }

  val integer = digit.+.map(charListToInt)

  val tel = integer.sepBy(char('-'))


  def parseSuccessfullChar(input: String): GenericResult[Char] = GenericResult(input.head,input.tail)

  def anyCharFunc(input: String): CharResult = NewResult[Char](Try(parseSuccessfullChar(input)))

  def charFunc(expectedHead: Char, input: String): CharResult = isExpectedChar(input.head == expectedHead, input)

  def isExpectedChar(condition: Boolean, input: String): CharResult = {
    if (condition)
      anyCharFunc(input)
    else
      NewResult[Char](Try(throw new ParseError))
  }

  def voidFunc(input: String): UnitResult = NewResult[Unit](Try(
    GenericResult(Unit,input.tail)
  ))

  def letterFunc(input: String): CharResult = isExpectedChar(input.head.isLetter, input)

  def digitFunc(input: String): CharResult = isExpectedChar(input.head.isDigit, input)

  def alphaNumFunc(input: String): CharResult = NewResult[Char](letterFunc(input).result.orElse(digitFunc(input).result))

  def stringFunc(expectedString: String, input: String): StringResult = {
    if (expectedString == input.take(expectedString.length))
      NewResult[String](Try(
        GenericResult(expectedString,input.drop(expectedString.length))
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

  import Musica._

  val silencio : Parser[Silencio] = char('_').map(_ => Silencio(Blanca)) <|> char('-').map(_ => Silencio(Negra)) <|> char('~').map(_ => Silencio(Corchea))

  val nombreNota : Parser[Nota]  = char('A').map(_=>A) <|> char('B').map(_=>A) <|> char('C').map(_=>B) <|> char('D').map(_=>C) <|> char('E').map(_=>D) <|> char('F').map(_=>E) <|> char('G').map(_=>F)
  val modificador : Parser[Char] = char('#') <|> char('b')

  val nota : Parser[Nota] = (nombreNota <> modificador.opt).map {
    case (not, Some('#')) => not.sostenido
    case (not, Some('b')) => not.bemol
    case (not, _) => not
  }

  val tono :Parser[Tono] = (digit <> nota).map {
    case (octava, not) => Tono(octava.toInt, not)
  }

  val figura = string("1/1").map(_=>Redonda) <|> string("1/2").map(_=>Blanca) <|> string("1/4").map(_=>Negra) <|> string("1/8").map(_=>Corchea) <|> string("1/16").map(_=>SemiCorchea)

  val sonido = (tono <> figura).map{case (ton,fig) => Sonido(ton,fig) }

  val acordeExplicito = (tono.sepBy(char('+')) <> figura).map{ case (ton,fig) => Acorde(ton,fig) }

  val acordeMenorOMayor = ((tono <> (char('m') <|> char('M'))) <> figura).map {
    case ((tono, 'm'), fig) => tono.nota.acordeMenor(tono.octava, fig)
    case ((tono, 'M'), fig) => tono.nota.acordeMayor(tono.octava, fig)
  }

  val acorde = acordeExplicito <|> acordeMenorOMayor

  val tocable = silencio <|> sonido <|> acorde

  val parserDeMelodia = tocable.sepBy(char(' '))

}