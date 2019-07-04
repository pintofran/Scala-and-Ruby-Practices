import scala.util.{Failure, Success, Try}

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
        (input :String) => applyUntilNoRemaining(input).result.get match {
          case (List(),_) => NewResult[List[T]](Try(throw new ParseError))
          case _ => applyUntilNoRemaining(input)
        }
      )
    }

    def * = {
      Parser(
        (input :String) => applyUntilNoRemaining(input)
      )
    }

    def sepBy (separator :Char) = {
      var esSeparador: Bool = false
      do
       var charBuffer: ListBuffer[Nota] = ListBuffer()
       var nextChar = anyChar.parse(input).result.get
      if !(anyChar.parse(input).result.get == separator) charBuffer ++= nextChar()
      else esSeparador = true
      while !esSeparador
      Parser(
        (input :String) => NewResult[U](Try(
          parser(charBuffer.toString).result.get match {
            case (result,remaining) => (constant,"")
          }
        ))input)
      )
      //Falta repeticion
    }

    def const (constant :U) = {
      Parser(
      (input :String) => NewResult[U](Try(
          parser(input).result.get match {
            case (result,remaining) => (constant,"")
          }
        ))
      )
    }

    def map (maperFunc : T => U ) = {
      Parser(
      (input :String) => NewResult[U](Try(
          parser(input).result.get match {
            case (result,remaining) => (maperFunc(result),"")
          }
        ))
      )
    }

    def applyUntilNoRemaining(input:String,resultList: List[T] = List()):NewResult[List[T]] = parser(input).result match {
      case Success((result,"")) => NewResult(Try((resultList ++ List(result),"")))
      case Success((result,remaining)) => applyUntilNoRemaining(remaining,resultList ++ List(result))
      case _ => NewResult(Try((resultList,input)))
      }

  }

  val aob: Parser[Char] = char('a') <|> char('b')
  val holaMundoConcat: Parser[(String, String)] = string("hola") <> string("mundo")
  val holaMundoRightmost: Parser[String] = string("hola") ~> string("mundo")
  val holaMundoLeftmost: Parser[String] = string("hola") <~ string("mundo")

  val talVezIn = string("in").opt
  // precedencia parsea exitosamente las palabras "infija" y "fija"
  val precedencia = talVezIn <> string("fija")

  val kleenePositivaDeHola = string("hola").+
  val kleeneDeC = char('C').*


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



}