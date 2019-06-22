import scala.util.{Failure, Success, Try}

object ParserCombinator {

  trait Result{
    def remaining() :String
  }

  /*
  case class UnitResult(result :Option[Result],remaining: String) extends Result

  case class CharResult(result: Char, remaining: String) extends Result

  case class StringResult(result: String, remaining: String) extends Result

  case class TupleResult(result: ParseResult, remaining: String) extends Result

  type ParseResult = (Try[Result],Try[Result])
*/


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
  val char: Char => Parser[Char] = (char :Char) => Parser[Char](charFunc(char,_))
  val void = Parser(voidFunc)
  val letter = Parser(letterFunc)
  val digit = Parser(digitFunc)
  val alphaNum = Parser(alphaNumFunc)
  val string: String => Parser[String] = (expected :String) => Parser[String](stringFunc(expected,_))


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

    def <>(otherParser :Parser[T]) = {
      Parser(
        (input :String) => NewResult[(T,T)](Try(
          ((parser(input).result.get._1,otherParser.parser(parser(input).result.get._2).result.get._1),otherParser.parser(parser(input).result.get._2).result.get._2)
        ))
      )
    }


  }

  val aob = char('a') <|> char('b')

  val holaMundo = ParserCombinator.string("hola") <> ParserCombinator.string("mundo")

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