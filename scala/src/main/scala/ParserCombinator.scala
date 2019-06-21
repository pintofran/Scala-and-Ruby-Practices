import scala.util.{Failure, Success, Try}

object ParserCombinator {

  trait Result

  case class UnitResult(remaining: String) extends Result

  case class CharResult(result: Char, remaining: String) extends Result

  case class StringResult(result: String, remaining: String) extends Result


  val anyChar = Parser(anyCharFunc)
  val void = Parser(voidFunc)
  val char: Char => Parser = (char :Char) => Parser(charFunc(char,_))
  val letter = Parser(letterFunc)
  val digit = Parser(digitFunc)
  val alphaNum = Parser(alphaNumFunc)
  val string: String => Parser = (expected :String) => Parser(stringFunc(expected,_))

  class ParseError extends RuntimeException

  case class Parser(parser :SpecificParser){

    def parse(input :String) :Try[Result] ={
      if (input.isEmpty)
        Try(throw new ParseError)
      else
        parser(input)
    }

    def <|>(otherParser :Parser) :Parser = {
      Parser(
        (input :String) => parser(input).orElse(otherParser.parser(input))
      )
    }
  }

  val aob: Parser = char('a') <|> char('b')

  type SpecificParser = String => Try[Result]

  def parseSuccessfullChar(input: String): CharResult = CharResult(input.head, input.tail)

  def anyCharFunc(input: String): Try[CharResult] = Try(parseSuccessfullChar(input))

  def voidFunc(input: String): Try[Result] = Try(UnitResult(input.tail))

  def isExpectedChar(condition: Boolean, input: String): Try[CharResult] = {
    if (condition)
      anyCharFunc(input)
    else
     Try(throw new ParseError)
  }

  def charFunc(expectedHead: Char, input: String): Try[CharResult] = isExpectedChar(input.head == expectedHead, input)

  def letterFunc(input: String): Try[CharResult] = isExpectedChar(input.head.isLetter, input)

  def digitFunc(input: String): Try[CharResult] = isExpectedChar(input.head.isDigit, input)

  def alphaNumFunc(input: String): Try[CharResult] = letterFunc(input).orElse(digitFunc(input))

  def stringFunc(expectedString: String, input: String): Try[StringResult] = {
    if (expectedString == input.take(expectedString.length)) Try(expectedString, input.drop(expectedString.length))
    Try(throw new ParseError)
  }

}