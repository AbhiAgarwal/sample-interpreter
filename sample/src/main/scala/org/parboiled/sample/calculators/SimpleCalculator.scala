import org.parboiled.scala._
import org.parboiled.errors.{ ErrorUtils, ParsingException }

class SimpleCalculator extends Parser {
  def InputLine = rule { WhiteSpace ~ Expression ~ EOI }
  def Expression: Rule1[Int] = rule {
    Term ~ zeroOrMore(
      "+" ~ Term ~~> ((a: Int, b) => a + b)
        | "-" ~ Term ~~> ((a: Int, b) => a - b))
  }
  def Term = rule {
    Factor ~ zeroOrMore(
      "*" ~ Factor ~~> ((a: Int, b) => a * b)
        | "/" ~ Factor ~~> ((a: Int, b) => a / b)
        | "%" ~ Factor ~~> ((a: Int, b) => a % b))
  }
  def Factor = rule { WhiteSpace ~ (Number | Parens) ~ WhiteSpace }
  def Parens = rule { "(" ~ Expression ~ ")" }
  def Number = rule { Digits ~> (_.toInt) }
  def Digits = rule { oneOrMore(Digit) }
  def Digit = rule { "0" - "9" }
  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ WhiteSpace
    else
      str(string)
}

object RunSimpleCalculator {
  def main(args: Array[String]) {
    val input = "(1+2*3) % 4 + 4"
    val parser = new SimpleCalculator { override val buildParseTree = true }
    val parsingResult = ReportingParseRunner(parser.InputLine).run(input)
    parsingResult.result match {
      case Some(i) => println("Answer is: " + i)
      case None => throw new ParsingException("Invalid calculation expression:\n" +
        ErrorUtils.printParseErrors(parsingResult))
    }
    // val parseTreePrintOut = org.parboiled.support.ParseTreeUtils.printNodeTree(parsingResult)
    // println(parseTreePrintOut)
  }
}