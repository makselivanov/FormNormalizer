package mselivanov.normalforms
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object ParserFormula extends RegexParsers with PackratParsers {
  val boolTrue: Parser[Boolean] = ("True" | "true" | "T") ^^ (_ => true)
  val boolFalse: Parser[Boolean] = ("False" | "false" | "F") ^^ (_ => false)
  val bool: Parser[Expression] = (boolTrue | boolFalse) ^^ Bool
  val variable: Parser[String] = """[a-zA-Z0-9]+""".r ^^ { identity }
  val neg:   Parser[String] = "~" | "!"
  val literal: Parser[Expression] =
    ((neg ~> bool) ^^ (b => Bool(!b.asInstanceOf[Bool].flag))) |
      bool |
      ((neg ~> variable) ^^ (it => Literal(it, neg = true))) |
      (variable ^^ (it => Literal(it)))
  val conj:  Parser[BinOperator] = ("^" | "&&" | "&") ^^ (_ => Conjunction())
  val disj:  Parser[BinOperator] = ("v" | "||" | "|") ^^ (_ => Disjunction())
  val cons:  Parser[BinOperator] = ("=>" | "->") ^^ (_ => Consequence())
  val equiv: Parser[BinOperator] = ("<=>" | "<->" | "==") ^^ (_ => Equivalence())
  lazy val not_level: Parser[Expression] =
    literal | (neg ~ ("(" ~> equ_level <~ ")") ^^ (it => NotOperation(it._2))) | "(" ~> equ_level <~ ")"
  lazy val and_level: Parser[Expression] =
    (and_level ~ cons ~ not_level ^^ makeBinOperation) | not_level
  lazy val or_level: Parser[Expression] =
    (or_level ~ disj ~ and_level ^^ makeBinOperation) | and_level
  lazy val imp_level: Parser[Expression] =
    (imp_level ~ cons ~ or_level ^^ makeBinOperation) | or_level
  lazy val equ_level: Parser[Expression] =
    (equ_level ~ equiv ~ imp_level ^^ makeBinOperation) | imp_level
  lazy val formula: Parser[Expression] =
    ("(" ~> formula <~ ")") | equ_level
  //TODO stackoverflow Error!!!

  def apply(string: String): Either[ParserError, Expression] = {
    parseAll(formula, new PackratReader(new CharSequenceReader(string))) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(ParserError(msg))
    }
  }

  def makeBinOperation(it: Expression ~ BinOperator ~ Expression): BinOperation = {
    BinOperation(it._1._1, it._1._2, it._2)
  }

  case class ParserError(msg: String)
}
sealed trait BinOperator
case class Conjunction() extends BinOperator
case class Disjunction() extends BinOperator
case class Consequence() extends BinOperator
case class Equivalence() extends BinOperator


sealed trait Expression
case class Bool(flag: Boolean) extends Expression
case class Literal(name: String, neg: Boolean = false) extends Expression
case class BinOperation(left: Expression, op: BinOperator, right: Expression) extends Expression
case class NotOperation(exp: Expression) extends Expression
