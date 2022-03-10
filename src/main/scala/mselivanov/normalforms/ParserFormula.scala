package mselivanov.normalforms
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object ParserFormula extends RegexParsers with PackratParsers {
  def boolTrue: Parser[Boolean] = ("True" | "true" | "T") ^^ (_ => true)
  def boolFalse: Parser[Boolean] = ("False" | "false" | "F") ^^ (_ => false)
  def bool: Parser[Expression] = (boolTrue | boolFalse) ^^ Bool
  def variable: Parser[String] = """[a-zA-Z0-9]+""".r ^^ { _.toString }
  def neg:   Parser[String] = ("~" | "!")
  def literal: Parser[Expression] =
    ((neg ~> bool) ^^ (b => Bool(!b.asInstanceOf[Bool].flag))) |
      (bool ^^ identity) |
      ((neg ~> variable) ^^ (it => Literal(it, neg = true))) |
      (variable ^^ (it => Literal(it)))
  def conj:  Parser[String] = ("^" | "&&" | "&")
  def disj:  Parser[String] = ("v" | "||" | "|")
  def cons:  Parser[String] = ("=>" | "->")
  def equiv: Parser[String] = ("<=>" | "<->" | "==")
  def bin_op: Parser[BinOperator] =
    (conj ^^ (_ => Conjunction())) |
      (disj ^^ (_ => Disjunctions())) |
      (cons ^^ (_ => Consequence())) |
      (equiv ^^ (_ => Equivalence()))
  lazy val formula: Parser[Expression] =
    (clause ^^ identity) | ("(" ~> clause <~ ")")
  lazy val clause: Parser[Expression] =
    ((neg ~> formula) ^^ (it => NotOperation(it))) |
      ((formula ~ bin_op ~ formula) ^^ (it => BinOperation(it._1._1, it._1._2, it._2)))

  def apply(string: String): Either[ParserError, Expression] = {
    parse(formula, new PackratReader[Char](new CharSequenceReader(string))) match {
      case Success(result, next) => Right(result)
      case NoSuccess(msg, next) => Left(ParserError(msg))
    }
  }

  case class ParserError(msg: String)
}
sealed trait BinOperator
case class Conjunction() extends BinOperator
case class Disjunctions() extends BinOperator
case class Consequence() extends BinOperator
case class Equivalence() extends BinOperator

sealed trait Expression
case class Bool(flag: Boolean) extends Expression
case class Literal(name: String, neg: Boolean = false) extends Expression
case class BinOperation(left: Expression, op: BinOperator, right: Expression) extends Expression
case class NotOperation(exp: Expression) extends Expression
