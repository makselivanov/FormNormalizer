package mselivanov.normalforms
import org.scalatest.flatspec.AnyFlatSpec

object ParserFormulaTest extends AnyFlatSpec {
  "A Parser" must "parse True" in {
    println(ParserFormula("True"))
    assert(ParserFormula("True").isRight)
  }
}
