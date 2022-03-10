package mselivanov.normalforms

import Constants.{nnf, cnf, dnf}

object Normalizer {
  def normalize(typeForm : Symbol, formula: String): String = {
    val parsed = ParserFormula(formula)
    formula //TODO chagne parsed to formula
  }
}
