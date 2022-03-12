package mselivanov.normalforms

object Normalizer {
  def nnf_transform(expression: Expression): Expression = {
    expression match {
      case BinOperation(left, op, right) =>
        op match {
          case Conjunction() | Disjunction() => //A ^ B ~~> A ^ B, A v B ~~> A v B
            BinOperation(nnf_transform(left), op, nnf_transform(right))
          case Consequence() => //A => B ~~> !A v B
            BinOperation(nnf_transform(NotOperation(left)), Disjunction(), nnf_transform(right))
          case Equivalence() => //A <=> B ~~> (A => B) ^ (B => A)
            BinOperation(
              nnf_transform(BinOperation(left, Consequence(), right)),
              Conjunction(),
              nnf_transform(BinOperation(right, Consequence(), left))
            )
        }

      //Sorting throught every possible with first NotOperation
      case NotOperation(NotOperation(exp)) => //!!A ~~> A
        nnf_transform(exp)
      case NotOperation(BinOperation(left, op, right)) =>
        op match {
          case Conjunction() => //!(A ^ B) ~~> !A v !B
            BinOperation(
              NotOperation(nnf_transform(left)),
              Disjunction(),
              NotOperation(nnf_transform(right))
            )
          case Disjunction() => //!(A v B) ~~> !A ^ !B
            BinOperation(
              NotOperation(nnf_transform(left)),
              Conjunction(),
              NotOperation(nnf_transform(right))
            )
          case Consequence() => //!(A => B) ~~> A ^ !B
            BinOperation(left, Conjunction(), NotOperation(right))
          case Equivalence() => //!(A <=> B) ~~> !(A => B) v !(B => A)
            BinOperation(
              nnf_transform(NotOperation(BinOperation(
                left,
                Consequence(),
                right
              ))),
              Disjunction(),
              nnf_transform(NotOperation(BinOperation(
                right,
                Consequence(),
                left
              )))
            )
        }
      case NotOperation(Literal(name, neg)) => //!x ~~> x, !!x ~~> x
        Literal(name, !neg)
      case NotOperation(Bool(flag)) => //!F ~~> T, !T ~~> F
        Bool(!flag)

      case Literal(name, neg) =>
        Literal(name, neg)

      case Bool(flag) =>
        Bool(flag)
    }
  }

  def string_transform(expression: Expression): String = {
    expression match {
      case Literal(name, neg) =>
        if (!neg) {
          name
        } else {
          "!" ++ name
        }
      case Bool(flag) =>
        if (flag) {
          "T"
        } else {
          "F"
        }
      case NotOperation(expression) =>
        "!(" ++ string_transform(expression) ++ ")"
      case BinOperation(left, op, right) =>
        op match {
          case Conjunction() =>
            "(" ++ string_transform(left) ++ " ^ " ++ string_transform(right) ++ ")"
          case Disjunction() =>
            "(" ++ string_transform(left) ++ " v " ++ string_transform(right) ++ ")"
          case Consequence() =>
            "(" ++ string_transform(left) ++ " => " ++ string_transform(right) ++ ")"
          case Equivalence() =>
            "(" ++ string_transform(left) ++ " <=> " ++ string_transform(right) ++ ")"
        }
    }
  }

  def normalize(typeForm : Symbol, formula: String): String = {
    val parsed = ParserFormula(formula)
    parsed match {
      case Right(expression) =>
        string_transform(typeForm match {
          case Constants.nnf => nnf_transform(expression)
          //case Constants.cnf => ///TODO
          //case Constants.dnf => ///TODO
          case _ => throw new RuntimeException("Mismatch typeForm")
        })
      case Left(error) => throw new RuntimeException(error.msg)
    }
  }
}
