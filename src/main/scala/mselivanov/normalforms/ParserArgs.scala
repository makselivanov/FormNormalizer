package mselivanov.normalforms

import scala.annotation.tailrec
import Constants._
import scala.collection.mutable.Map

object ParserArgs {
  val help_message: String =
    """
Program:
Program Arguments:
    (--type, -t) {nnf, dnf, cnf}
        | decide, to which form do need to normalize
        | default = nnf
    (--help, -h)
        | print this message
    (--file, -f) <PATH>
        | read formula from file
    (--input, -i)
        | read formula from standard input
    (--disable-hello-msg, -d)
        | disable hello message
Symbols used:
    ^, &&, &     : Conjunction
    v, ||, |     : Disjunction
    ~, !         : Negation
    =>, ->       : Consequence
    <=>, <->, == : Equivalence
    True, T      : True
    False, F     : False""".stripMargin
  ///TODO make True False
  def filterTypeInputSymb(symb: Symbol): Boolean = {
    symb == pathSymb | symb == inputSymb
  }

  def isValidType(str: String): Boolean = {
    val symb = Symbol(str)
    symb == nnf | symb == dnf | symb == cnf
  }
  def getTypeForm(str: String): Symbol = {
    Symbol(str)
  }

  @tailrec
  def nextOption(options: Options, list: List[String]): Options = {
    list match {
      case Nil => options
      case ("--type" | "-t") :: typeForm :: tail =>
        if (!isValidType(typeForm)) {
          println("Invalid type: " ++ typeForm)
          println("Type --help for all commands")
          sys.exit(2)
        }
        nextOption(options ++ Map(typeSymb -> getTypeForm(typeForm)), tail)
      case ("--help" | "-h") :: tail              => nextOption(options ++ Map(helpSymb -> true), tail)
      case ("--file" | "-f") :: path :: tail      => nextOption(options ++ Map(pathSymb -> path), tail)
      case ("--input" | "-i") :: tail             => nextOption(options ++ Map(inputSymb -> true), tail)
      case ("--disable-hello-msg" | "-d") :: tail => nextOption(options ++ Map(disableSymb -> true), tail)
      case option :: _ =>
        println("Unknown option: " ++ option)
        println("Type --help for all commands")
        sys.exit(1)
    }
  }

  type Options = Map[Symbol, Any]
  def parseArgs(args: List[String]): Options = {
    val options = nextOption(Map(), args)
    if (!(options contains typeSymb)) {
      options(typeSymb) = nnf
    }
    options
  }
}


