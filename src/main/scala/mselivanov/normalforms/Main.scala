package mselivanov.normalforms

import ParserArgs.{parseArgs, help_message, filterTypeInputSymb}
import Normalizer.normalize
import Constants._
import scala.io.{BufferedSource, Source}
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    val options = parseArgs(args.toList)
    if ((options contains helpSymb) | (options.view.filterKeys(filterTypeInputSymb).isEmpty)) {
      println(help_message)
      sys.exit(0)
    }
    if (options contains pathSymb) {
      val buffSource: BufferedSource = Source.fromFile((options get pathSymb).asInstanceOf[String])
      for (line <- buffSource.getLines) {
        val normForm = normalize((options get typeSymb).asInstanceOf[Symbol], line)

        //Turn out, scala don't have built-in output in files...
        println(normForm)
      }
      buffSource.close
    }
    if (options contains inputSymb) {
      if (!(options contains disableSymb)) {
        println("You can exit this program, if you enter empty line")
        println("Please, enter your formula below: ")
      }
      while (true) {
        val formula = readLine()
        val normForm = (options get typeSymb match {
          case Some(s: Symbol) => normalize(s, formula)
          case _ => throw new RuntimeException("didnt get type operation from options")
        })
        println(normForm)
      }
    }
  }
}
