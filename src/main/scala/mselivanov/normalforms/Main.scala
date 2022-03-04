package mselivanov.normalforms
import ParserArgs.{parseArgs, help_message, filterTypeInputSymb}
import Normalizer.normalize
import Constants._

import scala.io.{BufferedSource, Source}

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
        //TODO make write to output/file
      }
      buffSource.close
    }
    if (options contains inputSymb) {
      //TODO write hello msg and read from input
      //TODO write to output/file
    }
  }
}
