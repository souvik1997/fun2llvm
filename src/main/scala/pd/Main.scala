package pd

import Syntax._
import fastparse.all._
import CodeGen._
import scopt._
import java.io.File
import java.io.PrintStream
import pd.Syntax
import pd.CodeGen
import pd.FunParser
/*
 * Primary entry-point of the compiler; takes input from files and then passes it through
 * the parser, optimizer, and then LLVM generator.
 */
object Main {
    def main(args: Array[String]) = {

        val parser = new scopt.OptionParser[Config]("scopt") {
            head("fun2llvm", "1.0")
            opt[File]('i', "input") required() valueName("<file>") action { (x,c) => c.copy(input = x) }
            opt[File]('o', "output") optional() valueName("<file>") action { (x,c) => c.copy(output = x) }
        }
        parser.parse(args, Config()) match {
            case Some(config) => {
                val fileInput = io.Source.fromFile(config.input).getLines.mkString("\n")
                val outputStream = if (config.output.getName().equals("-")) System.out else new PrintStream(config.output)
                FunParser.parse(fileInput) match {
                    case Right(x) => {
                        CodeGen.generate(x, outputStream, io.Source.fromFile(config.lib).getLines.mkString("\n"))
                    }
                    case Left(x) => println(s"Failed to parse ${config.input}\n$x")
                }
            }
            case None => {
                println("error")
            }

        }
    }
}

case class Config(input: File = new File("-"), output: File = new File("-"), lib: File = new File("lib/libfun.ll"))
