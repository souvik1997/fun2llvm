package pd

import Syntax._
import fastparse.all._
import CodeGen._

/*
 * Primary entry-point of the compiler; takes input from files and then passes it through
 * the parser, optimizer, and then LLVM generator.
 */
object Main {
    def main(args: Array[String]) = {
        // Some easier tests
        val Parsed.Success(expr, _) = FunParser.expr_pred2.parse("1 * 2")
        val Parsed.Success(expr2, _) = FunParser.expr_pred2.parse("1 * 2 + 3")
        val Parsed.Success(expr3, _) = FunParser.expr_pred2.parse("1 + 2")
        val result = FunParser.parse("fun main(a, b) \n { \n}")
        result match {
            case Right(x) => { CodeGen.generate(x) }
            case Left(x) => { println("Error " + x) }
        }

        println(expr + " -> " + Optimizer.optimizeExpr(expr))
        println(expr2 + " -> " + Optimizer.optimizeExpr(expr2))
        println(expr3 + " -> " + Optimizer.optimizeExpr(expr3))

        for (num <- 0 to 7) {
            // Check that all of the files parse successfully.
            val fileInput = io.Source.fromFile(s"tests/t$num.fun").getLines.mkString("\n")

            FunParser.parse(fileInput) match {
                case Right(x) => {
                    println(s"t$num ... pass")
                    CodeGen.generate(x)
                }
                case Left(x) => println(s"t$num ... fail\n$x")
            }
        }
    }
}
