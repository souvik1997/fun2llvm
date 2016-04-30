package pd

import Syntax._

import java.io.PrintStream

/*
 * Responsible for taking a syntax tree and converting it to
 * the LLVM IR language.
 */
object CodeGen {

    var curTemp: Int = 0
    def getNextTempVar(): Int = { curTemp += 1; curTemp }

    def indentedPrintln(indent: Int, output: PrintStream, str: String): Unit = output.println((" "*indent) + str)

    def generate(program: Program, output: PrintStream) : Unit = program.functions.foreach(func => generateFunction(func, output, 0))

    def generateFunction(function: Function, output: PrintStream, indent: Int): Unit = {
        val formattedArguments = function.arguments.map("u64 " + _.name).mkString(",")

        // TODO: The return type might be void or similar
        output.println(s"define u64 @${function.name}($formattedArguments) ")

        generateStatement(function, function.body, output, indent + 4)
    }

    // NOTE: Question marks should be replaced by appropriate code for printing the LLVM code.
    // Some extra metadata might need to be collected, such as global variable information.
    // We can have generateExpression return a tuple which also marks any global variables used,
    // and then print out all of the global variables at the very end like we probably all did in p3/p4/p5.

    /*
     * Generate a statement, writing the results to a print stream with the provided indentation.
     */
    def generateStatement(context: Function, body: Statement, output: PrintStream, indent: Int): Unit = body match {
        case Sequence(statements) => statements.foreach(state => generateStatement(context, state, output, indent))
        case Print(value) =>
        case Assign(variable, value) => ???
        case Return(value) => ???
        case If(pred, trueBody, falseBody) => ??? // Falsebody could be NoOp, so don't emit if it is
        case While(pred, body) => ???
        case NoOp => () // Emit nothing for No-op
    }

    /*
     * Generates the LLVM IR for an expression, and produces the next available temporary variable.
     */
    def generateExpression(context: Function, expr: Expression, output: PrintStream,
        indent: Int) : Int = {
        val tempVariable = getNextTempVar()
        expr match {
            case Constant(value) => indentedPrintln(indent, output, "%${tempVariable} = add i64 0, ${value}")
            case Variable(name) => {
                val prefix = if (context.arguments.contains(name)) "%" else "@"
                indentedPrintln(indent, output, "%${tempVariable} = load i64, i64* ${prefix}${name}")
            }
            case Call(function, params) => {
                val args = params.map(p => "%"+generateExpression(context, p, output, indent))
                indentedPrintln(indent, output, "${tempVariable} = call i64 ${function} (${args})")
            }

            case Addition(left, right) => {
                val ltmp = generateExpression(context, left, output, indent)
                val rtmp = generateExpression(context, left, output, indent)
                indentedPrintln(indent, output, "${tempVariable} = add i64 %${ltmp}, %${rtmp}")
            }
            case Multiplication(left, right) => {
                val ltmp = generateExpression(context, left, output, indent)
                val rtmp = generateExpression(context, left, output, indent)
                indentedPrintln(indent, output, "${tempVariable} = mul i64 %${ltmp}, %${rtmp}")
            }
            case Equal(left, right) => ???
            case LessThan(left, right) => ???
            case GreaterThan(left, right) => ???
            case NotEqual(left, right) => ???
        }
        return tempVariable
    }

}
