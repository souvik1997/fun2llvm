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

    /*
     Since variables in LLVM IR are immutable, we can't store directly to local variables.
     The solution: Immediately copy arguments to local variables, and use load/store to access and modify them.

     Suppose a local variable is called "xyz". It's local copy is "_xyz"
     */
    def generateFunction(function: Function, output: PrintStream, indent: Int): Unit = {
        val formattedArguments = function.arguments.map("u64 " + _.name).mkString(",")

        // TODO: The return type might be void or similar
        output.println(s"define u64 @${function.name}($formattedArguments) ")
        function.arguments.foreach(f => {
            val local = "_" + f.name
            indentedPrintln(indent + 4, output, s"%${local} = alloca u64")
            indentedPrintln(indent + 4, output, s"store u64 %${f} u64* ${local}")
        })
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
        case Print(value) => {
            val res = generateExpression(context, value, output, indent)
            val junk = getNextTempVar()
            indentedPrintln(indent, output, s"%${junk} = call printNum (${res})") // TODO: Implement the printNum function
        }
        case Assign(variable, value) => {
            val res = generateExpression(context, value, output, indent)
            indentedPrintln(indent, output, s"store u64 %${res}, u64* %_${variable.name}")
        }
        case Return(value) => ???
        case If(pred, trueBody, falseBody) => ??? // Falsebody could be NoOp, so don't emit if it is
        case While(pred, body) => ???
        case NoOp => () // Emit nothing for No-op
    }

    /*
     * Generates the LLVM IR for an expression, and returns the temporary variable that contains the value of this expression
     */
    def generateExpression(context: Function, expr: Expression, output: PrintStream,
        indent: Int) : Int = {
        val tempVariable = getNextTempVar()
        expr match {
            case Constant(value) => indentedPrintln(indent, output, s"%${tempVariable} = add u64 0, ${value}")
            case Variable(name) => {
                val prefix = if (context.arguments.contains(name)) "%" else "@"
                indentedPrintln(indent, output, s"%${tempVariable} = load u64, u64* ${prefix}${name}")
            }
            case Call(function, params) => {
                val args = params.map(p => "%"+generateExpression(context, p, output, indent))
                indentedPrintln(indent, output, s"${tempVariable} = call u64 ${function} (${args})")
            }

            case Addition(left, right) => {
                val ltmp = generateExpression(context, left, output, indent)
                val rtmp = generateExpression(context, right, output, indent)
                indentedPrintln(indent, output, s"%${tempVariable} = add u64 %${ltmp}, %${rtmp}")
            }
            case Multiplication(left, right) => {
                val ltmp = generateExpression(context, left, output, indent)
                val rtmp = generateExpression(context, right, output, indent)
                indentedPrintln(indent, output, s"%${tempVariable} = mul u64 %${ltmp}, %${rtmp}")
            }
            case Equal(left, right) => {
                val intermediate = getNextTempVar()
                val ltmp = generateExpression(context, left, output, indent)
                val rtmp = generateExpression(context, right, output, indent)
                indentedPrintln(indent, output, s"%${intermediate} = icmp eq u64 %${ltmp}, %${rtmp}")
                indentedPrintln(indent, output, s"%${tempVariable} = zext i1 ${intermediate} to u64")
            }
            case LessThan(left, right) => {
                val intermediate = getNextTempVar()
                val ltmp = generateExpression(context, left, output, indent)
                val rtmp = generateExpression(context, right, output, indent)
                indentedPrintln(indent, output, s"%${intermediate} = icmp ult u64 %${ltmp}, %${rtmp}")
                indentedPrintln(indent, output, s"%${tempVariable} = zext i1 ${intermediate} to u64")
            }
            case GreaterThan(left, right) => {
                val intermediate = getNextTempVar()
                val ltmp = generateExpression(context, left, output, indent)
                val rtmp = generateExpression(context, right, output, indent)
                indentedPrintln(indent, output, s"%${intermediate} = icmp ugt u64 %${ltmp}, %${rtmp}")
                indentedPrintln(indent, output, s"%${tempVariable} = zext i1 ${intermediate} to u64")
            }
            case NotEqual(left, right) => {
                val intermediate = getNextTempVar()
                val ltmp = generateExpression(context, left, output, indent)
                val rtmp = generateExpression(context, right, output, indent)
                indentedPrintln(indent, output, s"%${intermediate} = icmp ne u64 %${ltmp}, %${rtmp}")
                indentedPrintln(indent, output, s"%${tempVariable} = zext i1 ${intermediate} to u64")
            }
        }
        return tempVariable
    }

}
