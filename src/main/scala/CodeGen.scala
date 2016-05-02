package pd

import Syntax._

import java.io.PrintStream

/*
 * Responsible for taking a syntax tree and converting it to
 * the LLVM IR language.
 */
object CodeGen {


    def indentedPrintln(indent: Int, output: PrintStream, str: String): Unit = output.println((" "*indent) + str)

    def generate(program: Program, output: PrintStream, librarySrc: String) : Unit = {
        program.functions.foreach(func => generateFunction(func, output, 0))

        indentedPrintln(0, output, librarySrc)
    }

    /*
     Since variables in LLVM IR are immutable, we can't store directly to local variables.
     The solution: Immediately copy arguments to local variables, and use load/store to access and modify them.

     Suppose a local variable is called "xyz". Its local copy is "_xyz"
     */
    def generateFunction(function: Function, output: PrintStream, indent: Int) : Unit = {
        val formattedArguments = function.arguments.map("i64 %" + _.name).mkString(", ")

        implicit val context: CodeGenContext = CodeGenContext(function, output, indent, 1)

        // TODO: The return type might be void or similar
        Context.emit(s"define i64 @${function.name}($formattedArguments) {")

        function.arguments.foreach { arg =>
            val local = "_" + arg.name

            Context.increaseIndent(4) {
                Context.emit(s"%${local} = alloca i64")
                Context.emit(s"store i64 %${arg.name}, i64* ${local}")
            }
        }

        generateStatement(function.body)

        Context.increaseIndent(4) {
            Context.emit(s"ret i64 0")
        }

        Context.emit("}")
    }

    // NOTE: Question marks should be replaced by appropriate code for printing the LLVM code.
    // Some extra metadata might need to be collected, such as global variable information.
    // We can have generateExpression return a tuple which also marks any global variables used,
    // and then print out all of the global variables at the very end like we probably all did in p3/p4/p5.

    /*
     * Generate a statement, writing the results to a print stream with the provided indentation.
     */
    def generateStatement(statement: Statement)(implicit context: CodeGenContext) : Unit = Context.increaseIndent(4) {
        statement match {
            case Sequence(statements) => Context.increaseIndent(-4) { statements.foreach(generateStatement) }

            case Print(value) => {
                val res_reg = generateExpression(value)
                Context.emit(s"call void @printNum (i64 %${res_reg})")
            }

            case Assign(variable, value) => {
                val res_reg = generateExpression(value)
                val prefix = if(Context.isGlobalVariable(variable.name)) "%_" else "@"
                Context.emit(s"store i64 %${res_reg}, i64* ${prefix}${variable.name}")
            }

            case Return(value) => {
                val res_reg = generateExpression(value)
                Context.emit(s"ret i64 %${res_reg}")
            }

            case If(pred, trueBody, falseBody) => Context.withTempVar { truncatedCondition =>
                val pred_reg = generateExpression(pred)
                Context.emit(s"%${truncatedCondition} = trunc i64 %${pred_reg} to i1")

                val labelPrefix = s"__${context.function.name}_if_${truncatedCondition}"
                val labelPrefixThen = s"${labelPrefix}_then"
                val labelPrefixElse = s"${labelPrefix}_else"

                Context.emit(s"br i1 %${truncatedCondition}, label ${labelPrefixThen}, label ${labelPrefixElse}")

                Context.emit(s"${labelPrefixThen}:")
                generateStatement(trueBody)

                Context.emit(s"${labelPrefixElse}:")
                generateStatement(falseBody)
            }
            case While(pred, body) => Context.withTempVar { truncatedCondition =>
                val pred_reg = generateExpression(pred)
                Context.emit(s"%${truncatedCondition} = trunc i64 %${pred_reg} to i1")

                val labelPrefix = s"__${context.function.name}_while_${truncatedCondition}"
                val labelPrefixBegin = s"${labelPrefix}_begin"
                val labelPrefixEnd = s"${labelPrefix}_end"

                Context.emit(s"${labelPrefixBegin}:")
                Context.emit(s"br i1 %${truncatedCondition}, label ${labelPrefixBegin}, label ${labelPrefixEnd}")
                generateStatement(body)

                Context.emit(s"br label ${labelPrefixBegin}")
                Context.emit(s"${labelPrefixEnd}:")
            }
            case NoOp => () // Emit nothing for No-op
        }
    }

    /*
     * Generates the LLVM IR for an expression, and returns the temporary variable that contains the value of this expression
     */
    def generateExpression(expr: Expression)(implicit context: CodeGenContext) : Int = expr match {
        case Constant(value) => Context.yieldTempVar { temp =>
            Context.emit(s"%${temp} = add i64 0, ${value}")
        }

        case Variable(name) => Context.yieldTempVar { temp =>
            val prefix = if(Context.isGlobalVariable(name)) "%_" else "@"
            Context.emit(s"%${temp} = load i64, i64* ${prefix}${name}")
        }

        case Call(function, params) => Context.yieldTempVar { temp =>
            val args = params.map(p => "i64 %" + generateExpression(p)).mkString(", ")
            Context.emit(s"%${temp} = call i64 @${function.trim} (${args})")
        }

        case Addition(left, right) => Context.yieldTempVar { temp =>
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.emit(s"%${temp} = add i64 %${ltmp}, %${rtmp}")
        }

        case Multiplication(left, right) => Context.yieldTempVar { temp =>
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.emit(s"%${temp} = mul i64 %${ltmp}, %${rtmp}")
        }

        case Equal(left, right) => Context.yieldTempVar2 { (temp1, temp2) =>
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.emit(s"%${temp1} = icmp eq i64 %${ltmp}, %${rtmp}")
            Context.emit(s"%${temp2} = zext i1 ${temp1} to i64")
        }

        case LessThan(left, right) => Context.yieldTempVar2 { (temp1, temp2) =>
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.emit(s"%${temp1} = icmp ult u64 %${ltmp}, %${rtmp}")
            Context.emit(s"%${temp2} = zext i1 ${temp1} to i64")
        }

        case GreaterThan(left, right) => Context.yieldTempVar2 { (temp1, temp2) =>
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.emit(s"%${temp1} = icmp ugt u64 %${ltmp}, %${rtmp}")
            Context.emit(s"%${temp2} = zext i1 ${temp1} to i64")
        }

        case NotEqual(left, right) => Context.yieldTempVar2 { (temp1, temp2) =>
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.emit(s"%${temp1} = icmp ne i64 %${ltmp}, %${rtmp}")
            Context.emit(s"%${temp2} = zext i1 ${temp1} to i64")
        }
    }

    // Utility functions and classes for cleaner emission.
    case class CodeGenContext(function: Function, output: PrintStream, var indent: Int, var nextTempVar: Int)

    object Context {
        /*
         * Executes the provided function by providing it a single temporary variable, and updates the context.
         */
        def withTempVar[A](f: Int => A)(implicit context: CodeGenContext) : A = {
            context.nextTempVar = context.nextTempVar + 1
            f(context.nextTempVar - 1)
        }

        /*
         * Executes the provided function by providing it two temporary variables, and updates the context.
         */
        def withTempVar2[A](f: (Int, Int) => A)(implicit context: CodeGenContext) : A = {
            context.nextTempVar = context.nextTempVar + 2
            f(context.nextTempVar - 2, context.nextTempVar - 1)
        }

        /*
         * Executes the provided function by providing it three temporary variables, and updates the context.
         */
        def withTempVar3[A](f: (Int, Int, Int) => A)(implicit context: CodeGenContext) : A = {
            context.nextTempVar = context.nextTempVar + 3
            f(context.nextTempVar - 3, context.nextTempVar - 2, context.nextTempVar - 1)
        }

        // Exactly the same as the with- functions, but returns the last temporary variable automatically.

        def yieldTempVar[A](f: Int => A)(implicit context: CodeGenContext) =
            { withTempVar(f); context.nextTempVar - 1 }

        def yieldTempVar2[A](f: (Int, Int) => A)(implicit context: CodeGenContext) =
            { withTempVar2(f); context.nextTempVar - 1 }

        def yieldTempVar3[A](f: (Int, Int, Int) => A)(implicit context: CodeGenContext) =
            { withTempVar3(f); context.nextTempVar - 1 }

        /*
         * Emits the provided LLVM.
         */
        def emit(llvm: String)(implicit context: CodeGenContext) : Unit =
            indentedPrintln(context.indent, context.output, llvm)

        /*
         * Temporary increases the indent by the specified amount for the function provided.
         */
        def increaseIndent[A](amount: Int)(f: => A)(implicit context: CodeGenContext) = {
            context.indent += amount
            f
            context.indent -= amount
        }

        /*
         * Returns true if the provided string is a global variable, and false otherwise.
         */
        def isGlobalVariable(name: String)(implicit context: CodeGenContext) : Boolean =
            context.function.arguments.count(_.name == name) > 0
    }

}
