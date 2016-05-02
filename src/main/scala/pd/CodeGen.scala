package pd

import Syntax._
import java.io.PrintStream

/*
 * Responsible for taking a syntax tree and converting it to
 * the LLVM IR language.
 */
object CodeGen {

    def generate(program: Program, output: PrintStream, librarySrc: String) : Unit = {
        val globalVariables = collection.mutable.Set.empty[String]
        program.functions.foreach { func =>
            implicit val context: CodeGenContext = CodeGenContext(program, func, output, 0, 1, globalVariables)
            generateFunction(func)(context)
        }
        
        // Output global variables
        globalVariables.foreach { variable =>
            indentedPrintln(0, output, s"@${variable} = global i64 0")
        }

        // Output library files
        indentedPrintln(0, output, librarySrc)
    }

    /*
     Since variables in LLVM IR are immutable, we can't store directly to local variables.
     The solution: Immediately copy arguments to local variables, and use load/store to access and modify them.

     Suppose a local variable is called "xyz". Its local copy is "_xyz"
     */
    def generateFunction(function: Function)(implicit context: CodeGenContext) : Unit = {
        val formattedArguments = function.arguments.map("i64 %" + _.name).mkString(", ")

        // TODO: The return type might be void or similar
        Context.emit(s"define i64 @${function.name}($formattedArguments) {")

        function.arguments.foreach { arg =>
            val local = "_" + arg.name

            Context.increaseIndent(4) {
                Context.emit(s"%${local} = alloca i64")
                Context.emit(s"store i64 %${arg.name}, i64* %${local}")
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
                val prefix = if(Context.isGlobalVariable(variable.name)) "@" else "%_"

                if(Context.isGlobalVariable(variable.name))
                    Context.addGlobalVariable(variable.name)

                Context.emit(s"store i64 %${res_reg}, i64* ${prefix}${variable.name}")
            }

            case Return(value) => {
                val res_reg = generateExpression(value)
                Context.emit(s"ret i64 %${res_reg}")
                
                // This lovely thing is due to return creating a new Basic Block, so we need to index appropriately.
                Context.wasteTempVar
            }

            case If(pred, trueBody, falseBody) => {
                val pred_reg = generateExpression(pred)
                Context.withTempVar { truncatedCondition =>
                    Context.emit(s"%${truncatedCondition} = trunc i64 %${pred_reg} to i1")

                    val labelPrefix = s"__${context.function.name}_if_${truncatedCondition}"
                    val labelPrefixThen = s"${labelPrefix}_then"
                    val labelPrefixElse = s"${labelPrefix}_else"
                    val labelPrefixEnd = s"${labelPrefix}_end"
                    
                    Context.emit(s"br i1 %${truncatedCondition}, label %${labelPrefixThen}, label %${labelPrefixElse}")

                    Context.emit(s"${labelPrefixThen}:")
                    generateStatement(trueBody)
                    Context.emit(s"br label %${labelPrefixEnd}")

                    Context.emit(s"${labelPrefixElse}:")
                    generateStatement(falseBody)
                    Context.emit(s"br label %${labelPrefixEnd}")
                    
                    Context.emit(s"${labelPrefixEnd}:")
                }
            }
            case While(pred, body) =>  {
                val pred_reg = generateExpression(pred)
                Context.withTempVar2 { (truncatedCondition, _) =>
                    Context.emit(s"%${truncatedCondition} = trunc i64 %${pred_reg} to i1")

                    val labelPrefix = s"__${context.function.name}_while_${truncatedCondition}"
                    val labelPrefixBegin = s"${labelPrefix}_begin"
                    val labelPrefixEnd = s"${labelPrefix}_end"
                    
                    Context.emit(s"br label %${labelPrefixBegin}")

                    Context.emit(s"${labelPrefixBegin}:")
                    Context.emit(s"br i1 %${truncatedCondition}, label %${labelPrefixBegin}, label %${labelPrefixEnd}")
                    generateStatement(body)

                    Context.emit(s"br label %${labelPrefixBegin}")
                    Context.emit(s"${labelPrefixEnd}:")
                }
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
            val prefix = if(Context.isGlobalVariable(name)) "@" else "%_"

            if(Context.isGlobalVariable(name))
                Context.addGlobalVariable(name)

            Context.emit(s"%${temp} = load i64* ${prefix}${name}")
        }

        case Call(functionName, params) => {
            // For call, we need to eliminate extra parameters.
            val actualFunction = context.program.functions.find(_.name == functionName)
            
            // This is how great our error handling is.
            if(!actualFunction.isDefined) 
                System.err.println("Calling a nonexistent function...?")
                
            val argCount = actualFunction.map(_.arguments.size).getOrElse(0)
            
            val args = params.take(argCount).map(p => "i64 %" + generateExpression(p)).mkString(", ")
            Context.yieldTempVar { temp =>
                Context.emit(s"%${temp} = call i64 @${functionName.trim} (${args})")
            }
        }

        case Addition(left, right) => {
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.yieldTempVar { temp =>
                Context.emit(s"%${temp} = add i64 %${ltmp}, %${rtmp}")
            }
        }

        case Multiplication(left, right) => {
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.yieldTempVar { temp =>
                Context.emit(s"%${temp} = mul i64 %${ltmp}, %${rtmp}")
            }
        }

        case Equal(left, right) => {
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.yieldTempVar2 { (temp1, temp2) =>
                Context.emit(s"%${temp1} = icmp eq i64 %${ltmp}, %${rtmp}")
                Context.emit(s"%${temp2} = zext i1 %${temp1} to i64")
            }
        }

        case LessThan(left, right) => {
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.yieldTempVar2 { (temp1, temp2) =>
                Context.emit(s"%${temp1} = icmp ult i64 %${ltmp}, %${rtmp}")
                Context.emit(s"%${temp2} = zext i1 %${temp1} to i64")
            }
        }

        case GreaterThan(left, right) => {
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.yieldTempVar2 { (temp1, temp2) =>
                Context.emit(s"%${temp1} = icmp ugt i64 %${ltmp}, %${rtmp}")
                Context.emit(s"%${temp2} = zext i1 %${temp1} to i64")
            }
        }

        case NotEqual(left, right) => {
            val (ltmp, rtmp) = (generateExpression(left), generateExpression(right))
            Context.yieldTempVar2 { (temp1, temp2) =>
                Context.emit(s"%${temp1} = icmp ne i64 %${ltmp}, %${rtmp}")
                Context.emit(s"%${temp2} = zext i1 %${temp1} to i64")
            }
        }
    }

    // Utility functions and classes for cleaner emission.
    case class CodeGenContext(program: Program, function: Function, output: PrintStream, var indent: Int, var nextTempVar: Int,
                                val globalVariables: collection.mutable.Set[String])

    def indentedPrintln(indent: Int, output: PrintStream, str: String): Unit = output.println((" " * indent) + str)

    object Context {
        def wasteTempVar(implicit context: CodeGenContext) = {
            context.nextTempVar += 1
        }

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
            !context.function.arguments.exists(_.name == name)
            
        def addGlobalVariable(name: String)(implicit context: CodeGenContext) : Unit =
            context.globalVariables.add(name)
    }

}
