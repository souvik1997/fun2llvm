package pd

/*
 * Responsible for taking a syntax tree and converting it to
 * the LLVM IR language.
 */
object CodeGen {
    def generate(program: Syntax.Program) = program.functions.map(generateFunction)

    def generateFunction(function: Syntax.Function): Unit = {
        System.out.printf("define i64 @%s (%s)%n { \n %s \n}", function.name, function.arguments.map(_.name).mkString(","), generateExpression(function.body))
    }

    def generateExpression(body: Syntax.Statement): Unit = {
        if (body.isInstanceOf[Syntax.Sequence]) {
            body.asInstanceOf[Syntax.Sequence].statements.map(generateExpression _)
        }
        /*else if (body.isInstanceOf[Syntax.Print]): Unit = {
            /* TODO.... */
        }*/

    }
}
