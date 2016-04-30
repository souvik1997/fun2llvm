package pd

/*
 * Responsible for taking a syntax tree and converting it to
 * the LLVM IR language.
 */
object CodeGen {
    def generate(program: Syntax.Program) = program.functions.map(generateFunction)

    def generateFunction(function: Syntax.Function) = {
        System.out.printf("define i64 @%s (%s)", function.name, function.arguments.map((v : Syntax.Variable) => v.name).mkString(","))
    }
}
