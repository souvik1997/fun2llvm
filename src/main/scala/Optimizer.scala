
/*
 * Responsible for taking the AST and providing simple optimizations.
 */
object Optimizer {
    // This optimizer is so good that we don't even do anything.
    def optimize(program: Syntax.Program) : Syntax.Program = program
}
