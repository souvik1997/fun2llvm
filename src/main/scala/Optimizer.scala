package pd

import Syntax._
/*
 * Responsible for taking the AST and providing simple optimizations.
 */
object Optimizer {

    /*
     * Given a program, optimizes and returns a new Abstract Syntax Tree representing the optimized program.
     */
    def optimize(program: Program): Program = program.mapFunctions(optimizeFunction)

    /*
     * Optimizes a function and returns a new, optimized function.
     */
    def optimizeFunction(function: Function): Function = function.mapStatement(optimizeStatement)

    /*
     * Optimizes a statement recursively, by optimizing all children and then optimizing the provided statement.
     */
    def optimizeStatement(root: Statement) : Statement = root.transformChildrenUp(optimizeSingleStatement)

    /*
     * Optimizes an expression recursively, by optimizing all children and then optimizing the provided statement.
     */
    def optimizeExpr(root: Expression) : Expression = root.transformChildrenUp(optimizeSingleExpr)

    /*
     * Optimizes a statement and eturns a new, optimized statement.
     */
    protected def optimizeSingleStatement(statement: Statement): Statement = statement match {
        case Sequence(statements) => statement
        case Print(expr) => Print(optimizeExpr(expr))
        case Assign(variable, expr) => Assign(variable, optimizeExpr(expr))
        case Return(expr) => Return(optimizeExpr(expr))

        case If(predicate, trueBody, falseBody) => optimizeExpr(predicate) match {
            case Constant(0) => falseBody
            case Constant(_) => trueBody
            case optimizedPred => If(optimizedPred, trueBody, falseBody)
        }

        case While(predicate, body) => optimizeExpr(predicate) match {
            case Constant(0) => NoOp
            case optimizedPred => While(optimizedPred, body)
        }

        case NoOp => NoOp
    }


    protected def optimizeSingleExpr(expr: Expression): Expression = expr match {
        /*
         * Optimizes addition by doing constant folding and addition identities.
         */
        case Addition(left, right) => (left, right) match {
            case (Constant(x), Constant(y)) => Constant(x + y)
            case (Constant(0), right) => right
            case (left, Constant(0)) => left
            case _ => expr
        }

        /*
         * Optimizes multiplication by doing constant folding and multiplication indentities/idempotence laws.
         */
        case Multiplication(left, right) => (left, right) match {
            case (Constant(x), Constant(y)) => Constant(x * y)
            case (Constant(1), right) => right
            case (left, Constant(1)) => left
            case (Constant(0), right) => Constant(0)
            case (left, Constant(0)) => Constant(0)
            case _ => expr
        }

        /*
         * Optimizes each of the possible equalities. The "tautology" term exists to optimze the case
         * where the same variable is used and the result depends on whether we are processing an
         * equality or inequality.
         */
        case Equal(left, right) => optimizeEquality(left, right, _ == _, true)
        case LessThan(left, right) => optimizeEquality(left, right, _ < _, false)
        case GreaterThan(left, right) => optimizeEquality(left, right, _ > _, false)
        case NotEqual(left, right) => optimizeEquality(left, right, _ != _, false)

        case _ => expr
    }

    /*
   * Provides a generic equality optimizer, which takes an equality operation. The tautology
   * boolean is returned when two of the same variable are compared against each other.
   */
    def optimizeEquality(left: Expression, right: Expression, operation: (Long, Long) => Boolean,
        tautology: Boolean): Expression = (left, right) match {
        case (Constant(x), Constant(y)) if operation(x, y) => Constant(1)
        case (Constant(x), Constant(y)) if !operation(x, y) => Constant(0)
        case (Variable(var1), Variable(var2)) if var1 == var2 => Constant(if (tautology) 1 else 0)
    }
}
