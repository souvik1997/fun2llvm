package pd

import scala.collection.immutable._

/*
 * Represents the actual set of constructs which define the FUN language.
 */
object Syntax {
    // The container type used to store sequences of things.
    type ListOf[T] = List[T]

    /*
     * Top-level object - contains a list of functions.
     */
    case class Program(val functions: ListOf[Function]) {
        /*
         * Maps each function into a list of output functions, which can be empty, contain one function,
         * or contain many functions.
         */
        def flatMapFunctions(f: Function => ListOf[Function]) : Program = this.copy(functions = functions.flatMap(f))

        /*
         * Maps each function in the program to another function.
         */
        def mapFunctions(f: Function => Function) : Program = this.copy(functions = functions.map(f))

        /*
         * Folds over the functions in the program, with a given initial value. Folds from Left -> Right, in order of
         * the functions original definition in the file (or some other compiler-defined order).
         */
        def foldLeft[A](initial: A)(f: (A, Function) => A) : A = functions.foldLeft(initial)(f)

        /*
         * Applies a (presumably side-effecting) function to each function in this.
         */
        def foreach[A](f: Function => A) : Unit = functions.foreach(f)
    }

    // Represents a function, which has a name, a scope, and a statement body.
    case class Function(val name: String, val arguments: ListOf[Variable], val body: Statement) {

        /*
         * Transforms this function by applying the given function to it, and returns the new function
         * produced.
         */
        def transform(f: Function => Function) : Function = f(this)

        /*
         * Applies some function to the statement of this function, returning a new function with the
         * mapped statement.
         */
        def mapStatement(f: Statement => Statement) : Function = this.copy(body = f(body))
    }



    // A generic representation of some kind of "statement" which can be executed for it's side effects.
    sealed trait Statement {
        /*
         * Transforms this statement by applying the given function to it, and returns the new statement produced.
         */
        def transform(f: Statement => Statement) : Statement = f(this)

        /*
         * Transforms the immediate children of this statement with the given function,
         * returning the statement with the transformed children.
         */
        def transformChildren(f: Statement => Statement) : Statement

        /*
         * Transforms all of the children of this statement from the bottom of the tree to the top
         * of the tree, which garuantees that all child nodes are processed before a parent node is processed.
         */
        def transformChildrenUp(f: Statement => Statement) : Statement =
            transformChildrenUp(child => child.transformChildrenUp(f)).transform(f)
    }

    // Represents a sequenced block of statements which should be executed one after the other.
    case class Sequence(val statements: ListOf[Statement]) extends Statement {
        def transformChildren(f: Statement => Statement) = Sequence(statements.map(f))
    }

    // For printing out the result of an expression.
    case class Print(val value: Expression) extends Statement {
        def transformChildren(f: Statement => Statement) = this
    }

    // For assigning a variable the result of an expression.
    case class Assign(val variable: Variable, val value: Expression) extends Statement {
        def transformChildren(f: Statement => Statement) = this
    }

    // For returning the result of an expression from a function immediately.
    case class Return(val value: Expression) extends Statement {
        def transformChildren(f: Statement => Statement) = this
    }

    // For branching based on a predicate to two possible statements.
    case class If(val predicate: Expression, val trueBody: Statement, val falseBody: Statement) extends Statement {
        def transformChildren(f: Statement => Statement) = If(predicate, f(trueBody), f(falseBody))
    }

    // Continually executes the statement while the predicate is still true.
    case class While(val predicate: Expression, val body: Statement) extends Statement {
        def transformChildren(f: Statement => Statement) = While(predicate, f(body))
    }

    // A no-op, which does nothing.
    case object NoOp extends Statement {
        def transformChildren(f: Statement => Statement) = this
    }

    // A generic representation of an expression, which yields a value given a context (eg, variable mappings.)
    sealed trait Expression {
        /*
         * Transforms this expression by applying the given function to it, and returns the created expression.
         */
        def transform(f: Expression => Expression) : Expression = f(this)

        /*
         * Transforms the immediate children of this expression by the given function,
         * and returns this expression with the mapped children.
         */
        def transformChildren(f: Expression => Expression) : Expression

        /*
         * Maps the function from the bottom of the tree up to the top of the expression tree;
         * this garuantees that the transforming function will be called on the current node only
         * after it has been called on all of the children.
         */
        def transformChildrenUp(f: Expression => Expression) : Expression =
            transformChildren(child => child.transformChildrenUp(f)).transform(f)
    }

    // Represents a constant value.
    case class Constant(val value: Long) extends Expression {
        def transformChildren(f: Expression => Expression) = this
    }

    // Represents a variable which may have a dynamic value at runtime.
    case class Variable(val name: String) extends Expression {
        def transformChildren(f: Expression => Expression) = this
    }

    // Represents a function call with a specific set of parameters.
    case class Call(val function: String, val parameters: ListOf[Expression]) extends Expression {
        def transformChildren(f: Expression => Expression) = Call(function, parameters.map(f))
    }

    // Represents the set of binary operations between two subexpressions.
    sealed abstract class BinaryOperation(val left: Expression, val right: Expression) extends Expression

    // Represents integer addition between two subexpressions.
    case class Addition(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right) {
        def transformChildren(f: Expression => Expression) = Addition(f(left), f(right))
    }

    // Represents integer multiplication between two subexpressions.
    case class Multiplication(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right) {
        def transformChildren(f: Expression => Expression) = Multiplication(f(left), f(right))
    }

    // Represents equality between two subexpressions.
    case class Equal(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right) {
        def transformChildren(f: Expression => Expression) = Equal(f(left), f(right))
    }

    // Represents a less-than "<" check, to see if the left expression is less than the second.
    case class LessThan(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right) {
        def transformChildren(f: Expression => Expression) = LessThan(f(left), f(right))
    }

    // Represents a greater-than ">" check, to see if the left expression is greater than the second.
    case class GreaterThan(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right) {
        def transformChildren(f: Expression => Expression) = GreaterThan(f(left), f(right))
    }

    // Represents a not-equal "<>" check, to see if the results of the expressions are not equal.
    case class NotEqual(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right) {
        def transformChildren(f: Expression => Expression) = NotEqual(f(left), f(right))
    }
}
