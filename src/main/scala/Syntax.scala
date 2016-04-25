/*
 * Represents the actual set of constructs which define the FUN language.
 */
object Syntax {
	// The container type used to store sequences of things.
	type ListOf[T] = List[T]

	/*
	 * Top-level object - contains a list of functions.
	 */
	case class Program(val functions: ListOf[Function])

	// Represents a function, which has a name, a scope, and a statement body.
	case class Function(val name: String, val arguments: ListOf[Variable], val body: Statement)



	// A generic representation of some kind of "statement" which can be executed for it's side effects.
	sealed trait Statement

	// Represents a sequenced block of statements which should be executed one after the other.
	case class Sequence(val statements: ListOf[Statement]) extends Statement

	// For printing out the result of an expression.
	case class Print(val value: Expression) extends Statement

	// For assigning a variable the result of an expression.
	case class Assign(val variable: Variable, val value: Expression) extends Statement

	// For returning the result of an expression from a function immediately.
	case class Return(val value: Expression) extends Statement

	// For branching based on a predicate to two possible statements.
	case class If(val predicate: Expression, val trueBody: Statement, val falseBody: Statement) extends Statement

	// Continually executes the statement while the predicate is still true.
	case class While(val predicate: Expression, val body: Statement) extends Statement

	// A no-op, which does nothing.
	case object NoOp extends Statement

	// A generic representation of an expression, which yields a value given a context (eg, variable mappings.)
	sealed trait Expression

	// Represents a constant value.
	case class Constant(val value: Long) extends Expression

	// Represents a variable which may have a dynamic value at runtime.
	case class Variable(val name: String) extends Expression
	
	// Represents a function call with a specific set of parameters.
	case class Call(val function: String, val parameters: ListOf[Expression]) extends Expression

	// Represents the set of binary operations between two subexpressions.
	sealed class BinaryOperation(val left: Expression, val right: Expression) extends Expression

	// Represents integer addition between two subexpressions.
	case class Addition(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right)

	// Represents integer multiplication between two subexpressions.
	case class Multiplication(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right)

	// Represents equality between two subexpressions.
	case class Equal(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right)

	// Represents a less-than "<" check, to see if the left expression is less than the second.
	case class LessThan(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right)

	// Represents a greater-than ">" check, to see if the left expression is greater than the second.
	case class GreaterThan(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right)

	// Represents a not-equal "<>" check, to see if the results of the expressions are not equal.
	case class NotEqual(override val left: Expression, override val right: Expression) extends BinaryOperation(left, right)
}
