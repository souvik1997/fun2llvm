package pd

import fastparse.all._
import Syntax._
import scala.Left
import scala.Right

/*
 * Implements the parser part of pd, which is responsible for taking the raw FUN source
 * and producing a nice Abstract Syntax Tree out of it.
 *
 * The parser can generally deal with whitespace, and does so by eliminating any whitespace at the beginning
 * of the file
 */
object FunParser {
    /*
     * The most simple form of parsing - take in an input string, and produce an Abstract Syntax Tree of the
     * resulting program.
     */
    def parse(input: String) : Either[String, Syntax.Program] = program.parse(input) match {
        case Parsed.Success(value, index) => Right(value)
        case fail@Parsed.Failure(parser, index, error) => Left(error.traced.trace)
    }

    // Here, we get to define the set of parsers we use, combined via lovely parser combinators.

    // Fundamental parsing of whitespace characters, which we use in all of our fundamental parser-combinators.
    def whitespace : P0 = P(CharIn(List(' ', '\t', '\n'))) // Matches a single whitespace character
    def whitespaceRep : P0 = P(whitespace.rep) // Matches 0 or more whitespace characters.

    // Parses a number as a series of digits, and then maps it to a long. Must have at least 1 digit. Whitespace aware.
    def number : P[Long] = P(whitespaceRep ~ CharIn('0' to '9').rep(1).!).map(_.toLong)

    // Parses an identifier as an alphabetic character followed by a series of alphanumeric characters. Whitespace aware.
    def identifier : P[String] = P(whitespaceRep ~ CharIn('a' to 'z', 'A' to 'Z').! ~ CharIn('a' to 'z', 'A' to 'Z', '0' to '9').rep.!)
      .map { case (left, right) => left + right }

    // Converts a character into a whitespace-aware character.
    def char(ch: Char) : P0 = P(whitespaceRep ~ s"$ch")


    // Now, we can define a lovely parser which builds up a program.

    // Parses a Constant as a numeric literal, consuming whitespace around it.
    def constant : P[Constant] = number.map(Syntax.Constant)

    // Parses a variable as an identifier, consuming whitespace around it.
    def variable : P[Variable] = identifier.map(Syntax.Variable)

    /*
     * Constructs a simple parser which passes if the token found was the specified token, and fails otherwise.
     * Automatically eliminates whitespace.
     */
    def token(target: String) : P0 = P(whitespaceRep ~ target)

    // Constructs a parser for a specific precedence, which looks for 1 or more instances of the subexpression
    // separated by one of the provided operators, and then maps the operators to their appropriate expression using
    // the provided function.
    def createPrecedenceParser(sub_expr: P[Expression])(operators: (String, (Expression, Expression) => Expression)*) = {
        // Create a map of our operator to operator -> expression functions, and also create a list for us to match on.
        val operator_map = operators.toMap
        val operator_list = operator_map.keys.toSeq

        // Then, create the parser. It looks for at least 1 sub expression, followed by 0 or more operators and
        // more expressions.
        val op_parser = P(sub_expr ~ (whitespaceRep ~ StringIn(operator_list : _*).! ~/ sub_expr).rep)

        // Finally, we create the final parser by folding over the list of (operator, expression) pairs which we parse.
        // If no pairs are present, then this is equivalent to just using the lower-level parser directly.
        // Otherwise, it has the effect of building up the abstract syntax tree from left to right, using the provided
        // functions in the mapping.
        op_parser.map { case ((left, opList)) =>
            opList.foldLeft(left) { case (accum, (op, next)) => operator_map(op)(accum, next) }
        }
    }

    // Useful for delimiting comma-seperated lists.
    def commaSeperator : P0 = char(',')

    // Resolves precedence due to parenthesis, and automatically consumes whitespace as well.
    def parens : P[Expression] = P(char('(') ~/ expression ~/ char(')'))

    // Resolves a function call of the form <identifier>(expr1, ...)
    def functionCall : P[Expression] = P(identifier ~ char('(') ~/ expression.rep(sep = commaSeperator) ~/ char(')'))
      .map { case ((name, params)) => Call(name, params.toList) }

    // The highest precedence is either a constant, a variable, or a paren-wrapped expression.
    def expr_pred0 : P[Expression] = P(functionCall | constant | variable | parens)

    // Followed by multiplicative stuff
    def expr_pred1 : P[Expression] = createPrecedenceParser(expr_pred0)("*" -> Multiplication)

    // Followed by additive stuff
    def expr_pred2 : P[Expression] = createPrecedenceParser(expr_pred1)("+" -> Addition)

    // Followed finally by comparison operators.
    def expr_pred3 : P[Expression] = createPrecedenceParser(expr_pred2)(
        "==" -> Equal,
        "<>" -> NotEqual,
        "<" -> LessThan,
        ">" -> GreaterThan
    )

    // A full expression is then just the highest precedence parser.
    def expression : P[Expression] = expr_pred3

    // Now, we handle control constructs (eg, statements.)
    // A print statement just has an expression to print. Note we use whitespace.rep(1) as we
    // want at least 1 whitespace element.
    def printStatement : P[Print] = P(token("print") ~/ expression ~/ char(';').?).map(Print)

    // An assign statement has a variable followed by an equals sign and an expression.
    def assignStatement : P[Assign] = P(variable ~ char('=') ~/ expression ~/ char(';').?)
      .map(Assign.tupled)

    // A return statement has a "return" token followed by an expression.
    def returnStatement : P[Return] = P(token("return") ~/ expression ~/ char(';').?).map(Return)

    // An if statement has an expression, a true block, and an optional else with a false block.
    def ifStatement : P[If] = P(token("if") ~/ expression ~/ statement ~/ (token("else") ~/ statement).? ~/ char(';').?).map {
        case (expr, trueBlock, falseBlock) => If(expr, trueBlock, falseBlock.getOrElse(NoOp))
	  }

    // A while statement has an expression and a block.
    def whileStatement : P[While] = P(token("while") ~/ expression ~/ statement ~/ char(';').?).map(While.tupled)

    // A sequence of statements is a set of repeated statements, where we have at least 1 statement.
    def sequence: P[Sequence] = P(char('{') ~/ statement.rep ~/ char('}'))
      .map(seq => Sequence(seq.toList))

    // A statement is any of the above statements.
    def statement : P[Statement] = P(printStatement | assignStatement | returnStatement | ifStatement | whileStatement | sequence)

    // Finally, we can construct functions and the program as a whole.
    // A function is a "fun" keyword, followed by an identifier and a comma seperated arguments list, followed by
    // a statement.

    def function : P[Function] = P(token("fun") ~/ whitespaceRep ~/ identifier.! ~/ whitespaceRep ~ char('(') ~/ variable.rep(sep = commaSeperator) ~/ char(')') ~/ statement)
        .map { case (name, vars, statement) => Function(name, vars.toList, statement) }

    // A full program parser, which attempts to parse the entire file as a program.
    def program = P(Start ~ whitespaceRep ~ function.rep ~ whitespaceRep ~ End).map { case functions => Program(functions.toList) }
}
