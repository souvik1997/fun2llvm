

/*
 * Primary entry-point of the compiler; takes input from files and then passes it through
 * the parser, optimizer, and then LLVM generator.
 */
object Main {
    def main(args: Array[String]) = {
        System.out.println(args(0));
	// Some easier tests
	System.out.println(FunParser.constant.parse("12"));
	System.out.println(FunParser.constant.parse("1298123"))

	System.out.println(FunParser.variable.parse("x"))
	System.out.println(FunParser.variable.parse("x123143yz"))

	System.out.println(FunParser.expr_pred1.parse("1 * 2"))
	System.out.println(FunParser.expr_pred2.parse("1 * 2 + 3"))
	System.out.println(FunParser.expr_pred2.parse("1 + 2"))

	System.out.println(FunParser.expression.parse("1 + x + (2 * y)"))
	System.out.println(FunParser.expression.parse("(((2))) + ((((((9) + 8)))) * 2)"))


	val fileSource = io.Source.fromFile("tests/t1.fun")
	// Hardcoding tests is a magical thing.
	val sourceCode = fileSource.getLines.mkString("\n")

	System.out.println(FunParser.parse(sourceCode))

	fileSource.close()
    }
}
