fun main()
{
print fib(1, 1, 0, 1)
print fib(1, 1, 0, 2)
print fib(1, 1, 0, 3)
print fib(1, 1, 0, 4)
print fib(1, 1, 0, 5)
print fib(1, 1, 0, 6)
print fib(1, 1, 0, 7)
print fib(1, 1, 0, 8)
print fib(1, 1, 0, 9)
print fib(1, 1, 0, 10)
print fib(1, 1, 0, 11)
print fib(1, 1, 0, 12)
print fib(1, 1, 0, 13)
}

fun fib(a, b, c, n)
{
if c == n
return b
return fib(b, a+b, c+1, n)
}