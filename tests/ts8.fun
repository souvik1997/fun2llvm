fun main()
{
glob = 10
print f1(f2(),f3())
}

fun f1(a,b)
{
print a + b
}

fun f2()
{
glob = glob + 1
print glob
return 13
}

fun f3()
{
glob = glob * 100
print glob
return 12
}