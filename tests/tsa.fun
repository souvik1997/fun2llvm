fun f(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
{
print 32
}

fun g(f,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
{
print f + f(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
}

fun gg(gg, a, b, c, d)
{
print gg + g(gg, a, b, c, d, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) + a + b + c*d
}

fun loop()
{
counter = 0
while counter < 10000
{
print counter
counter = counter + 1
tmp = gg(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)
}
}

fun main()
{
print loop()
}