fun main(x)
{
print a(0, a(4, 6))
}

fun a(ax, a)
{
        if a > 20
        {
                print ax
                print a
                return ax+a
        }
        return a(ax+1, a+1);
}