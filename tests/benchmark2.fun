fun main()
{
        print sum(0, 10000)
}

fun sum(start, limit)
{
        if (start == limit)
           return start
        return sum(start + 1, limit) + start
}
