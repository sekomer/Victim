{-
    Lexical scoping in Victim
-}

fn fact(n)
{
    if (n < 2) return 1;
    return n * fact(n - 1);
}

print("fact 5 is:", fact(5));
var a := fact;
print("fact 6 is:", a(6));


fn odd (n)
{
    if (n == 0)
        return false;
    return even(n - 1);
}

fn even (n)
{
    if (n == 0)
        return true;
    return odd(n - 1);
}

print("is 5 odd?", odd(5));

