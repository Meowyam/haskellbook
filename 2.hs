piSquare x = pi * (x * x)

area x = 3.14 * (x * x)

double x = x * 2

x = 7
y = 10
f = x + y

plusY = x * 3 + y
  where x = 3
        y = 1000

multi5 = x * 5
  where y = 10
        x = 10 * 5 + y

divX = z / x + y
  where x = 7
        y = negate x
        z = y * 10

-- 2.11 chapter exercises

first = 2 + (2 * 3) - 1
second = 10 ^ (1 + 1)
third = ((2^2) * (4^5)) + 1

-- equivalent expressions: 1, 2, 3, (5 is wrong for obv reasons, 4 is wrong because div is an integral function)

waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

triple x = x * 3

waxOff x = triple x
