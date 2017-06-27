main :: IO()
main = do
    print (hanoi 2 "a" "b" "c")

-- EXERCISE 1

-- lists of the digits in n
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- list of the digits in n, in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = lastDigit n : toDigitsRev (div (n - lastDigit n) 10)

lastDigit :: Integer -> Integer
lastDigit n = rem n 10

-- EXERCISE 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther [x] = [x]
doubleEveryOther xs  = reverse (doubleEveryOtherRev (reverse xs))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []         = []
doubleEveryOtherRev [x]        = [x]
doubleEveryOtherRev (x:(y:zs)) = x:(y*2):doubleEveryOtherRev zs

-- EXERCISE 3

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits [x]    = x
sumDigits (x:xs) = sum(toDigits x) + sumDigits xs

-- EXERCISE 4

validate :: Integer -> Bool
validate n = rem (sumDigits (doubleEveryOther (toDigits n))) 10 == 0

-- EXERCISE 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
