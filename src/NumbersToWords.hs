units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["zero", "ten", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]

link :: Int -> String
link n = if (n < 100)
         then " and "
         else " "

-- Units - 0's
convert1 :: Int -> String
convert1 n = units !! n

-- Tens - 00's
convert2 :: Int -> String
convert2 n
    | t == 0        = convert1 u
    | t == 1        = teens !! u
    | u == 0        = tens !! t
    | otherwise     = tens !! t ++ "-" ++ convert1 u
    where (t, u)    = (n `div` 10, n `mod` 10)

-- Hundreds - 000's
convert3 :: Int -> String
convert3 n
    | h == 0        = convert2 n
    | t == 0        = convert1 h ++ " hundred"
    | otherwise     = convert1 h ++ " hundred" ++ link t ++ convert2 t
    where (h, t) = (n `div` 100, n `mod` 100)

-- Thousands - 000,000's
convert6 :: Int -> String
convert6 n
    | k == 0        = convert3 n
    | h == 0        = convert3 k ++ " thousand"
    | otherwise     = convert3 k ++ " thousand" ++ link h ++ convert3 h
    where (k, h)    = (n `div` 1000, n `mod` 1000)

-- Millions - 000,000,000's 
convert9 :: Int -> String
convert9 n
    | m == 0        = convert6 n
    | k == 0        = convert3 m ++ " million"
    | otherwise     = convert3 m ++ " million" ++ link k ++ convert6 k
    where (m, k)    = (n `div` 1000000, n `mod` 1000000)

-- Billions - 000,000,000,000's
convert12 :: Int -> String
convert12 n
    | b == 0        = convert9 n
    | m == 0        = convert3 b ++ " billion"
    | otherwise     = convert3 b ++ " billion" ++ link m ++ convert9 m
    where (b, m)    = (n `div` 1000000000, n `mod` 1000000000)

numbersToWords = convert12