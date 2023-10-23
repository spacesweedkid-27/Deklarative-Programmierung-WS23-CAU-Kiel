-- 0.1
intSum1 :: Int -> Int
intSum1 n = div (n * (n + 1)) 2

-- 0.2
intSum2 :: Int -> Int
intSum2 0 = 0
intSum2 n = n + intSum2(n - 1)

-- 1
-- (Screenshot 2023-10-23 140047.png)

-- 2.1
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

binom :: Int -> Int -> Int
binom n k = div (fac n) (fac k * fac (n - k))

-- 2.2
pascal :: Int -> Int -> Int
pascal n k
    | k == 0 = 1
    | k == n = 1
    | n > 0 && k > 0 && k <= n = pascal (n-1) (k-1) + pascal (n-1) k
    | otherwise = undefined

-- 3.1
nthPowerTwo :: Int -> Int
nthPowerTwo 0 = 1
nthPowerTwo n = 2 * nthPowerTwo (n-1)

-- 3.2
isPowerTwo :: Int -> Bool
isPowerTwo 1 = True
isPowerTwo n = even n && isPowerTwo (div n 2)

-- 3.3
-- caculates log_2 of n floored
intLog2 :: Int -> Int
intLog2 1 = 0
intLog2 n = intLog2 (div n 2) + 1

roundUpToPowerTwo :: Int -> Int
roundUpToPowerTwo n
    | isPowerTwo n = n
    | otherwise = nthPowerTwo (intLog2 n + 1)