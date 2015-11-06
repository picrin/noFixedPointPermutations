-- factorial n is the number of unique permutations of n-element set.
factorial :: Integer -> Integer
factorial n = (product [1 .. n])

-- nChoosek n k is the number of unique subsets of size k of universe n.
nChoosek :: Integer -> Integer -> Integer
nChoosek n k = div (product [(k + 1) .. n]) (product [1 .. (n - k)])

-- number of no fixed point permutations, Kurkiewicz.
nfppK :: Integer -> Integer
nfppK 0 = 1
nfppK n = factorial n - sum(map (\k -> (nChoosek n k) * (nfppK (n - k))) [1 .. n])

-- number of no fixed point pemutation, Kurkiewicz2.
nfppK2 :: Integer -> Integer
nfppK2 0 = 1
nfppK2 1 = 0
nfppK2 n = (nfppK2 (n - 1)) * (n - 1) + (nfppK2 (n - 2)) * (n - 1)

-- number of no fixed point permutation, Euler.
nfppE :: Integer -> Integer
nfppE 0 = 1
nfppE n = n * (nfppE (n - 1)) + (-1)^n


kurkiewicz = map nfppK [1 .. ]
euler = map nfppE [1 .. ]
kurkiewicz2 = map nfppK2 [1 ..]

range = 4

main :: IO ()
main = print (((take range euler) == (take range kurkiewicz)) && ((take range euler) == (take range kurkiewicz2)))

