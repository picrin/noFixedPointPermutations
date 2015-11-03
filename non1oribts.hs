factorial :: Integer -> Integer
factorial n = (product [1 .. n])

nChoosek :: Integer -> Integer -> Integer
nChoosek n k = div (product [(k + 1) .. n]) (product [1 .. (n - k)])

-- number of no fixed point permutations, Kurkiewicz
nfppK :: Integer -> Integer
nfppK 0 = 1
nfppK n = factorial n - sum(map (\k -> (nChoosek n k) * (nfppK (n - k))) [1 .. n])

-- number of no fixed point permutation, Euler
nfppE :: Integer -> Integer
nfppE 0 = 1
nfppE n = n * (nfppE (n - 1)) + (-1)^n

kurkiewicz = map nfppK [1 .. ]
euler = map nfppE [1 .. ]


