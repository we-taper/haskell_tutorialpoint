sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
primes = sieve [2..]

main = do
    print(primes !! 7000)