fibonacciAux :: Int -> Int
fibonacciAux 0 = 1
fibonacciAux 1 = 1
fibonacciAux n = fibonacciAux (n-2) + fibonacciAux (n-1) 


fibonacci :: [Int]
fibonacci = [fibonacciAux x | x <- [0..]]



primes :: [Int]
primes = auxPrimes [2..]

auxPrimes :: [Int] -> [Int]
auxPrimes (x:xs) = x : filter (\y -> y `mod` x /= 0 ) (auxPrimes xs) 