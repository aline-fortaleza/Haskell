fatPrime :: Int -> [(Int, Int)]
fatPrime 0 = [(0,1)]
fatPrime n = fatAux n 2


fatAux :: Int -> Int -> [(Int,Int)] -- o numero e o divisor da vez (inicializa dois)
fatAux 1 _ = []
fatAux n divisor 
        |n < 2 = []
        |n `mod` divisor == 0 = (divisor, divCount n divisor) : fatAux (n `div` (divisor ^(divCount n divisor) )) (nextDivisor divisor)
        |otherwise = fatAux n (nextDivisor divisor)

divCount :: Int -> Int -> Int -- conta quantas vezes um número aparece em uma divisão, primeiro num o que vai ser fatorado e o segundo o num que ta dividindo
divCount x i | x < i = 0
        | x `mod` i == 0 = 1 + (divCount (x `div` i) i)
        | otherwise = 0     

nextDivisor :: Int -> Int
nextDivisor 2 = 3
nextDivisor d = d + 2




