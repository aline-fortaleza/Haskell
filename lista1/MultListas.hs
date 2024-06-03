mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = [] -- caso das duas vazias (fim da interação que tem o mesmo tamanho)
mul2 [] (b:bs) = [0| x <- (b:bs)] 
mul2 (a:as) [] = [0| x <- (a:as)] 
mul2 (a:as) (b:bs) = (a*b) : (mul2 as bs)

