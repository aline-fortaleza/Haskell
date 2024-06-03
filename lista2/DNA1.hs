data Tree t = Node t (Tree t) (Tree t) | Nilt
    deriving (Read, Show)

dna1 :: Tree Int -> [String]
dna1 arv = listToString (listToEight (treeToList arv))


--1. Transformar de uma árvore para uma lista de inteiros
treeToList :: Tree Int -> [Int]
treeToList Nilt = []
treeToList (Node a arv1 arv2) = treeToList arv1 ++ [a] ++ treeToList arv2

-- 2. Transformar essa lista em uma lista de listas com tam 8 (a última tendo o que restar)
listToEight :: [Int] -> [[Int]]
listToEight [] = []
listToEight (a:as) | length (a:as) <= 8 = [a:as]
                   | otherwise = listToEightAUX (a:as) 0 : listToEight (takeEight (a:as) 0 )

listToEightAUX :: [Int] -> Int -> [Int] -- retorna os 8 primiros elementos de uma lista
listToEightAUX [] _ = []
listToEightAUX (a:as) n | n < 8 = a : listToEightAUX as (n + 1) 
                        | otherwise = [] 


takeEight :: [Int] -> Int -> [Int] -- tira os oito primeiros elementos de uma lista
takeEight [] _ = []
takeEight (a:as) n | n < 7 = takeEight as (n + 1)
                   | otherwise = as 

--3. transformar os números em strings
listToString :: [[Int]] -> [String]
listToString [] = []
listToString ((a:as):xs) = listToStringAUX (a:as) : listToString xs  

listToStringAUX :: [Int] -> String
listToStringAUX [] = []
listToStringAUX (a:as) | a `mod` 5 == 0 = 'E' : listToStringAUX as 
                       | a `mod` 5 == 1 = 'M' : listToStringAUX as 
                       | a `mod` 5 == 2 = 'A' : listToStringAUX as 
                       | a `mod` 5 == 3 = 'C' : listToStringAUX as 
                       | a `mod` 5 == 4 = 'S' : listToStringAUX as 