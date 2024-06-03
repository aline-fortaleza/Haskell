palavrasFrequentes :: [String] -> [String]
palavrasFrequentes list = firstThree (qSortTupl (sortLen (countQtd (strToTupl (qSort list)))))



--1. Botar em ordem alfabetica (vai juntar as palavras iguais)
qSort :: [String] -> [String]
qSort [] = []
qSort (a:as) = qSort [x | x <- as, x < a] ++ [a] ++ qSort [x | x <- as, x >= a]

--2. criar a função que transforma para [(String,Int)] com o contador
strToTupl :: [String] -> [(String,Int)]
strToTupl as = map (\ a -> (a, 0)) as

--3. faz uma função que conta até ser diferente
countQtd :: [(String,Int)] -> [(String,Int)] -- recebe a string ordenada
countQtd [] = []
countQtd [(str, num)] = [(str, num + 1)]
countQtd ((str,num):(str2,num2):xs) | str == str2 = countQtd ((str2,num2 + num + 1):xs)
                                    | otherwise = (str, num + 1) : countQtd ((str2,num2):xs)



-- ordenar por quantidade de aparições
qSortTupl :: [(String, Int)] -> [String] 
qSortTupl [] = []
qSortTupl [(a,b)] = [a]
qSortTupl ((str,num):(str2,num2):xs) = qSortTupl [(str3,num3) | (str3,num3) <- ((str2,num2):xs), num3 > num ] ++ [str] ++ qSortTupl [(str3,num3) | (str3,num3) <- ((str2,num2):xs), num3 <= num ]

-- testa os valores iguais, se for igual manda pra ordenar por tamanho
{-seIgual :: [(String,Int)] -> [String]
seIgual [] = []
seIgual [(a,b)] = [a] 
seIgual [(str,num),(str2,num2)] | num == num2 = sortLen [(str,num),(str2,num2)]
                                | otherwise = [str,str2]
seIgual ((str,num):(str2,num2):(str3,num3):xs) | num == num2 && num2 == num3 = sortLen [(str,num),(str2,num2),(str3,num3)] ++ seIgual xs
                                               | num == num2 = sortLen [(str,num),(str2,num2)] ++ seIgual ((str3,num3):xs) 
                                               | otherwise = str : seIgual ((str2,num2):(str3,num3):xs) -}

-- ordenar por tamanho da String 
sortLen :: [(String,Int)] -> [(String,Int)]
sortLen [] = []
sortLen [(a,b)] = [(a,b)]
sortLen ((str,num):(str2,num2):xs) = sortLen [(str3,num3) | (str3,num3) <- ((str2,num2):xs), (length str3) <= (length str)] ++ [(str,num)] ++  sortLen [(str3,num3) | (str3,num3) <- ((str2,num2):xs), (length str3) > (length str) ]
{-sortLen [(str,num),(str2,num2)] | length str > length str2  = [str2,str]
                                | otherwise = [str,str2] -}


firstThree :: [String] -> [String]
firstThree [] = []
firstThree [a] = [a]
firstThree [a,b] = [a,b]
firstThree (a:b:c:xs) = [a,b,c] 