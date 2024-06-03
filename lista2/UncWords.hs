-- "Uma palavra é incomum se aparecer exatamente uma vez em uma das frases e não aparecer na outra frase."

--"As palavras não diferenciam letras maiúsculas de minúsculas, portanto, "Banana" é o mesmo que "banana""

--"Dadas duas sentenças s1 e s2 , retorne uma lista de todas as palavras incomuns em letras minúsculas em ordem alfabética"

uncommonFromTwoSentences :: String -> String -> [String]
uncommonFromTwoSentences str1 str2 = uncommonAUX (qSort (senToList (toLowerString str1 str2)) ) 0
--1. Transformar tudo das duas frases em minusculo
toLowerString :: String -> String -> [String]
toLowerString str1 str2 = [map toLowerChar str1, map toLowerChar str2]

toLowerChar :: Char -> Char 
toLowerChar c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
              | otherwise = c 
--2. Separar as strings em uma lista de lista de strings (cada frase em uma lista de palavras)
senToList :: [String] -> [String]
senToList [] = []
senToList [str1,str2] = (senToListInd str1 "") ++ (senToListInd str2 "") 

senToListInd :: String -> String -> [String] -- segunda string é um acc, inicia vazio
senToListInd [] acc = [acc]
senToListInd (a:as) acc | a == ' ' = acc : senToListInd as [] 
                        | otherwise = senToListInd as (acc ++ [a])


--3. Ordenar as listas internas em ordem alfabetica 
{-ordList :: [[String]] -> [[String]]
ordList [] = []
ordList [(a:as),(b:bs)] = [qSort (a:as), qSort (b:bs)] -}

qSort :: [String] -> [String]
qSort [] = []
qSort (a:as) = qSort [x | x <- as, x < a] ++ [a] ++ qSort [x | x <- as, x >= a]
--4. Comparar as palavras com todas as palavras de todas as listas (pq ela tb n pode ser igual a nenhuma palavra da lista que tá) se for igual a alguma descarta chamando a recursão, se não acumula e depois chama a recursão

uncommonAUX :: [String] -> Int -> [String] -- int é uma flag
uncommonAUX [] _ = []
uncommonAUX [a] 0 = [a]
uncommonAUX [a] 1 = []
uncommonAUX (a:b:as) 0 | a == b = uncommonAUX (b:as) 1
                     | otherwise = a : uncommonAUX (b:as) 0 -- se a palavra não apareceu antes e nem agora, como ta ordeado não vai mais aparecer
uncommonAUX (a:b:as) 1 | a == b = uncommonAUX (b:as) 1
                     | otherwise = uncommonAUX (b:as) 0