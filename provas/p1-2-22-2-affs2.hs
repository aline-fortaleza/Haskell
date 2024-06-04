-- ALine FOrtaleza (affs2)

-- Questão 1
rlencode0 :: [Int] -> [Int]
rlencode0 [] = []
rlencode0 (a:as) | a == 0 = [0] ++ rlencode0AUX (a:as) 0 ++ rlencode0 (rlencode0Rest (a:as)) -- coloca zero, sua quantidade e chama a recursão sem os zeros do inicio
                 | otherwise = a : rlencode0 as

rlencode0AUX :: [Int] -> Int -> [Int] -- conta quantos zeros tem no inicio de uma lista
rlencode0AUX [] a | a == 0 = [] 
                  | otherwise = [a]
rlencode0AUX (b:bs) a | b == 0 = rlencode0AUX bs (a+1)
                      | otherwise = [a]

rlencode0Rest :: [Int] -> [Int] -- tirar os zeros do início
rlencode0Rest [] = []
rlencode0Rest (a:as) | a == 0 = rlencode0Rest as
                     | otherwise = a:as


rldecode0 :: [Int] -> [Int] 
rldecode0 [] = []
rldecode0 (0:b:cauda) = rldecode0AUX b ++ rldecode0 cauda -- se o inicio da lista tiver 0, printa 0 b vezes e chama a recursão
rldecode0 (a:as) = a : rldecode0 as -- se o primeiro n for zero, so coloca ele de novo e roda a recursão

rldecode0AUX :: Int -> [Int] -- devolve uma lista com n zeros
rldecode0AUX 0 = []
rldecode0AUX n = 0 : rldecode0AUX (n-1)

-- Questão 2

rlencodeLetras :: String -> String
rlencodeLetras [] = [] -- caso base
rlencodeLetras (a:b:as)| a == b = [a] ++ show (countIguais (a:b:as) 1) ++ rlencodeLetras (tirarIguais (a:b:as)) -- quando é igual coloca o char, mostra quantas vezes aparece e chama a recursão co a lista sem os iguais do inicio
                       | otherwise = a : rlencodeLetras (b:as) -- se diferente, chama a recursão com a lista a partir do 2°
rlencodeLetras (a:as) = a : rlencodeLetras as -- caso pra cobrir as possibilidades de input errado no fim

countIguais :: String -> Int -> Int -- conta quantos chars iguais tem no inicio de uma string
countIguais [] count = count
countIguais (a:b:as) count | a==b = countIguais (b:as) (count + 1)
                           | otherwise = count
countIguais (a:as) count = count

tirarIguais :: String -> String -- tirar um conjunto de chars iguais no inicio de uma string 
tirarIguais [] = []
tirarIguais (a:b:as) | a == b = tirarIguais (b:as)
                     | otherwise = b:as
tirarIguais (a:as) = tirarIguais as

rldecodeLetras :: String -> String
rldecodeLetras [] = []
rldecodeLetras (a:b:cauda) | b >= '2' && b <= '9' = repeatChar a (read [b] :: Int) ++ rldecodeLetras cauda -- se tiver entre o intervalo 2 e 9 da tabela ascii, repete o char anterior essa qtd de vezes e chama a recusão
                           | otherwise = a : rldecodeLetras (b:cauda) -- se não for número, coloca no inicio e chama a recursão
rldecodeLetras (a:as) = a : rldecodeLetras as -- caso pra cobrir as possibilidades de input errado no fim

repeatChar :: Char -> Int -> String -- repete um dado char uma certa quantidade de vezes
repeatChar a 0 = []
repeatChar a n = a : repeatChar a (n-1)

-- Questão 3 
data Letra = Unica Char | Repetida Char Int deriving Show

rlencodeLetrasCodigo :: String -> [Letra] -- logica analoga a anterior com as mesamas funções auxiliares
rlencodeLetrasCodigo [] = []
rlencodeLetrasCodigo (a:b:cauda) | a == b = Repetida a  (countIguais (a:b:cauda) 1) : rlencodeLetrasCodigo (tirarIguais (a:b:cauda))
                                 | otherwise = Unica a : rlencodeLetrasCodigo (b:cauda)
rlencodeLetrasCodigo (a:as) = Unica a : rlencodeLetrasCodigo as

rldecodeLetrasCodigo :: [Letra] -> String -- logica analoga a anterior com as mesamas funções auxiliares
rldecodeLetrasCodigo [] = []
rldecodeLetrasCodigo (Unica a: cauda) = a : rldecodeLetrasCodigo cauda
rldecodeLetrasCodigo (Repetida b n: cauda) = repeatChar b n ++ rldecodeLetrasCodigo cauda