-- Questão 1
fibonacciAux :: Int -> Int
fibonacciAux 0 = 1
fibonacciAux 1 = 1
fibonacciAux n = fibonacciAux (n-2) + fibonacciAux (n-1) 


fibonacci :: [Int]
fibonacci = [fibonacciAux x | x <- [0..]]

-- Questão 2

merge :: Ord t => [t] -> [t] -> [t]
merge [] [] = []
merge [] list2 = list2
merge list1 [] = list1
merge (a:as) (b:bs) | a <= b = a : merge as (b:bs)
                    | otherwise = b : merge (a:as) bs



-- Questão 3
-- Ficou faltando resolver o problema dos parenteses
type Pilha t = [t]
data Elemento = Valor Int | Soma | Multiplica deriving (Show) 
-- ((1*2) + (3*4)) 1 2 mult 3 4 mult soma 
geraString :: Pilha Elemento -> String
geraString [] = []
geraString (Valor a:Soma:xs) = "+" ++ (show a) ++ ")" ++ geraString xs -- ++ ")"
geraString (Valor a:Multiplica:xs) = "*" ++ (show a) ++ ")" ++ geraString xs -- ++ ")"
geraString (Valor a:Valor b:Soma:xs) = "(" ++ "(" ++ (show a ) ++ "+" ++ (show b) ++ ")" ++ geraString xs
geraString (Valor a:Valor b:Multiplica:xs) = "(" ++ "(" ++ (show a) ++ "*" ++ (show b) ++ ")" ++ geraString xs
geraString (a:as) = []

-- Questão 4 

calcula :: Pilha Elemento -> Int
calcula [] = 0
calcula [Valor v] = v
calcula (Valor v1 : Valor v2 : Soma : cauda) = calcula (Valor (v1+v2): cauda)
calcula (Valor v1 : Valor v2 : Multiplica : cauda) = calcula (Valor (v1*v2): cauda)
calcula (Valor v1 : Valor v2 : Valor v3 : Multiplica : cauda) = calcula(Valor v1 : Valor (v2*v3): cauda)
calcula (Valor v1 : Valor v2 : Valor v3 : Soma : cauda) = calcula(Valor v1 : Valor (v2+v3): cauda)
