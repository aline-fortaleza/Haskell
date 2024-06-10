-- Questão 1

type Chave = [(Char,Char)]

letras :: [Char]
letras = ['A'..'Z'] ++ ['A'..'Z']

cria_chave :: Int -> Chave
cria_chave n = cria_chaveAUX n letras


cria_chaveAUX :: Int -> [Char] -> Chave
cria_chaveAUX 0 (a:as) = (a,a) : cria_chaveAUX 0 as 
cria_chaveAUX n [] = [] 
cria_chaveAUX n (a:as) = (a, andarQtd n a letras) : cria_chaveAUX n as

 

andarQtd :: Int -> Char -> [Char] -> Char
andarQtd 0 a str = a 
andarQtd n a [] = andarQtd n a letras
andarQtd n a (x:xs) | a == x = head (andarQtdAUX (n-1) xs)
                    |otherwise = andarQtd n a xs

andarQtdAUX :: Int -> [Char] -> [Char]
andarQtdAUX 0 (a:as) = [a]
andarQtdAUX 0 [] = []
andarQtdAUX n [] = andarQtdAUX (n-1) letras
andarQtdAUX n (a:as) = andarQtdAUX (n-1) as 


-- Questão 2

crypt :: Chave -> String -> String 
crypt chv [] = []
crypt chv (x:xs) = encontrarIgual chv x : crypt chv xs 

encontrarIgual :: Chave -> Char -> Char
encontrarIgual  [] x = x
encontrarIgual ((a,b) :cauda) x | a == x = b
                                | otherwise = encontrarIgual cauda x 


-- Questão 3

data ChaveTree = Node Char Char ChaveTree ChaveTree | Leaf deriving (Show) 

cryptTree :: ChaveTree -> String -> String 
cryptTree tree [] = []
cryptTree tree (x:xs) = encontrarIgualTree tree x : cryptTree tree xs

chave_parcial :: ChaveTree
chave_parcial = Node 'I' 'L' (Node 'A' 'D' Leaf Leaf) (Node 'L' 'O' Leaf Leaf)

encontrarIgualTree :: ChaveTree -> Char -> Char
encontrarIgualTree  Leaf x = x
encontrarIgualTree (Node a b tree1 tree2) x | x == a = b
                                            | x < a = encontrarIgualTree tree1 x
                                            | otherwise = encontrarIgualTree tree2 x


-- Questão 4

ctree_to_chave :: ChaveTree -> Chave 
ctree_to_chave Leaf = []
ctree_to_chave (Node a b tree1 tree2) = ctree_to_chave tree1 ++ [(a,b)] ++ ctree_to_chave tree2