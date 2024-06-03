maquinaSomar :: [Int] -> [Int]
maquinaSomar [] = []
maquinaSomar (0:0:as) = []
maquinaSomar lista = somarList (notnull (acharZeros lista [])) 

-- definir os intevalos de soma 
acharZeros :: [Int] -> [Int] -> [[Int]] -- segunda int é um acumulador e deve ser iniciado vazio
acharZeros [] acc = [acc]
acharZeros (0:0:as) acc = [acc] 
acharZeros (0:as) acc = acc : acharZeros as [] 
acharZeros (a:as) acc = acharZeros as (acc ++ [a])
      
-- tirar os nulos
notnull :: [[Int]] -> [[Int]]
notnull list = filter (not . null) list 


-- fazer a soma 

somarList :: [[Int]] -> [Int] 
somarList list =  map sum list 

