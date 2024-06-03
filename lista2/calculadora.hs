type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa list = executaAux 0 list 


executaAux :: Int -> [(Comando,Valor)] -> Int -- deve ser chamado com o int endo 0 (acumulador)
executaAux a [] = a -- devolvve o acumulador na lista vazia
executaAux a (("Multiplica", b): xs) = executaAux (a*b) xs
executaAux a (("Soma", b): xs) = executaAux (a+b) xs
executaAux a (("Subtrai", b): xs) = executaAux (a-b) xs
executaAux a (("Divide", 0): xs) = executaAux (-666) xs
executaAux a (("Divide", b): xs) = executaAux (a `div` b) xs


