
dividirStr :: Char -> String -> [String] 
dividirStr _ "" = [""]
dividirStr chr (a:as) 
    | a == chr = "" : dividirStr chr as -- no nosso caso chr vai ser ; 
    |  otherwise = (a: head (dividirStr chr as) ) : tail (dividirStr chr as)


dividirEsp :: String -> [String] -- transforma os elementos da 1° string em elementos diferentes 
dividirEsp str = words str 

concTudo :: [String] -> [String] 
concTudo list = concatMap dividirEsp list 

listFinal :: String -> [String]
listFinal fatura = concTudo (dividirStr ';' fatura) 


gastosMes :: [String] -> String -> [Double] --[String] vem do listFinal e o primeiro double é um acumulador iniciado vazio
gastosMes [] month = []
gastosMes (dia:mes:tipo:valor:as) month | mes == month = (read valor) : (gastosMes as month) 
                                        | otherwise = gastosMes as month

soma :: [Double] -> Double
soma lista = foldl (+) 0 lista 


logMes :: String -> String -> Double 
logMes mes fatura = soma (gastosMes (listFinal fatura) mes ) 
