-- primeiro dividir a string em varios fatores com ; 
dividirStr :: Char -> String -> [String] 
dividirStr _ "" = [""]
dividirStr chr (a:as) 
    | a == chr = "" : dividirStr chr as -- no nosso caso chr vai ser ; 
    | otherwise = (a: head (dividirStr chr as) ) : tail (dividirStr chr as)



-- depois criar uma lista so com os gastos excluindo o resto e já transforma de String pra double
soGastos :: [String] -> [Double]
soGastos [] = []
soGastos (_:_:a:as) = (read a) : soGastos as -- retira só os terceiros itens da lista de strings
soGastos (a) = [] --definir os casos pra não multiplos de 3 
  
valores :: String -> [Double] 
valores fatura = soGastos (dividirStr ';' fatura)


-- cria a funçao da questão usando min e max
minMaxCartao :: String -> (Double, Double) 
minMaxCartao [] = (0,0)
minMaxCartao fatura = (minimum (valores fatura), maximum (valores fatura)) -- min e max é só pra quando é uma tupla aparentemente
    