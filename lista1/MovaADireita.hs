addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos(n-1)

paraDireita :: Int -> String -> String
--addEspacos 0 str = str
paraDireita n str = (addEspacos n) ++ str