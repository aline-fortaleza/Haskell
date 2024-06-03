--main = do
--    s <- getLine
--    let result = htob s
--    print result


htob :: String -> String -- passa de hexa pra binario 
htob [] = []
htob (a:as) = hexDigitToBit a ++ htob as

--htod :: String -> Int -- passa de hexa pra decimal 
--htod [] = 0
--htod (a:as) = ((hexDigitToInt a)*(16^((length (a:as))-1))) + htod as

--dtob :: Int -> String -- passa de decimal para binario
--dtob 0 = "0"
--dtob 1 = "1"
--dtob n = dtob (n `div` 2) ++ (show (n `mod` 2))

hexDigitToBit :: Char -> String -- converte os char pra uma string em binario
hexDigitToBit ch
        | ch == '0' = "0000"
        | ch == '1' = "0001"
        | ch == '2' = "0010"
        | ch == '3' = "0011"
        | ch == '4' = "0100"
        | ch == '5' = "0101"
        | ch == '6' = "0110"
        | ch == '7' = "0111"
        | ch == '8' = "1000"
        | ch == '9' = "1001"
        | ch == 'A' = "1010"
        | ch == 'B' = "1011"
        | ch == 'C' = "1100"
        | ch == 'D' = "1101"
        | ch == 'E' = "1110"
        | ch == 'F' = "1111"
        | otherwise     = "0000"

