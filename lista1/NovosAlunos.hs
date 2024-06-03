--bSort :: [String] -> [String]


qSort :: [String] -> [String]
qSort [] = []
qSort (a:as) = qSort [x | x <- as, x < a] ++ [a] ++ qSort [x | x <- as, x >= a] 