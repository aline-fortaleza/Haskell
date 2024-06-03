suaviza :: [Float] -> [Float]
suaviza [] = []
suaviza [a] = [a]
--suaviza [a,b] = [a,b]
suaviza (a:as) = a : suavizaMeio (a:as)  




suavizaMeio :: [Float] -> [Float]
suavizaMeio [] = []
suavizaMeio [a] = [a]
suavizaMeio [a,b] = [b]
suavizaMeio (a:b:c:as) = (media a b c) : suavizaMeio (b:c:as)

media :: Float -> Float -> Float -> Float
media a b c = (a+b+c)/3
