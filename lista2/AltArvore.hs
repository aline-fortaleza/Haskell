data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)


-- alturaArvore :: Tree t -> Int

alturaArvore :: Tree t  -> Int
alturaArvore Nilt = 0
alturaArvore (Node x arv1 arv2) = max (alturaArvore arv1) (alturaArvore arv2) + 1 
