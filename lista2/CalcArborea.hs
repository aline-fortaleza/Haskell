data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)



evalTree :: IntTree -> Int
evalTree (Nilt a) = a 
evalTree (Node SUM arv1 arv2) = evalTree arv1 + evalTree arv2
evalTree (Node MUL arv1 arv2) = evalTree arv1 * evalTree arv2
evalTree (Node SUB arv1 arv2) = evalTree arv1 - evalTree arv2