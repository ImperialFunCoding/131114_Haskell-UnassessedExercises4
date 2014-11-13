module UE4 where

data Tree3 a b = Empty |Leaf a| Node (Tree3 a b) (b) (Tree3 a b)
                    deriving (Eq, Ord, Show)

tree = Node (Node (Leaf 1) 2 (Leaf 3)) 100 (Leaf 10)
tree' = Node (Node (Leaf (True,123)) (False,1) (Leaf (True,2))) (True,3) (Leaf (False,10000))

map3 :: (t -> a) -> (t1 -> b) -> Tree3 t t1 -> Tree3 a b
map3 f g Empty        = Empty
map3 f g (Leaf a)     = Leaf (f a)
map3 f g (Node a b c) = Node (map3 f g a) (g b) (map3 f g c)

fold3 f g base Empty            = base
fold3 f g base (Leaf a)         = f a
fold3 f g base (Node t1 b t2)   = g (fold3 f g base t1) b (fold3 f g base t2)

-- a
countLeaf a = 1
addUp x y z = x + z
-- fold3 countLeaf addUp 0 tree

-- b
add3 x y z = x + y + z
-- fold3 (id) (add3) 0 tree

-- c
append3 x y z = x++[y]++z
-- fold3 (:[]) append3 [] tree

-- d
append3' x y z = z++[y]++x
-- fold3 (:[]) append3' [] tree

-- e

append3'' a x y z = x++(pass a y)++z
pass :: (Eq a) =>a -> (a, b) -> [b]
pass v (a,b)
    | v == a    = [b]
    | otherwise = []

-- fold (pass True) (append3'' True) tree'
