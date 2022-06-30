-- 5.3.1 Growing a Tree
module TreeExample where

data Tree a = Node a (Tree a) (Tree a) | Empty
  deriving Show

tree1 :: Tree Int
tree1 = Node 1 Empty Empty

tree2 :: Tree Int
tree2 = (Node 0
              (Node 1
                    (Node 2 Empty Empty)
                    (Node 3 Empty Empty))
              (Node 4 Empty Empty))
{-
  -- tree2
                0
               / \
              /   \
             1     4
            / \   
           /   \
          2     3
-}


treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- Example sur treeHeight de tree2
-- treeHeight tree2
-- treeHeight Node 0 (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)) (Node 4 Empty Empty)
--  ===> 1 + max (treeHeight (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)))
--               (treeHeight (Node 4 Empty Empty))
--  ===> 1 + max ( 1 + max (treeHeight (Node 2 Empty Empty))
--                         (treeHeight (Node 3 Empty Empty)))
--               ( 1 + max (treeHeight Empty) (treeHeight Empty))
-- ===> 1 + max ( 1 + max ( 1 + max (treeHeight Empty) (treeHeight Empty))
--                        ( 1 + max (treeHeight Empty) (treeHeight Empty)))
--              ( 1 )
-- ===> 1 + max ( 1 + 1)
--              ( 1 )
-- ===> 1 + max 2 1 ==> 1 + 2 ==> 3
