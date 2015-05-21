{-# LANGUAGE FlexibleInstances #-}

module SevenTrees where

import Test.QuickCheck.Arbitrary 
import Test.QuickCheck.Gen
import Control.Applicative

data Tree = Leaf | Node Tree Tree
  deriving (Show, Read, Eq)

left (Node l _) = l
right (Node _ r) = r

instance Arbitrary Tree where
  arbitrary = oneof [return Leaf, Node <$> arbitrary <*> arbitrary]

instance Arbitrary (Tree, Tree) where
  arbitrary = (,) <$> arbitrary <*> arbitrary

instance Arbitrary (Tree, Tree, Tree, Tree, Tree, Tree, Tree) where
  arbitrary = (,,,,,,) 
              <$> arbitrary <*> arbitrary <*> arbitrary 
              <*> arbitrary <*> arbitrary <*> arbitrary 
              <*> arbitrary 

f :: (Tree, Tree, Tree, Tree, Tree, Tree, Tree) -> Tree
f (Leaf, Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)                  = Leaf
f (t1, Node Leaf Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)          = Node t1  Leaf
f (Node t1 t2, Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)            = Node t1 (Node t2 Leaf)
f (t1, Node (Node t2 t3) Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)  = Node t1 (Node t2 (Node t3 Leaf))
f (t1, Node t2 (Node t3 t4), Leaf, Leaf, Leaf, Leaf, Leaf)    = Node t1 (Node t2 (Node t3 (Node t4 Leaf)))
f (t1, t2, Node t3 t4, Leaf, Leaf, Leaf, Leaf)                = Node t1 (Node t2 (Node t3 (Node t4 (Node Leaf Leaf))))
f (t1, t2, t3, Node t4 t5, Leaf, Leaf, Leaf)                  = Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 Leaf) Leaf))))
f (t1, t2, t3, t4, Node t5 t6, Leaf, Leaf)                    = Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 (Node t6 Leaf)) Leaf))))
f (t1, t2, t3, t4, t5, t6, Node t7 t8)                        = Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 (Node t6 (Node t7 t8))) Leaf))))
f (t1, t2, t3, t4, t5, Node t6 t7, Leaf)                      = Node t1 (Node t2 (Node t3 (Node t4 (Node t5 (Node t6 t7)))))

g :: Tree -> (Tree, Tree, Tree, Tree, Tree, Tree, Tree)
g Leaf                                                                                 = (Leaf, Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)
g (Node t1 Leaf)                                                                       = (t1, Node Leaf Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 Leaf))                                                             = (Node t1 t2, Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 Leaf)))                                                   = (t1, Node (Node t2 t3) Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 (Node t4 Leaf))))                                         = (t1, Node t2 (Node t3 t4), Leaf, Leaf, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 (Node t4 (Node Leaf Leaf)))))                             = (t1, t2, Node t3 t4, Leaf, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 Leaf) Leaf)))))                   = (t1, t2, t3, Node t4 t5, Leaf, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 (Node t6 Leaf)) Leaf)))))         = (t1, t2, t3, t4, Node t5 t6, Leaf, Leaf)
g (Node t1 (Node t2 (Node t3 (Node t4 (Node (Node t5 (Node t6 (Node t7 t8))) Leaf))))) = (t1, t2, t3, t4, t5, t6, Node t7 t8)
g (Node t1 (Node t2 (Node t3 (Node t4 (Node t5 (Node t6 t7))))))                       = (t1, t2, t3, t4, t5, Node t6 t7, Leaf)

w :: (Tree, Tree) -> Tree
w (t1, t2) = go (Node t1 t2)
  where go t = if leftOnly t then left t else t

leftOnly t = t == Leaf || right t == Leaf && leftOnly (left t)

v :: Tree -> (Tree, Tree)
v t | leftOnly t = (t, Leaf)
v (Node t1 t2) = (t1, t2)

prop_vw_id :: (Tree, Tree) -> Bool
prop_vw_id t2 = v(w t2) == t2

prop_wv_id :: Tree -> Bool
prop_wv_id t = w(v t) == t

prop_fg_id :: Tree -> Bool
prop_fg_id t = f(g t) == t

prop_gf_id :: (Tree, Tree, Tree, Tree, Tree, Tree, Tree) -> Bool
prop_gf_id t7 = g(f t7) == t7
