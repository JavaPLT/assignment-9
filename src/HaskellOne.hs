module HaskellOne where

smooth2 :: Fractional a => [a] -> [a]
smooth2 xs = undefined

starling :: (p -> q -> r) -> (p -> q) -> p -> r
starling = undefined

kestrel :: p -> q -> p
kestrel = undefined

kLargest :: Ord a => Int -> [a] -> [a]
kLargest k xs
  | k > length xs = error "k should be smaller than the length of the list"
kLargest k xs = undefined

data InfBinTree a = Fork a (InfBinTree a) (InfBinTree a)

treeRepeat :: a -> InfBinTree a
treeRepeat = undefined

boundedEq :: Eq a => Int -> InfBinTree a -> InfBinTree a -> Bool
boundedEq = undefined

xTree, yTree :: InfBinTree Int
xTree = undefined
yTree = undefined

treeBfs :: InfBinTree a -> [a]
treeBfs = undefined