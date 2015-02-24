module Huffman.Tree where

import Control.Applicative
import Control.Monad (join)
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List
import Data.Map      (toList, fromListWith)
import Data.Monoid

data Tree a = Leaf { symbol :: [a],    weight :: Int }
            | Node { left   :: Tree a, right  :: Tree a
                   , symbol :: [a],    weight :: Int }
                   deriving (Eq, Show)

instance Eq a => Ord (Tree a) where
  compare = compare `on` weight

frequency :: Ord a => [a] -> [([a], Int)]
frequency xs = sortBy (compare `on` snd) $ toList vals
  where vals = fromListWith (+) $ map (\x -> ([x], 1)) xs

leaves :: [([a], Int)] -> [Tree a]
leaves = map (uncurry Leaf)

conj :: Tree a -> Tree a -> Tree a
conj a b = Node a b (symbol a ++ symbol b) (weight a + weight b)

build :: Ord a => [Tree a] -> Tree a
build []       = Leaf mempty 0
build [final]  = final
build (l:r:ts) = build (sort $ conj l r : ts)

tree :: Ord a => [a] -> Tree a
tree = build . leaves . frequency

path :: Ord a => Tree a -> a -> Maybe [Int]
path (Node a b _ _) ch
  | ch `elem` symbol a = (:) <$> Just 0 <*> path a ch
  | ch `elem` symbol b = (:) <$> Just 1 <*> path b ch
  | otherwise          = Nothing
path (Leaf s _) ch
  | [ch] == s = Just []
  | otherwise = Nothing

paths :: Ord a => Tree a -> [a] -> Maybe [Int]
paths = (foldMap id .) . map . path

encode :: Ord a => [a] -> Maybe [Int]
encode = join $ paths . tree
