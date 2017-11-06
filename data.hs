module Compression where

import Data.List
import Data.Char
import Numeric (showIntAtBase)

data HTree a = Leaf Int a | Node Int (HTree a) (HTree a)
               deriving (Show)

instance Eq (HTree a) where
  t1 == t2 = freqCount t1 == freqCount t2

instance Ord (HTree a) where
  t1 <= t2' = freqCount t1 <= freqCount t2'

type Code = [Int]

freqCount :: HTree a -> Int
freqCount (Leaf n a)
  = n
freqCount (Node n t1 t2)
  = n

testString :: String
testString
  = "mississippi is missing"

--
-- Example Huffman coding tree from the spec.
--
fig :: HTree Char
fig
  = Node 22 (Node 8 (Node 4 (Leaf 2 'p') (Leaf 2 ' '))
                    (Node 4 (Node 2 (Leaf 1 'n') (Leaf 1 'g'))
                            (Leaf 2 'm')))
            (Node 14 (Leaf 7 'i') (Leaf 7 's'))

----------------------------------------------------------------------------

count :: Eq a => a -> [a] -> Int
count query list = length [ 1 | x <- list, query == x ]

countAll :: Eq a => [a] -> [a] -> [(a, Int)]
countAll xs ys = [(item, count item ys) | item <- xs, elem item ys]

buildTable :: Eq a => [a] -> [(a, Int)]
buildTable list = countAll (nub list) list

merge :: HTree a -> HTree a -> HTree a
merge t1 t2
  | t1 < t2   = Node ((freqCount t1) + (freqCount t2)) t1 t2
  | otherwise   = Node ((freqCount t1) + (freqCount t2)) t2 t1

reduce :: [HTree a] -> HTree a
-- Pre: The argument list non-empty and sorted based on the ordering function
--      in the Ord instance above.
reduce ts = head (reduce' ts)
  where
    reduce' :: [HTree a] -> [HTree a]
    reduce' [t] = [t]
    reduce' (t1:t2:ts) = reduce' $ insert (merge t1 t2) ts

buildTree :: Eq a => [a] -> HTree a
-- Pre: The list is non-empty
buildTree list = reduce treeList
  where
    treeList = sort [Leaf c n | (n, c) <- buildTable list]

encode :: Eq a => [a] -> HTree a -> Code
-- Pre: The tree can encode each of the items the list
encode [] tree     = []
encode (x:xs) tree = (encode' [] x tree) ++ (encode xs tree)
  where
    encode' :: Eq a => Code -> a -> HTree a -> Code
    encode' acc x (Leaf count item)
      | item == x = acc
      | otherwise = []
    encode' acc x (Node count t1 t2)
      | null l1 = l2
      | null l2 = l1
      where
        l1 = encode' (acc ++ [0]) x t1
        l2 = encode' (acc ++ [1]) x t2

decode :: Code -> HTree a -> [a]
-- Pre: The code is valid with respect to the tree
decode code tree = decode' code tree
  where
    decode' [] (Leaf count item) = [item]
    decode' (x:xs) (Node count t1 t2)
      | x == 0 = decode' xs t1
      | x == 1 = decode' xs t2
    decode' xs (Leaf count item) = item : (decode xs tree)



compressTree :: HTree Char -> [Int]
compressTree (Leaf _ item) = ([1] ++ toBase (ord item) 2 7)
compressTree (Node _ t1 t2) = [0] ++ (compressTree t1) ++ (compressTree t2)

toBase :: Int -> Int -> Int -> [Int]
-- Pre: given x, b and n, n is at least as large as the number of digits
-- required to represent x in base b.
toBase x b 0 = []
toBase x b n = q : toBase r b (n - 1)
  where
    (q, r) = quotRem x $ pow b (n - 1)

pow :: Int -> Int -> Int
pow b x = product $ take x $ repeat b

-- Pre: The bitstring ([Int]) is a valid encoding of a Huffman tree
--      of characters
rebuildTree :: [Int] -> HTree Char
-- rebuildTree [] =
-- rebuildTree (1:ts) = Leaf 0 (binaryTo10 enc)
--   where
--     (enc, rest) = splitAt 6 ts
-- rebuildTree (0:ts) =
rebuildTree ints = snd (rebuildTree' ints)
  where
    rebuildTree' :: [Int] -> (Int, HTree Char)
    rebuildTree' (0:xs) = (n - 1, Node 0 tr1 tr2)
      where
        (_, x2) = splitAt ((length xs) - n) xs
        (n, tr1) = rebuildTree' xs
        (_, tr2) = rebuildTree' x2
    rebuildTree' (1:xs) = (length rest, Leaf 0 (chr ((binaryTo10 enc))))
      where
        (enc, rest) = splitAt 6 xs

binaryTo10 :: [Int] -> Int
binaryTo10 [a] = a
binaryTo10 xes@(x:xs) = x * (pow 2 ((length xes) - 1)) + binaryTo10 xs

-----------------------------------------------------------------------------
-- THE SECRET TEST STRING

secretString :: String
secretString
  = decode code (rebuildTree tree)
  where
    code = [0,1,0,0,0,0,1,1,0,1,0,1,1,1,1,0,0,0,1,
            1,0,0,1,1,1,1,0,0]
    tree = [0,0,0,1,1,1,0,1,0,0,0,1,1,1,0,0,0,0,1,
            0,1,1,1,0,1,1,1,1,1,1,1,1,1,0,0,1,0,1,
            1,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,1,0,1,
            0,1,0,1,1,0,0,1,1,1,1,0,0,1,1,1,0,1,0,
            0,0,0,0]
