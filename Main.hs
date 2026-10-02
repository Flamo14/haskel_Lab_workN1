module Main where

import Data.Char (toLower, toUpper)

-- Task 1: variant 6
maxNumber :: [Int] -> String
maxNumber [] = "The maximum number is undefined for an empty list"
maxNumber xs = "The maximum number is " ++ show (maximum xs)

-- Task 2: variant 6
reverseName :: String -> String
reverseName [] = []
reverseName s = go (length s - 1) s
  where
    go i xs
      | i < 0 = []
      | otherwise = xs !! i : go (i - 1) xs

-- Task 3: variant 6
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:cs) = toUpper c : map toLower cs

capitalizeWords :: String -> String
capitalizeWords = unwords . map capitalizeWord . words

-- Task 4: variant 6
oneButLast :: [a] -> Maybe a
oneButLast [] = Nothing
oneButLast [_] = Nothing
oneButLast (x:_:[]) = Just x
oneButLast (_:xs) = oneButLast xs

-- Task 5: variant 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Task 6: variant 6
gaps :: [a] -> [[a]]
gaps [] = []
gaps xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

-- Task 7
-- Binary search tree representation

data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Eq, Show)

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree e Leaf = Branch e Leaf Leaf
insertTree e (Branch x left right)
  | e <= x = Branch x (insertTree e left) right
  | otherwise = Branch x left (insertTree e right)

collectValues :: Tree a -> [a]
collectValues Leaf = []
collectValues (Branch x left right) = collectValues left ++ [x] ++ collectValues right

isSearchTree :: Ord a => Tree a -> Bool
isSearchTree Leaf = True
isSearchTree (Branch x left right) =
  isSearchTree left &&
  isSearchTree right &&
  all (<= x) (collectValues left) &&
  all (> x) (collectValues right)

propInsertPreservesBST :: [Int] -> Bool
propInsertPreservesBST xs = isSearchTree (foldl (flip insertTree) Leaf xs)

-- A mutated variant that breaks the BST invariant
mutatedInsertTree :: Ord a => a -> Tree a -> Tree a
mutatedInsertTree e Leaf = Branch e Leaf Leaf
mutatedInsertTree e (Branch x left right)
  | e <= x = Branch x left (mutatedInsertTree e right)
  | otherwise = Branch x (mutatedInsertTree e left) right

mutatedPropertyFails :: Bool
mutatedPropertyFails =
  let badTree = foldl (flip mutatedInsertTree) Leaf [5, 3, 7, 1, 4, 6, 8]
  in not (isSearchTree badTree)

-- Task 8
lawMapConcat :: Eq b => (a -> b) -> [[a]] -> Bool
lawMapConcat f xs = (map f . concat) xs == (concat . map (map f)) xs

-- Demonstration
printTask1 :: IO ()
printTask1 = putStrLn (maxNumber [5, 10, 11, 15, 3, 8])

printTask2 :: IO ()
printTask2 = putStrLn ("Reversed name: " ++ reverseName "Haskell")

printTask3 :: IO ()
printTask3 = putStrLn ("Capitalized sentence: " ++ capitalizeWords "haskell programming language lab")

printTask4 :: IO ()
printTask4 = putStrLn ("One but last: " ++ show (oneButLast [1, 2, 3, 4, 5]))

printTask5 :: IO ()
printTask5 = putStrLn ("Is palindrome [1,2,3,2,1]? " ++ show (isPalindrome [1, 2, 3, 2, 1]))

printTask6 :: IO ()
printTask6 = putStrLn ("Gaps: " ++ show (gaps [1, 2, 3, 4, 5]))

printTask7 :: IO ()
printTask7 = do
  let sampleTree = Branch 5 (Branch 3 Leaf Leaf) (Branch 7 Leaf Leaf)
  let insertExample = insertTree 4 sampleTree
  putStrLn ("BST after insert: " ++ show insertExample)
  putStrLn ("isSearchTree insertExample = " ++ show (isSearchTree insertExample))
  putStrLn ("propInsertPreservesBST [7,3,9,1,5,8] = " ++ show (propInsertPreservesBST [7, 3, 9, 1, 5, 8]))
  putStrLn ("Mutation detected = " ++ show mutatedPropertyFails)

printTask8 :: IO ()
printTask8 = do
  let sampleList = [[1, 2], [], [3, 4], [5]]
  putStrLn ("Task 8 law holds: " ++ show (lawMapConcat (+ 1) sampleList))
  putStrLn ("Expanded form: " ++ show ((map (+ 1) . concat) sampleList))
  putStrLn ("Nested map form: " ++ show ((concat . map (map (+ 1))) sampleList))

main :: IO ()
main = do
  putStrLn "Task 1"
  printTask1
  putStrLn ""
  putStrLn "Task 2"
  printTask2
  putStrLn ""
  putStrLn "Task 3"
  printTask3
  putStrLn ""
  putStrLn "Task 4"
  printTask4
  putStrLn ""
  putStrLn "Task 5"
  printTask5
  putStrLn ""
  putStrLn "Task 6"
  printTask6
  putStrLn ""
  putStrLn "Task 7"
  printTask7
  putStrLn ""
  putStrLn "Task 8"
  printTask8
