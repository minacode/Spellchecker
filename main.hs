import System.Environment
import Data.Tree
import Data.Char
import Data.List hiding (insert)
import Data.Function

type Trie = Forest (Char, Bool)
type TrieNode = Tree (Char, Bool)


{- | This is the main function of the Spellchecker.
   It reads exactly two files as command line arguments.
   The first one contains words which are used for correction.
   The second one contains the text which should be corrected.
   The corrected text is written into "correction.txt".
-}
main :: IO ()
main = do 
  args <- getArgs
  if length args == 2
  then do
    let (wordListPath : textPath : _) = args
    ws <- readFile wordListPath 
    let trie = buildTrie ws
    txt <- readFile textPath  
    writeFile correctionFileName []
    let (txtList, a) = parse txt
    if a
    then runCorrection trie (cycle [correctAlpha, correctNonAlpha]) txtList
    else runCorrection trie (cycle [correctNonAlpha, correctAlpha]) txtList 
  else
    putStrLn "Please give a word list and a text as input."

  where
    runCorrection :: Trie -> [Trie -> String -> IO ()] -> [String] -> IO ()
    runCorrection _ [] _  = error "runCorrection: no functions"
    runCorrection _ _  [] = return ()
    runCorrection trie (f : fs) (w : ws) = do 
      f trie w
      runCorrection trie fs ws 
    
    correctAlpha :: Trie -> String -> IO ()
    correctAlpha = correct 
    
    correctNonAlpha :: Trie -> String -> IO ()
    correctNonAlpha _ = appendFile correctionFileName
 
{- | This function splits a given String in Groups of alphabetical 
   and non-alphabetical Chars using "Data.Char.isAlpha". 
   The Bool value is true if the first String in the List 
   is a String of alphabetical Chars.
   Empty Strings are considered non-alphabetical.
-}
parse :: String -> ([String], Bool)
parse s = let parsedString = groupBy ((==) `on` isAlpha) s
           in (parsedString, startsWithAlpha parsedString)
  where
    startsWithAlpha :: [String] -> Bool
    startsWithAlpha ((x : _) : _) = isAlpha x
    startsWithAlpha _ = False

{- | This function takes a String and creates a Trie.
     The String can contain multiple words which are seperated by whitespace.
     For more details see "insert".
-}
buildTrie :: String -> Trie
buildTrie s = go (words s) []
  where
    go :: [String] -> Trie -> Trie
    go (x : xs) trie = go xs $ insert trie x
    go _        trie = trie

{- | This function inserts a single String into the Trie.
     The Char at each Node is the Char by which the Node was reached.
     The Bool at a Node describes whether it is an end-Node or not.
-}
insert :: Trie -> String -> Trie
insert [] [x] = [Node (x, True) []]
insert [] (x : xs) = [Node (x, False) (insert [] xs)]
insert (Node (y, end) ts : zs) [x] 
  | x == y    = Node (y, True) ts : zs
  | otherwise = Node (y, end ) ts : insert zs [x] 
insert (Node (y, end) ts : zs) (x : xs)
  | x == y    = Node (y, end) (insert ts xs) : zs 
  | otherwise = Node (y, end) ts : insert zs (x:xs)
insert trie _ = trie 

-- | This function checks if a Trie contains a String.
contains :: Trie -> String -> Bool
contains [] _ = False
contains _ [] = True
contains (Node (l, end) _ : ts) w@[x]
  | x == l    = end
  | otherwise = contains ts w 
contains (Node (l, _) cs : ts) w@(x : xs)
  | x == l    = contains cs xs
  | otherwise = contains ts w 

{- | This function is the head of the correction, 
     deciding whether it is necessary to correct 
     and resume with user interaction or not.
-}
correct :: Trie -> String -> IO () 
correct trie word
  | trie `contains` word = appendFile correctionFileName word
  | otherwise = do
      putStrLn word
      let sugg = suggestions trie (maxCorrectionDistance word) word
      getUserCorrection word sugg

-- | This is the output function for suggestions.
-- It basically prints a List of Strings in a more human readable way.
printSuggestions :: [String] -> IO ()
printSuggestions [] = putStrLn "[No suggestions. Please type in your own.]"
printSuggestions sugg = go sugg
  where
    go :: [String] -> IO ()
    go [] = return ()
    go (x : xs) = do
      putStrLn $ "-> " ++ x
      go xs

-- | This is the name of the output file.
correctionFileName :: String
correctionFileName = "correction.txt"

-- | This is the maximum levensthein distance for suggested words.
-- It allows one mistake, including swaps, per 5 Chars.
maxCorrectionDistance :: String -> Int
maxCorrectionDistance w = length w `div` 5

{- | This function asks the user for correcting a word
   by using a List of suggestions.
   It is possible to type in a non-suggestion word.
   In that case, the function asks again to avoid mistakes in user input.
   Therefore it is possible to use a non suggested correction 
   or to keep the original version of the word.
-}
getUserCorrection :: String -> [String] -> IO ()
getUserCorrection word sugg = do
  printSuggestions sugg
  userCorrection <- getLine
  if userCorrection `elem` sugg || null sugg
  then appendFile correctionFileName userCorrection
  else do 
    putStrLn "You chose a non-suggested word. [y/n]?"
    a <- getLine
    if a == "y"
    then appendFile correctionFileName userCorrection
    else getUserCorrection word sugg 

{- | This function calculates all words in a Trie which
   levensthein distances are less or equal a given number.
-}
suggestions :: Trie -> Int -> String -> [String]
suggestions [] _ _ = []
suggestions trie maxDist word
  = concatMap 
    (go []
        (take (length word + 1) $ iterate (+1) 0, [] )
    )
    trie

  where
    go :: String -> ([Int], [Int]) -> TrieNode 
       -> [String]
    go suggBuffer 
       (vec1, vec2) 
       (Node (label, end) ts)
      | minimum vec > maxDist = []
      | end && last vec <= maxDist
        = (suggBuffer ++ [label])
          : concatMap
            (go (suggBuffer ++ [label])
                (vec, vec1)
            ) ts
      | otherwise 
        = concatMap  
          (go (suggBuffer ++ [label])
              (vec, vec1)
          ) ts
      where
        vec = length suggBuffer + 1
              : calcVector label 
                           1
                           (length suggBuffer) -- not sure
                           (length suggBuffer + 1)
                           (vec1, vec2)
    
    calcVector :: Char -> Int -> Int -> Int -> ([Int], [Int])
               -> [Int]
    calcVector label pos vecNum lastX (vec1, vec2)
      | pos > length word = []
      | pos >= 2 && vecNum >= 2  
        = let x = minimum [ (vec1 !! (pos -1)) + costs (word !! (pos -1)) label
                          , (vec1 !! pos     ) + 1
                          , lastX              + 1
                          , (vec2 !! (pos -2)) + 1
                          ]
           in x : calcVector label 
                             (pos + 1) 
                             vecNum 
                             x 
                             (vec1, vec2)
      | otherwise 
        = let x = minimum [ (vec1 !! (pos -1)) + costs (word !! (pos -1)) label
                          , (vec1 !! pos     ) + 1
                          , lastX              + 1
                          ]
           in x : calcVector label 
                             (pos + 1) 
                             vecNum 
                             x 
                             (vec1, vec2)
      
    costs :: Char -> Char -> Int
    costs w label
      | w == label = 0
      | otherwise  = 1
