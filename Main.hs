import System.Environment
import Data.Tree
import Data.Char
import Data.List hiding (insert)
import Data.Function
import Control.Monad
import Text.Read
import Debug.Trace

type Trie = Forest (Char, Bool)
type TrieNode = Tree (Char, Bool)
data Config = Config { inputFile       :: String
                     , outputFile      :: String
                     , wordListFile    :: String
                     , charsPerMistake :: Int
                     }


{- | This is the main function of the Spellchecker.
   It reads exactly two files as command line arguments.
   The first one contains words which are used for correction.
   The second one contains the text which should be corrected.
   The corrected text is written into "correction.txt".
-}
main :: IO ()
main = do 
  args <- getArgs
  if length args == 4
  then do
    let (wordListFile : inputFile : outputFile : charsPerMistake : _) = args
        config = Config { inputFile       = inputFile
                        , outputFile      = outputFile
                        , wordListFile    = wordListFile
                        , charsPerMistake = read charsPerMistake :: Int
                        }
    ws <- readFile wordListFile
    let trie = buildTrie ws
    txt <- readFile inputFile
    writeFile outputFile []
    let (txtList, alpha) = parse txt
    zipWithM_ (\f t -> f t) (getFunctionList alpha config trie) txtList
  else
    putStrLn "Wrong input. \n \
               \ Please give in this order: \n \ 
               \ - a word list file \n \
               \ - an input file \n \
               \ - an output file \n \
               \ - the number of chars per one allowed mistake"

  where
    getFunctionList :: Bool -> Config -> Trie -> [String -> IO ()]
    getFunctionList True config trie = cycle [ correctAlpha    config trie
                                             , correctNonAlpha config trie
                                             ]
    getFunctionList _    config trie = cycle [ correctNonAlpha config trie
                                             , correctAlpha    config trie
                                             ]

    correctAlpha :: Config -> Trie -> String -> IO ()
    correctAlpha = correct 
    
    correctNonAlpha :: Config -> Trie -> String -> IO ()
    correctNonAlpha config _ = appendFile $ outputFile config
 
{- | This function splits a given String in groups of alphabetical 
   and non-alphabetical Chars using "Data.Char.isAlpha". 
   The Bool value is True if the first String in the List 
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
buildTrie s = foldl' insert [] (words s)

{- | This function inserts a single String into a Trie.
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
correct :: Config -> Trie -> String -> IO () 
correct config trie word
  | trie `contains` word = appendFile (outputFile config) word
  | otherwise = do
      putStrLn word
      let maxDist = maxCorrectionDistance (charsPerMistake config) word
          sugg = suggestions trie maxDist word
      traceShow maxDist $ getUserCorrection (outputFile config) word sugg

-- | This is the maximum levensthein distance for suggested words.
-- It allows one mistake, including swaps, per 5 Chars.
maxCorrectionDistance :: Int -> String -> Int
maxCorrectionDistance charsPerMistake w = 
  (length w `div` charsPerMistake) + 1

-- | This is the output function for suggestions.
-- It basically prints a List of Strings in a more human readable way.
printSuggestions :: [String] -> IO ()
printSuggestions [] = putStrLn "[No suggestions. Please type in your own.]"
printSuggestions sugg =
  mapM_ (\ (n, x) -> putStrLn $ show n ++ " " ++ x) $ zip [1..] sugg

{- | This function asks the user to correct a word
   by using a List of suggestions.
   It is possible to type in a not suggested word.
   In that case, the function asks again to make sure it wasn't a mistake.
   Therefore it is possible to use a non suggested correction 
   or to keep the original version of the word.
-}
getUserCorrection :: String -> String -> [String] -> IO ()
getUserCorrection outputFile word sugg = do
  printSuggestions sugg
  userCorrection <- getLine
  if null sugg
  then appendFile outputFile userCorrection
  else evaluateUserCorrection userCorrection
  
  where 
    evaluateUserCorrection userCorrection =
      case readMaybe userCorrection of
        Nothing  -> appendFile outputFile userCorrection
        (Just n) -> 
          if n < length sugg
          then appendFile outputFile (sugg !! n)
          else do 
            putStrLn "Try again."
            userCorrection <- getLine
            evaluateUserCorrection userCorrection

{- | This function calculates all words in a Trie which
   levensthein distances are less or equal a given number.
-}
suggestions :: Trie -> Int -> String -> [String]
suggestions [] _ _ = []
suggestions trie maxDist word
  = traceShow maxDist $ concatMap 
      (go []
          (take (length word + 1) $ iterate (+1) 0, [] )
      )
      trie

  where
    go :: String -> ([Int], [Int]) -> TrieNode -> [String]
    go suggBuffer (vec1, vec2) (Node (label, end) ts)
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
                           (length suggBuffer)
                           (length suggBuffer + 1)
                           (vec1, vec2)
    
    calcVector :: Char -> Int -> Int -> Int -> ([Int], [Int]) -> [Int]
    calcVector label pos vecNum lastMin (vec1, vec2)
      | pos > length word = []
      | otherwise
        = let values' = [ (vec1 !! (pos -1)) + costs (word !! (pos -1)) label
                        , (vec1 !! pos     ) + 1
                        , lastMin            + 1
                        ]
              values = if pos >= 2 && vecNum >= 2
                       then (vec2 !! (pos -2)) + 1 : values'
                       else values'
              newMin = minimum values
           in newMin : calcVector label 
                                  (pos + 1) 
                                  vecNum 
                                  newMin 
                                  (vec1, vec2)
      
    costs :: Char -> Char -> Int
    costs w label
      | w == label = 0
      | otherwise  = 1
