{-# OPTIONS_GHC -fdefer-typed-holes #-}
import Data.List
import Data.Char

function :: Integer -> Integer -> Integer
function x y = if (x > y) then (x  + 10) else y

function2 :: Integer -> Integer -> Integer
function2 x y = case (x > y) of
                    False -> y
                    True  -> (x  + 10)


absVal :: (Ord a, Num a) => a -> a
absVal x = if (x < 0) then (negate x) else x

absVal2 :: (Num a, Ord a) => a -> a
absVal2 x = case (x < 0) of
                True  -> negate x
                False -> x

validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password =
    if null username
      then (if null password
              then "Empty username and password"
              else "Empty username")
      else (if null password
              then "Empty password"
              else "Okay")

validateUsernamePassword2 :: String -> String -> String
validateUsernamePassword2 username password =
  case ((null username, null password)) of
        (True,  True)  -> "Empty username and password"
        (False, True)  -> "Empty password"
        (True,  False) -> "Empty username"
        (False, False) -> "Okay"


safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

isAnagram :: String -> String -> Bool
isAnagram word1 word2 = (sort word1) == (sort word2)

isWord :: String -> Maybe String
isWord word = case (null word) of
                  True -> Nothing
                  False ->
                      case (all isAlpha word) of
                          False -> Nothing
                          True -> Just word

checkAnagram :: String -> String -> String
checkAnagram word1 word2 = case (isWord word1) of
                               Nothing -> "The first word is invalid."
                               Just word1 ->
                                   case (isWord word2) of
                                       Nothing -> "The second word is invalid."
                                       Just word2 ->
                                           case (isAnagram word1 word2) of
                                               False -> "These words are not anagrams."
                                               True  -> "These words are anagrams."


main :: IO ()
main =
  do
    putStr "Please enter a word.\n> "
    word1 <- getLine
    putStr "Please enter a second word.\n> "
    word2 <- getLine
    print (checkAnagram word1 word2)
