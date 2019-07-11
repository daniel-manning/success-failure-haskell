import Data.List
import Data.Char

isPalindrome :: String -> Bool
isPalindrome word = word == (reverse word)

isWord :: String -> Maybe String
isWord word = case (null word) of
                  True -> Nothing
                  False ->
                      case (all isAlpha word) of
                          False -> Nothing
                          True -> Just word

checkPalindrome :: String -> String
checkPalindrome word = case (isWord word) of
                                       Nothing -> "The provided word is invalid."
                                       Just word2 ->
                                           case (isPalindrome word) of
                                               False -> "This word is not a palindrome."
                                               True  -> "This word is a palindrome."

main :: IO ()
main =
  do
    putStr "Please enter a word.\n> "
    word <- getLine
    print (checkPalindrome word)