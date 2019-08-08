import Data.List

promptWord1 :: IO String
promptWord1 =
  do
    putStr "Please enter a word.\n> "
    getLine

promptWord2 :: IO String
promptWord2 =
  do
    putStr "Please enter a second word.\n> "
    getLine

checkAnagram:: String -> String -> Bool
checkAnagram s1 s2 = (sort s1) == (sort s2)


main :: IO()
main = do
         result <- checkAnagram <$> promptWord1 <*> promptWord2
         print result