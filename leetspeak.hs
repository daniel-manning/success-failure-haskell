substituteChar :: Char -> Char
substituteChar c = case c of
                     'e' -> '3'
                     'a' -> '4'
                     't' -> '7'
                     'l' -> '1'
                     'o' -> '0'
                     _   -> c


translateWord :: String -> String
translateWord word = case (isWord word) of
                       Nothing -> "This is not a word."
                       Just word -> map substituteChar word

isWord :: String -> Maybe String
isWord word = case (null word) of
                  True -> Nothing
                  False -> Just word

main :: IO ()
main =
  do
    putStr "Please enter a word.\n> "
    word <- getLine
    print (translateWord word)