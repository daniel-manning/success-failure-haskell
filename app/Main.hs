module Main where

import Lib
import Data.Char

checkPasswordLength :: String -> Maybe String
checkPasswordLength password = case (length password > 20) of
                                    True -> Nothing
                                    False -> Just password

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs = case (all isAlphaNum xs) of
                                    False -> Nothing
                                    True -> Just xs

cleanWhiteSpace :: String -> Maybe String
cleanWhiteSpace "" = Nothing
cleanWhiteSpace 

main :: IO ()
main =
  do
    putStr "Please enter password.\n> "
    password <- getLine
    print (requireAlphaNum password)
