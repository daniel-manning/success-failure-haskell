{-# LANGUAGE GeneralisedNewtypeDeriving #-}

import Data.Char
import Data.Validation

newtype Error = Error [String]
    deriving (Semigroup, Show)
    
newtype Password = Password String
    deriving Show

newtype Username = Username String
    deriving Show

data User = User Username Password
    deriving Show

checkLength :: Int -> String ->  Validation Error String
checkLength maxLength input =
    case (length input > maxLength) of
        True -> Failure (Error ["Input cannot be longer than maximum length."])
        False -> Success input

checkPasswordLength :: String -> Validation Error Password
checkPasswordLength password = Password <$> checkLength 20 password


checkUsernameLength :: String -> Validation Error Username
checkUsernameLength username = Username <$> checkLength 15 username

requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
    case (all isAlphaNum xs) of
        False -> Failure (Error ["Cannot contain white space or special characters."])
        True -> Success xs

cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (Error ["Cannot be empty."])
cleanWhitespace (x:xs) =
    case (isSpace x) of
        True -> cleanWhitespace xs
        False -> Success (x:xs)

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  cleanWhitespace password
  >>= requireAlphaNum
  >>= checkPasswordLength

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  cleanWhitespace username
  >>= requireAlphaNum
  >>= checkUsernameLength

makeUser :: Username -> Password -> Validation Error User
makeUser username password = User
                             <$> validateUsername username
                             <*> validatePassword password


main :: IO ()
main =
  do
    putStr "Please enter a username.\n> "
    username <- Username <$> getLine
    putStr "Please enter a password.\n> "
    password <- Password <$> getLine
    print (makeUser username password)
