{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Validation
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)

newtype Error = Error [Text]
    deriving (Semigroup, Show)

newtype Password = Password Text
    deriving Show

newtype Username = Username Text
    deriving Show

data User = User Username Password
    deriving Show

checkLength :: Int -> Text ->  Validation Error Text
checkLength maxLength input =
    case (T.length input > maxLength) of
        True -> Failure (Error ["Input cannot be longer than maximum length."])
        False -> Success input

checkPasswordLength :: Text -> Validation Error Password
checkPasswordLength password = Password <$> checkLength 20 password


checkUsernameLength :: Text -> Validation Error Username
checkUsernameLength username = Username <$> checkLength 15 username

requireAlphaNum :: Text -> Validation Error Text
requireAlphaNum xs =
    case (T.all isAlphaNum xs) of
        False -> Failure (Error ["Cannot contain white space or special characters."])
        True -> Success xs

cleanWhitespace :: Text -> Validation Error Text
cleanWhitespace t =
    case T.null(T.strip t) of
        True -> Failure (Error ["Cannot be empty."])
        False -> Success (T.strip t)

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  case (cleanWhitespace password) of
      Failure err -> Failure err
      Success password2 -> requireAlphaNum password2
                          *> checkPasswordLength password2


validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case (cleanWhitespace username) of
      Failure err -> Failure err
      Success username2 -> requireAlphaNum username2
                          *> checkUsernameLength username2

makeUser :: Username -> Password -> Validation Error User
makeUser username password = User
                             <$> validateUsername username
                             <*> validatePassword password


main :: IO ()
main =
  do
    putStr "Please enter a username.\n> "
    username <- Username <$> T.getLine
    putStr "Please enter a password.\n> "
    password <- Password <$> T.getLine
    print (makeUser username password)
