{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Validation
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Control.Lens


newtype Error = Error (NonEmpty Text)
    deriving (Semigroup, Show)

--instance Semigroup Error where
--    Error x <> Error y = Error (x <> T.pack("\n") <> y)

newtype Password = Password Text
    deriving Show

newtype Username = Username Text
    deriving Show

data User = User Username Password
    deriving Show

checkLength :: Int -> Text ->  Validation Error Text
checkLength maxLength input =
    case (T.length input > maxLength) of
        True -> Failure (constructError "Input cannot be longer than maximum length.")
        False -> Success input

checkPasswordLength :: Text -> Validation Error Password
checkPasswordLength password = Password <$> checkLength 20 password


checkUsernameLength :: Text -> Validation Error Username
checkUsernameLength username = Username <$> checkLength 15 username

requireAlphaNum :: Text -> Validation Error Text
requireAlphaNum xs =
    case (T.all isAlphaNum xs) of
        False -> Failure (constructError "Cannot contain white space or special characters.")
        True -> Success xs

cleanWhitespace :: Text -> Validation Error Text
cleanWhitespace t =
    case T.null(T.strip t) of
        True -> Failure (constructError "Cannot be empty.")
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

mapFailure :: (e1 -> e2) -> Validation e1 a -> Validation e2 a
mapFailure f (Failure err) = Failure (f err)
mapFailure _ (Success x)   = Success x

passwordErrors :: Password -> Validation Error Password
passwordErrors password =
    mapFailure (\err -> constructError "Invalid password:" <> err) (validatePassword password)

usernameErrors :: Username -> Validation Error Username
usernameErrors username =
    mapFailure (\err -> constructError "Invalid username:" <> err) (validateUsername username)

makeUser :: Validate v => Username -> Password -> v Error User
makeUser username password =
   review _Validation
     (User <$> usernameErrors username <*> passwordErrors password)

display :: Username -> Password -> IO()
display name password =
    case makeUser name password of
         Failure err -> putStrLn (unlines (NE.toList (errorCoerce err)))
         Success (User (Username name) password2)-> putStrLn ("Welcome, " ++ T.unpack(name))

errorCoerce :: Error -> NonEmpty String
errorCoerce (Error err) = NE.map T.unpack err

constructError :: String -> Error
constructError msg = Error (T.pack(msg) :| [])


main :: IO ()
main =
  do
    putStr "Please enter a username.\n> "
    username <- Username <$> T.getLine
    putStr "Please enter a password.\n> "
    password <- Password <$> T.getLine
    display username password
