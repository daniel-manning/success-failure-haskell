import Data.List
import Data.Char

newtype Password = Password String
    deriving Show

newtype Error = Error String
    deriving Show

newtype Username = Username String
    deriving Show

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
    case (length password > 20) of
        True -> Left (Error "Your password cannot be longer than 20 characters.")
        False -> Right (Password password)

checkUsernameLength :: String -> Either Error Username
checkUsernameLength username =
    case (length username > 15) of
        True -> Left (Error "Username cannot be longer than 15 characters.")
        False -> Right (Username username)

requireAlphaNum :: String -> Either Error String
requireAlphaNum xs =
    case (all isAlphaNum xs) of
        False -> Left (Error "Cannot contain white space or special characters.")
        True -> Right xs

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Cannot be empty.")
cleanWhitespace (x:xs) =
    case (isSpace x) of
        True -> cleanWhitespace xs
        False -> Right (x:xs)

validatePassword :: Password -> Either Error Password
validatePassword (Password password) =
  cleanWhitespace password
  >>= requireAlphaNum
  >>= checkPasswordLength

main :: IO ()
main =
  do
    putStr "Please enter a password.\n> "
    password <- Password <$> getLine
    print (validatePassword password)