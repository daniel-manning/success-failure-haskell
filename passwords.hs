import Data.List
import Data.Char

newtype Password = Password String
    deriving Show

newtype Error = Error String
    deriving Show

newtype Username = Username String
    deriving Show

data User = User Username Password
    deriving Show

checkLength :: Int -> String ->  Either Error String
checkLength maxLength input =
    case (length input > maxLength) of
        True -> Left (Error "Input cannot be longer than maximum length.")
        False -> Right input

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password = Password <$> checkLength 20 password


checkUsernameLength :: String -> Either Error Username
checkUsernameLength username = Username <$> checkLength 15 username

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

validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
  cleanWhitespace username
  >>= requireAlphaNum
  >>= checkUsernameLength

makeUser :: Username -> Password -> Either Error User
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
