import Control.Monad
import Text.Read

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn "What is your age?"
  age <- getLine
  case (readMaybe age :: Maybe Integer) of
    Nothing -> do putStrLn $ "Age Not Integer"
    Just age -> do case mkPerson name age of
                     Left NameEmpty -> putStrLn ("Name Empty")
                     Left AgeTooLow -> putStrLn ("Age Too Low")
                     Left (PersonInvalidUnknown err) -> putStrLn (err)
                     Right person -> putStrLn (show person)

