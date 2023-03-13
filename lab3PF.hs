import Control.Applicative

import Data.Char
import Data.ByteString (isInfixOf)

-- sa se scrie un parser care sa identifice o cifra precedata de un semn (+ sau -)

newtype Parser a = Parser { apply :: String -> [(a, String)] } 

parse :: Parser a -> String -> Either String a
parse = undefined

instance Functor Parser where
    fmap f pa = Parser (\input -> [(f a, rest) | (a, rest) <- apply pa input])

instance Applicative Parser where
    pure a = Parser (\input -> [(a, input)])
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pa = Parser (\input -> [(f a, resta) | (f, restf) <- apply pf input, (a, resta) <- apply pa restf])

instance Monad Parser where
    pa >>= k = Parser (\input -> [(b, restb) | (a, resta) <- apply pa input, (b, restb) <- apply (k a) resta])

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where
    go [] = []   -- imposibil de parsat șirul vid
    go (c:input)
      | p c = [(c, input)]   -- dacă predicatul ține, întoarce c și restul șirului de intrare
      | otherwise = [] 



--- | acceptă doar caracterul dat ca argument
char :: Char -> Parser Char
char c = satisfy (\x -> x == c)

cifraSemn :: Parser Int
cifraSemn = undefined

string :: String -> Parser String
string = undefined

digit :: Parser Char
digit = satisfy isDigit

cifSemn :: Parser Int
cifSemn = do 
  ch <- satisfy (\x -> x == '+' || x == '-')
  d <- digit 
  case ch of 
    '+' -> return (digitToInt d)
    '-' -> return ((-1) * (digitToInt d) )

--ex 2 
cifSemn1 :: Parser Int
cifSemn1 = pure (\ch d -> if ch == '+' then d else (-1)*d ) <*> satisfy (\x -> x == '+' || x == '-') <*> (digitToInt <$> digit)

--ex 3
string1 :: String -> Parser String
string1 [] = return []
string1 (x:xs) = do
  char x
  string1 xs 
  return (x:xs)

-- cazul general
string2 (x:xs) = pure (:) <*> char x <*> string2 xs

--tema restul ex din lab 3
-- bogdan.macovei.fmi@gmail.com
