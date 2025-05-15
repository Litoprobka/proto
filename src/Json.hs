-- an example json parser
module Json where

import Prelude hiding (null)
import Proto
import Control.Monad.Combinators
import Data.Text qualified as Text
import Data.Text (Text)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)

data Json
    = Object (HashMap Text Json)
    | Array [Json]
    | String Text
    | Number Double
    | Bool Bool
    | Null
  deriving (Show, Eq)

type Parser = Proto ()

lexeme :: Parser a -> Parser a
lexeme p = p <* space

symbol :: Text -> Parser Text
symbol = lexeme . string

json, object, array, number, null :: Parser Json
json = lexeme $ choice [object, array, String <$> string', number, bool, null]

object = Object . HashMap.fromList <$> between (symbol "{") (symbol "}") (sepEndBy keyValue (symbol ","))

keyValue :: Parser (Text, Json)
keyValue = (,) <$> string' <* symbol ":" <*> json

array = Array <$> between (symbol "[") (symbol "]") (sepEndBy json (symbol ","))

string' :: Parser Text
string' = lexeme $ between (char '"') (symbol "\"") $ takeWhileP (/='"')

number = lexeme $ Number . read . Text.unpack . Text.concat <$> sequence
    [nonZero, digits, fromMaybe "" <$> optional (liftA2 Text.cons (char '.') digits)]
  where
    nonZero = Text.singleton <$> satisfy (`elem` ['1'..'9'])
    digits = takeWhileP (`elem` ['0'..'9'])

bool :: Parser Json
bool = Bool <$> (True <$ symbol "true" <|> False <$ symbol "false")

null = Null <$ symbol "null"

