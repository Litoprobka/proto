module Json where

import Relude hiding (bool, null)
import Relude.Unsafe (read)
import Proto
import Control.Monad.Combinators
import Data.Text qualified as Text
import Data.HashMap.Strict qualified as HashMap

data Json
    = Object (HashMap Text Json)
    | Array [Json]
    | String Text
    | Number Double
    | Bool Bool
    | Null
  deriving (Show, Eq)

lexeme :: Proto a -> Proto a
lexeme p = p <* space

symbol :: Text -> Proto Text
symbol = lexeme . string

json, object, array, number, null :: Proto Json
json = lexeme $ choice [object, array, String <$> string', number, bool, null]

object = Object . HashMap.fromList <$> between (symbol "{") (symbol "}") (sepEndBy keyValue (symbol ","))

keyValue :: Proto (Text, Json)
keyValue = (,) <$> string' <* symbol ":" <*> json

array = Array <$> between (symbol "[") (symbol "]") (sepEndBy json (symbol ","))

string' :: Proto Text
string' = lexeme $ between (char '"') (symbol "\"") $ takeWhileP (/='"')

number = lexeme $ Number . read . Text.unpack . Text.concat <$> sequence
    [nonZero, digits, fromMaybe "" <$> optional (liftA2 Text.cons (char '.') digits)]
  where
    nonZero = one <$> satisfy (`elem` ['1'..'9'])
    digits = takeWhileP (`elem` ['0'..'9'])

bool :: Proto Json
bool = Bool <$> (True <$ symbol "true" <|> False <$ symbol "false")

null = Null <$ symbol "null"

