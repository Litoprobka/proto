module Proto (
    Proto,
    ErrorPlaceholder (..),
    skip,
    try,
    takeWhileP,
    takeWhile1P,
    takeP,
    satisfy,
    eof,
    notFollowedBy,
    char,
    anyChar,
    string,
    space,
    space1,
    parse,
) where

import Data.Char (isSpace)
import Data.Text qualified as Text
import Relude

-- monad transformer is TBD
newtype Proto a = Proto (Text -> ParseStep a) deriving (Functor)

newtype ErrorPlaceholder = Err Text deriving (Show)

data ParseStep a
    = Parsed Text a
    | Skipped ErrorPlaceholder
    | Error ErrorPlaceholder
    deriving (Functor)

instance Applicative Proto where
    pure x = Proto (`Parsed` x)
    Proto wf <*> Proto wx = Proto \s -> case wf s of
        Parsed s' f -> f <$> wx s'
        Skipped err -> Skipped err
        Error err -> Error err

instance Monad Proto where
    Proto wx >>= f = Proto \s -> case wx s of
        Parsed s' x -> let Proto wy = f x in wy s'
        Skipped err -> Skipped err
        Error err -> Error err

instance Alternative Proto where
    empty = Proto \_ -> Skipped (Err "Alternative.empty")
    Proto wx <|> Proto wy = Proto \s -> case wx s of
        Skipped _ -> wy s
        other -> other

instance MonadPlus Proto where
    mzero = empty
    mplus = (<|>)

-- | a placeholder for proper errors
skip :: ParseStep a
skip = Skipped (Err "error placeholder")

try :: Proto a -> Proto a
try (Proto parser) = Proto \s -> case parser s of
    Error err -> Skipped err
    other -> other

takeWhileP :: (Char -> Bool) -> Proto Text
takeWhileP p = Proto \s ->
    let (taken, stream) = Text.span p s
     in Parsed stream taken

takeWhile1P :: (Char -> Bool) -> Proto Text
takeWhile1P p = Proto \s -> case Text.span p s of
    (taken, s')
        | Text.null taken -> skip
        | otherwise -> Parsed s' taken

takeP :: Int -> Proto Text
takeP n = Proto \s -> case Text.splitAt n s of
    (taken, s')
        | Text.length taken == n -> Parsed s' taken
        | otherwise -> skip

satisfy :: (Char -> Bool) -> Proto Char
satisfy p = Proto \s -> case Text.uncons s of
    Just (c, s') | p c -> Parsed s' c
    _ -> skip

eof :: Proto ()
eof = Proto \s ->
    if Text.null s
        then Parsed s ()
        else skip

notFollowedBy :: Proto a -> Proto ()
notFollowedBy (Proto notNext) =
    Proto \s -> case notNext s of
        Parsed _ _ -> Error (Err "placeholder")
        _ -> Parsed s ()

char :: Char -> Proto Char
char c = satisfy (== c)

anyChar :: Proto Char
anyChar = satisfy (const True)

string :: Text -> Proto Text
string txt = do
    chunk <- takeP (Text.length txt)
    if chunk == txt
        then pure chunk
        else Proto (const skip) -- should this be backtrackable?

space, space1 :: Proto ()
space = void $ takeWhileP isSpace
space1 = void $ takeWhile1P isSpace

parse :: Proto a -> Text -> Either ErrorPlaceholder a
parse (Proto parser) input = case parser input of
    Parsed _ output -> Right output
    Skipped err -> Left err
    Error err -> Left err