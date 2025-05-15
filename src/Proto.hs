module Proto (
    Proto,
    fail,
    try,
    expect,
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
    error,
) where

import Data.Char (isSpace)
import Data.Text qualified as Text
import Data.Text (Text)
import Prelude hiding (fail, error)
import Control.Monad hiding (fail)
import Control.Applicative

-- monad transformer is TBD
newtype Proto err a = Proto (Text -> ParseStep err a) deriving (Functor)

data ParseStep err a
    = Parsed Text a
    | Skipped (Either Text err) -- an expected parse failure
    | Error (Either Text err)
    deriving (Functor)

instance Applicative (Proto err) where
    pure x = Proto (`Parsed` x)
    Proto wf <*> Proto wx = Proto \s -> case wf s of
        Parsed s' f -> f <$> wx s'
        Skipped err -> Skipped err
        Error err -> Error err

instance Monad (Proto err) where
    Proto wx >>= f = Proto \s -> case wx s of
        Parsed s' x -> let Proto wy = f x in wy s'
        Skipped err -> Skipped err
        Error err -> Error err

instance Alternative (Proto err) where
    empty = Proto \_ -> Skipped (Left "Alternative.empty")
    Proto wx <|> Proto wy = Proto \s -> case wx s of
        Skipped _ -> wy s
        other -> other

instance MonadPlus (Proto err) where
    mzero = empty
    mplus = (<|>)

-- | a placeholder for proper errors
skip :: ParseStep err a
skip = Skipped (Left "error placeholder")

-- | signal a backtrackable parse failure
fail :: Text -> Proto err a
fail msg = Proto (const $ Skipped $ Left msg)

-- | throw a non-backtrackable parse failure
error :: Text -> Proto err a
error msg = Proto (const $ Error $ Left msg)

try :: Proto err a -> Proto err a
try (Proto parser) = Proto \s -> case parser s of
    Error err -> Skipped err
    other -> other

-- | treat parse failure as an error, i.e. expect a parser to always succeed
-- 
-- > >>> parser = char 'a' <|> char 'b'
-- > >>> parse parser "b"
-- > Right 'b'
-- >
-- > >>> parser2 = expect (char 'a') <|> char 'b'
-- > >>> parse parser2 "b"
-- > Left (Left "placeholder")
expect :: Proto err a -> Proto err a
expect (Proto parser) = Proto \s -> case parser s of
    Skipped err -> Error err
    other -> other

takeWhileP :: (Char -> Bool) -> Proto err Text
takeWhileP p = Proto \s ->
    let (taken, stream) = Text.span p s
     in Parsed stream taken

takeWhile1P :: (Char -> Bool) -> Proto err Text
takeWhile1P p = Proto \s -> case Text.span p s of
    (taken, s')
        | Text.null taken -> skip
        | otherwise -> Parsed s' taken

takeP :: Int -> Proto err Text
takeP n = Proto \s -> case Text.splitAt n s of
    (taken, s')
        | Text.length taken == n -> Parsed s' taken
        | otherwise -> skip

satisfy :: (Char -> Bool) -> Proto err Char
satisfy p = Proto \s -> case Text.uncons s of
    Just (c, s') | p c -> Parsed s' c
    _ -> skip

eof :: Proto err ()
eof = Proto \s ->
    if Text.null s
        then Parsed s ()
        else skip

notFollowedBy :: Proto err a -> Proto err ()
notFollowedBy (Proto notNext) =
    Proto \s -> case notNext s of
        Parsed _ _ -> Error (Left "placeholder")
        _ -> Parsed s ()

char :: Char -> Proto err Char
char c = satisfy (== c)

anyChar :: Proto err Char
anyChar = satisfy (const True)

string :: Text -> Proto err Text
string txt = do
    chunk <- takeP (Text.length txt)
    if chunk == txt
        then pure chunk
        else fail "placeholder"

space, space1 :: Proto err ()
space = void $ takeWhileP isSpace
space1 = void $ takeWhile1P isSpace

parse :: Proto err a -> Text -> Either (Either Text err) a
parse (Proto parser) input = case parser input of
    Parsed _ output -> Right output
    Skipped err -> Left err
    Error err -> Left err