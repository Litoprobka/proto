{-# LANGUAGE TypeFamilies #-}
module Proto (
    Proto(..),
    ParseStep(..),
    Stream(..),
    failed,
    try,
    expect,
    satisfy,
    eof,
    notFollowedBy,
    token,
    anyToken,
    parse,
    error,
    lookAhead,
) where

import Prelude hiding (error)
import Control.Monad hiding (fail)
import Control.Applicative
import Data.List (uncons)
import Control.Monad.Combinators qualified as C

-- monad transformer is TBD
newtype Proto err s a = Proto (s -> ParseStep err s a) deriving (Functor)

data ParseStep err s a
    = Parsed !s !a
    | Skipped
    | Error !err
    deriving (Functor)

instance Applicative (Proto err s) where
    pure x = Proto (`Parsed` x)
    Proto wf <*> Proto wx = Proto \s -> case wf s of
        Parsed s' f -> f <$> wx s'
        Skipped -> Skipped
        Error err -> Error err

instance Monad (Proto err s) where
    Proto wx >>= f = Proto \s -> case wx s of
        Parsed s' x -> let Proto wy = f x in wy s'
        Skipped -> Skipped
        Error err -> Error err

instance Alternative (Proto err s) where
    empty = Proto (const Skipped)
    Proto wx <|> Proto wy = Proto \s -> case wx s of
        Skipped -> wy s
        other -> other
    many = C.many
    some = C.some

instance MonadPlus (Proto err s) where
    mzero = empty
    mplus = (<|>)

instance MonadFail (Proto err s) where
    fail _ = failed

-- | a very minimal interface for a token stream. It is assumed that you bundle location information by yourself
class Stream s where
    type Token s
    take1 :: s -> Maybe (Token s, s)

instance Stream [a] where
    type Token [a] = a
    take1 = uncons

-- | signal a backtrackable parse failure
failed :: Proto err s x
failed = empty

-- | throw a non-backtrackable parse error
error :: err -> Proto err s a
error msg = Proto (const $ Error msg)

-- | treat a parse error as a parse failure
try :: Proto err s a -> Proto err s a
try (Proto parser) = Proto \s -> case parser s of
    Error _ -> Skipped
    other -> other

-- | treat parse failure as an error, i.e. expect a parser to always succeed
-- 
-- > >>> parser = char 'a' <|> char 'b'
-- > >>> parse parser "b"
-- > Right 'b'
-- >
-- > >>> parser2 = expect "placeholder" (char 'a') <|> char 'b'
-- > >>> parse parser2 "b"
-- > Left "placeholder"
expect :: err -> Proto err s a -> Proto err s a
expect msg (Proto parser) = Proto \s -> case parser s of
    Skipped -> Error msg
    other -> other

-- | parse a token that satisfies a predicate
satisfy :: Stream s => (Token s -> Bool) -> Proto err s (Token s)
satisfy p = Proto \s -> case take1 s of
    Just (t, s') | p t -> Parsed s' t
    _ -> Skipped

eof :: Stream s => Proto err s ()
eof = Proto \s -> case take1 s of
    Nothing -> Parsed s ()
    Just _ -> Skipped

notFollowedBy :: Proto err s a -> Proto err s ()
notFollowedBy (Proto notNext) =
    Proto \s -> case notNext s of
        Parsed _ _ -> Skipped
        _ -> Parsed s ()

token :: (Stream s, Eq (Token s)) => Token s -> Proto err s (Token s)
token c = satisfy (== c)

anyToken :: Stream s => Proto err s (Token s)
anyToken = Proto \s -> case take1 s of
    Just (t, s') -> Parsed s' t
    _ -> Skipped

-- | run a parser without changing the stream state
lookAhead :: Proto err s a -> Proto err s a
lookAhead (Proto p) = Proto \s -> case p s of
    Parsed _ x -> Parsed s x
    other -> other

parse :: Proto err s a -> s -> Either (Maybe err) a
parse (Proto parser) input = case parser input of
    Parsed _ output -> Right output
    Error err -> Left $ Just err
    Skipped -> Left Nothing