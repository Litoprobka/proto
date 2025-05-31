{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Proto.Text where

import Proto
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Char (isSpace)
import Control.Monad (void)

-- some helper functions for text parsing

instance Stream Text where
    type Token Text = Char
    take1 = Text.uncons

takeWhileP :: (Char -> Bool) -> Proto err Text Text
takeWhileP p = Proto \s ->
    let (taken, stream) = Text.span p s
     in Parsed stream taken

takeWhile1P :: (Char -> Bool) -> Proto err Text Text
takeWhile1P p = Proto \s -> case Text.span p s of
    (taken, s')
        | Text.null taken -> Skipped
        | otherwise -> Parsed s' taken

takeP :: Int -> Proto err Text Text
takeP n = Proto \s -> case Text.splitAt n s of
    (taken, s')
        | Text.length taken == n -> Parsed s' taken
        | otherwise -> Skipped

char :: Char -> Proto err Text Char
char = token

anyChar :: Proto err Text Char
anyChar = anyToken

string :: Text -> Proto err Text Text
string txt = do
    chunk <- takeP (Text.length txt)
    if chunk == txt
        then pure chunk
        else failed

space, space1 :: Proto err Text ()
space = void $ takeWhileP isSpace
space1 = void $ takeWhile1P isSpace