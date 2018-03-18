{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PLEditor.Line
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

A Line is a single line of Text which may be appended and taken from.
-}
module PLEditor.Line
  ( Line ()
  , textLine
  , lineText
  , reverseLine
  , emptyLine
  , appendLine
  , lineLength
  , firstCharacter
  , prefixWithCharacter
  , takeFromLine
  )
  where

import Data.Text
import Data.Monoid
import qualified Data.Text as Text

{- TODO:
 - Decide whether Lines should be forbidden from containing newline characters.
 - Encode directional flow of text for type-safe appends and potential
   optimisations between the interaction of reverse and appends.
-}

-- | A Line of Text.
newtype Line = Line Text

instance Monoid Line where
  mempty = Line ""
  mappend (Line l0) (Line l1) = Line (l0 <> l1)

textLine
  :: Text
  -> Line
textLine = Line

lineText
  :: Line
  -> Text
lineText (Line txt) = txt

-- | Reverse the text within a line.
reverseLine
  :: Line
  -> Line
reverseLine (Line txt) = Line (Text.reverse txt)

-- | An empty line with no text.
emptyLine
  :: Line
emptyLine = Line ""

-- | Append two lines.
appendLine
  :: Line
  -> Line
  -> Line
appendLine lineL lineR = lineL <> lineR

-- | A lines length is zero or greater.
lineLength
  :: Line
  -> Int
lineLength (Line txt) = Text.length txt

-- | The first character of a line may or may not exist.
firstCharacter
  :: Line
  -> Maybe (Char, Line)
firstCharacter (Line txt) = case Text.uncons txt of
  Nothing
    -> Nothing

  Just (c,txt')
    -> Just (c, Line txt')

-- | Insert a character infront of a line.
prefixWithCharacter
  :: Char
  -> Line
  -> Line
prefixWithCharacter c (Line txt) = Line (Text.cons c txt)

-- | Take a number of characters from a line.
takeFromLine
  :: Int
  -> Line
  -> Line
takeFromLine n (Line txt) = Line $ Text.take n txt

