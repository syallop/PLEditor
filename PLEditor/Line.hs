{-# LANGUAGE
    OverloadedStrings
  , DataKinds
  , TypeFamilies
  #-}
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
  , lastCharacter
  , prefixWithCharacter
  , postfixWithCharacter
  , takeFromLine

  , HDir (..)
  , ReverseH
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

data HDir
  = LeftToRight
  | RightToLeft

type family ReverseH h where
  ReverseH LeftToRight = RightToLeft
  ReverseH RightToLeft = LeftToRight

-- | A Line of Text.
newtype Line (hDir :: HDir) = Line Text

instance Monoid (Line hDir) where
  mempty = Line ""
  mappend (Line l0) (Line l1) = Line (l0 <> l1)

textLine
  :: Text
  -> Line 'LeftToRight
textLine = Line

lineText
  :: Line 'LeftToRight
  -> Text
lineText (Line txt) = txt

-- | Reverse the text within a line.
reverseLine
  :: Line hDir
  -> Line (ReverseH hDir)
reverseLine (Line txt) = Line (Text.reverse txt)

-- | An empty line with no text.
emptyLine
  :: Line hDir
emptyLine = Line ""

-- | Append two lines.
appendLine
  :: Line hDir
  -> Line hDir
  -> Line hDir
appendLine lineL lineR = lineL <> lineR

-- | A lines length is zero or greater.
lineLength
  :: Line hDir
  -> Int
lineLength (Line txt) = Text.length txt

-- | The first character of a line may or may not exist.
firstCharacter
  :: Line 'LeftToRight
  -> Maybe (Char, Line 'LeftToRight)
firstCharacter (Line txt) = case Text.uncons txt of
  Nothing
    -> Nothing

  Just (c,txt')
    -> Just (c, Line txt')

-- | The last character of a line may or may not exist.
lastCharacter
  :: Line 'RightToLeft
  -> Maybe (Char, Line 'RightToLeft)
lastCharacter (Line txt) = case Text.uncons txt of
  Nothing
    -> Nothing

  Just (c,txt')
    -> Just (c, Line txt')


-- | Insert a character infront of a line.
prefixWithCharacter
  :: Char
  -> Line 'LeftToRight
  -> Line 'LeftToRight
prefixWithCharacter c (Line txt) = Line (Text.cons c txt)

-- | Insert a character at the end of a line.
postfixWithCharacter
  :: Char
  -> Line 'RightToLeft
  -> Line 'RightToLeft
postfixWithCharacter c (Line txt) = Line (Text.cons c txt)

-- | Take a number of characters from a line.
takeFromLine
  :: Int
  -> Line 'LeftToRight
  -> Line 'LeftToRight
takeFromLine n (Line txt) = Line $ Text.take n txt

