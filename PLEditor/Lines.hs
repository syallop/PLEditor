module PLEditor.Lines
  ( Lines ()
  , singletonLines
  , firstLine
  , prependLine
  , reverseLines
  )
  where

import Data.Monoid

import PLEditor.Line

newtype Lines = Lines { _unLines :: [Line] }

instance Monoid Lines where
  mempty = Lines []
  mappend (Lines lL) (Lines lR) = Lines (lL <> lR)

-- | One Line can be many lines.
singletonLines
  :: Line
  -> Lines
singletonLines l = Lines [l]

-- | Extract the first line if there is one.
firstLine
  :: Lines
  -> Maybe (Line, Lines)
firstLine lines = case lines of
  Lines (l:ls)
    -> Just (l, Lines ls)
  _ -> Nothing

-- | Add a line to the front on lines.
prependLine
  :: Line
  -> Lines
  -> Lines
prependLine l (Lines ls) = Lines (l:ls)

-- | Reverse the order of lines.
reverseLines
  :: Lines
  -> Lines
reverseLines (Lines ls) = Lines . reverse $ ls

