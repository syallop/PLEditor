{-|
Module      : PLEditor.Lines
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

Lines are an ordered collection of 'Line's which may be appended together and
Line's inserted and removed.
-}
module PLEditor.Lines
  ( Lines ()
  , emptyLines
  , singletonLines
  , firstLine
  , prependLine
  , reverseLines
  , takeLines
  , mapLines
  , renderLines
  )
  where

import Data.Monoid
import qualified Data.Text as Text

import PLEditor.Line

newtype Lines = Lines { _unLines :: [Line] }

instance Monoid Lines where
  mempty = Lines []
  mappend (Lines lL) (Lines lR) = Lines (lL <> lR)

emptyLines
  :: Lines
emptyLines = Lines []

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

-- | Take at most a number of lines returning the number NOT taken.
takeLines
  :: Int
  -> Lines
  -> (Lines,Int)
takeLines n (Lines ls) =
  let (linesTaken, remaining) = takeLines n ls
   in (Lines linesTaken, remaining)
  where
    takeLines 0 _      = ([],0)
    takeLines n []     = ([],n)
    takeLines n (l:ls) = let (linesTaken, remaining) = takeLines (n-1) ls
                          in (l:linesTaken, remaining)

-- | Map a function across each line.
mapLines
  :: (Line -> Line)
  -> Lines
  -> Lines
mapLines lineF (Lines ls) = Lines . map lineF $ ls

renderLines
  :: Lines
  -> [Line]
renderLines (Lines ls) = ls

