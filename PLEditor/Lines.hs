{-# LANGUAGE
    DataKinds
  , TypeFamilies
  #-}
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
  , postpendLine
  , reverseLines
  , takeLines
  , mapLines
  , renderLines

  , VDir (..)
  , ReverseV
  )
  where

import Data.Monoid
import qualified Data.Text as Text

import PLEditor.Line

data VDir
  = TopToBottom
  | BottomToTop

type family ReverseV v where
  ReverseV TopToBottom = BottomToTop
  ReverseV BottomToTop = TopToBottom

data Lines (vDir :: VDir) = Lines [Line]

instance Monoid (Lines vDir) where
  mempty = Lines []
  mappend (Lines lL) (Lines lR) = Lines (lL <> lR)

emptyLines
  :: Lines vDir
emptyLines = Lines []

-- | One Line can be many lines.
singletonLines
  :: Line
  -> Lines vDir
singletonLines l = Lines [l]

-- | Extract the first line if there is one.
firstLine
  :: Lines vDir
  -> Maybe (Line, Lines vDir)
firstLine lines = case lines of
  Lines (l:ls)
    -> Just (l, Lines ls)
  _ -> Nothing

-- | Add a line to the front on lines.
prependLine
  :: Line
  -> Lines 'TopToBottom
  -> Lines 'TopToBottom
prependLine l (Lines ls) = Lines (l:ls)

-- | Add a line to the end of lines.
postpendLine
  :: Line
  -> Lines 'BottomToTop
  -> Lines 'BottomToTop
postpendLine l (Lines ls) = Lines (l:ls)

-- | Reverse the order of lines.
reverseLines
  :: Lines vDir
  -> Lines (ReverseV vDir)
reverseLines (Lines ls) = Lines . reverse $ ls

-- | Take at most a number of lines returning the number NOT taken.
takeLines
  :: Int
  -> Lines vDir
  -> (Lines vDir,Int)
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
  -> Lines vDir
  -> Lines vDir
mapLines lineF (Lines ls) = Lines . map lineF $ ls

renderLines
  :: Lines TopToBottom
  -> [Line]
renderLines (Lines ls) = ls

