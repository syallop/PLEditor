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
  , lastLine
  , prependLine
  , postpendLine
  , reverseLines
  , takeLines
  , mapLines
  , renderLines
  , lineCount

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

data Lines (vDir :: VDir) (hDir :: HDir) = Lines [Line hDir]

instance Monoid (Lines vDir hDir) where
  mempty = Lines []
  mappend (Lines lL) (Lines lR) = Lines (lL <> lR)

emptyLines
  :: Lines vDir hDir
emptyLines = Lines []

-- | One Line can be many lines.
singletonLines
  :: Line hDir
  -> Lines vDir hDir
singletonLines l = Lines [l]

-- | Extract the first line if there is one.
firstLine
  :: Lines 'TopToBottom hDir
  -> Maybe (Line hDir, Lines 'TopToBottom hDir)
firstLine lines = case lines of
  Lines (l:ls)
    -> Just (l, Lines ls)
  _ -> Nothing

-- | Extract the last line if there is one.
lastLine
  :: Lines 'BottomToTop hDir
  -> Maybe (Line hDir, Lines 'BottomToTop hDir)
lastLine lines = case lines of
  Lines (l:ls)
    -> Just (l, Lines ls)
  _ -> Nothing


-- | Add a line to the front on lines.
prependLine
  :: Line hDir
  -> Lines 'TopToBottom hDir
  -> Lines 'TopToBottom hDir
prependLine l (Lines ls) = Lines (l:ls)

-- | Add a line to the end of lines.
postpendLine
  :: Line hDir
  -> Lines 'BottomToTop hDir
  -> Lines 'BottomToTop hDir
postpendLine l (Lines ls) = Lines (l:ls)

-- | Reverse the order of lines.
reverseLines
  :: Lines vDir hDir
  -> Lines (ReverseV vDir) hDir
reverseLines (Lines ls) = Lines . reverse $ ls

-- | Take at most a number of lines returning the number NOT taken.
takeLines
  :: Int
  -> Lines vDir hDir
  -> (Lines vDir hDir,Int)
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
  :: (Line hDir -> Line hDir')
  -> Lines vDir hDir
  -> Lines vDir hDir'
mapLines lineF (Lines ls) = Lines . map lineF $ ls

renderLines
  :: Lines TopToBottom hDir
  -> [Line hDir]
renderLines (Lines ls) = ls

-- | A positive count of the number of lines.
lineCount
  :: Lines vDir hDir
  -> Int
lineCount (Lines ls) = length ls

