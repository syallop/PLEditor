{-# LANGUAGE
    OverloadedStrings
  , DataKinds
#-}
{-|
Module      : PLEditor.CurrentLine
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

A CurrentLine encodes a Line with a cursor position at which characters can be
inserted or deleted.
Functions are provided which 'try' to move the cursor left and right. 'try' in
that if movement in that direction is impossible, no movement will be performed.
This might be a good default for a text editor. If this needs to be detected,
the resulting CurrentLine would need to be tested for equality. This decision
may be reversed.
-}
module PLEditor.CurrentLine
  ( CurrentLine ()
  , emptyCurrentLine
  , startCurrentLine
  , completeCurrentLine
  , cursorCurrentLine
  , tryMoveCursorLeft
  , tryMoveCursorRight
  , insertAtCursor
  , deleteAtCursor
  )
  where

import PLEditor.Line

-- | A current line is a line with a cursor positon.
newtype CurrentLine = CurrentLine
  { _unCurrentLine :: ( Line 'RightToLeft -- First half of line ordered right-to-left. The cursor is considered on the first character.
                      , Line 'LeftToRight -- Second half of line ordered left-to-right.
                      )
  }

-- | No text in the current line.
emptyCurrentLine
  :: CurrentLine
emptyCurrentLine = CurrentLine (reverseLine . textLine $ "",textLine "")

-- | A current line starts at the first character.
startCurrentLine
  :: Line 'LeftToRight
  -> CurrentLine
startCurrentLine line = CurrentLine (reverseLine . textLine $ "",line)


-- | Complete a current line by merging it into a single line, returning where the
-- cursor was.
completeCurrentLine
  :: CurrentLine
  -> (Line 'LeftToRight, Int)
completeCurrentLine currentLine =
  case currentLine of
    CurrentLine (linePrefix, lineSuffix)
      -> (reverseLine linePrefix <> lineSuffix, lineLength linePrefix)

-- | Complete a current line adding a cursor character at the appropriate place,
-- returning where the cursor is.
cursorCurrentLine
  :: CurrentLine
  -> (Line 'LeftToRight, Int)
cursorCurrentLine currentLine =
  case currentLine of
    CurrentLine (linePrefix, lineSuffix)
      -> case lastCharacter linePrefix of
           Nothing
             -> (emptyLine <> lineSuffix, lineLength linePrefix)

           Just (_c,remainingPrefix)
             -> (reverseLine remainingPrefix <> textLine "_" <> lineSuffix, lineLength linePrefix)

-- | Move a cursor left if possible.
tryMoveCursorLeft
  :: CurrentLine
  -> CurrentLine
tryMoveCursorLeft currentLine =
  case currentLine of
    CurrentLine (prefixes, suffixes)
      -> case lastCharacter prefixes of
           Nothing
             -> currentLine

           Just (c, remainingPrefixes)
             -> CurrentLine (remainingPrefixes, prefixWithCharacter c suffixes)

-- | Move a cursor right if possible.
tryMoveCursorRight
  :: CurrentLine
  -> CurrentLine
tryMoveCursorRight currentLine =
  case currentLine of
    CurrentLine (prefixes, suffixes)
      -> case firstCharacter suffixes of
           Nothing
             -> currentLine

           Just (c, remainingSuffixes)
             -> CurrentLine (postfixWithCharacter c prefixes, remainingSuffixes)

-- | Insert a character at the cursor position.
insertAtCursor
  :: Char
  -> CurrentLine
  -> CurrentLine
insertAtCursor c currentLine =
  case currentLine of
    CurrentLine (prefixLine, suffixLine)
      -> CurrentLine (postfixWithCharacter c prefixLine, suffixLine)

-- Delete the character at the cursor.
--
-- This function is aware it would normally be typed
-- '... -> Maybe (CurrentLine, Char)' but currently chooses this representation
-- so that when the caller does not care that a delete did not actually occur
-- (and doesnt need the deleted character) it need not bind the deleted character
-- or perform case analysis on the currentline. It is assumed this is the
-- standard editor pattern.
deleteAtCursor
  :: CurrentLine
  -> (CurrentLine, Maybe Char)
deleteAtCursor currentLine =
  case currentLine of
    CurrentLine (prefixLine, suffixLine)
      -> case lastCharacter prefixLine of
           Nothing
             -> (currentLine, Nothing)

           Just (deletedCharacter, remainingPrefix)
             -> (CurrentLine (remainingPrefix, suffixLine), Just deletedCharacter)

