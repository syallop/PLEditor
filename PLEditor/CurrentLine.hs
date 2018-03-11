{-# LANGUAGE OverloadedStrings #-}
module PLEditor.CurrentLine
  ( CurrentLine ()
  , emptyCurrentLine
  , startCurrentLine
  , completeCurrentLine
  , tryMoveCursorLeft
  , tryMoveCursorRight
  , insertAtCursor
  , deleteAtCursor
  )
  where

import Data.Monoid

import PLEditor.Line

-- | A current line is a line with a cursor positon.
newtype CurrentLine = CurrentLine
  { _unCurrentLine :: ( Line -- First half of line ordered right-to-left. The cursor is considered on the first character.
                      , Line -- Second half of line ordered left-to-right.
                      )
  }

-- | No text in the current line.
emptyCurrentLine
  :: CurrentLine
emptyCurrentLine = CurrentLine (textLine "",textLine "")

-- A current line starts at the first character.
startCurrentLine
  :: Line
  -> CurrentLine
startCurrentLine line = CurrentLine (textLine "",line)


-- Complete a current line by merging it into a single line, returning where the
-- cursor was.
completeCurrentLine
  :: CurrentLine
  -> (Line, Int)
completeCurrentLine currentLine =
  case currentLine of
    CurrentLine (linePrefix, lineSuffix)
      -> (reverseLine linePrefix <> lineSuffix, lineLength linePrefix)

-- Move a cursor left if possible.
tryMoveCursorLeft
  :: CurrentLine
  -> CurrentLine
tryMoveCursorLeft currentLine =
  case currentLine of
    CurrentLine (prefixes, suffixes)
      -> case firstCharacter prefixes of
           Nothing
             -> currentLine

           Just (c, remainingPrefixes)
             -> CurrentLine (remainingPrefixes, prefixWithCharacter c suffixes)

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
             -> CurrentLine (prefixWithCharacter c prefixes, remainingSuffixes)

insertAtCursor
  :: Char
  -> CurrentLine
  -> CurrentLine
insertAtCursor c currentLine =
  case currentLine of
    CurrentLine (prefixLine, suffixLine)
      -> CurrentLine (prefixWithCharacter c prefixLine, suffixLine)

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
      -> case firstCharacter prefixLine of
           Nothing
             -> (currentLine, Nothing)

           Just (deletedCharacter, remainingPrefix)
             -> (CurrentLine (remainingPrefix, suffixLine), Just deletedCharacter)

