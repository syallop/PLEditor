module PLEditor.Editor
  ( Editor ()
  , editorLines
  , tryMoveLeft
  , tryMoveRight
  , tryMoveDown
  , tryMoveUp
  , insertCharacter
  , deleteCharacter
  )
  where

import Data.Monoid

import PLEditor.Lines
import PLEditor.Line
import PLEditor.CurrentLine

-- | An Editor is a collection of lines with a current cursor position which
-- can be (semi-)efficiently traversed and characters inserted and deleted.
data Editor = Editor
  { _priorLines  :: Lines       -- Previous lines ordered bottom-to-top.
  , _currentLine :: CurrentLine -- Current line with cursor.
  , _afterLines  :: Lines       -- Lines after the current line ordered top-to-bottom.
  }

-- | Convert an Editor to a collection of its lines top-to-bottom and
-- left-to-right.
editorLines
  :: Editor
  -> Lines
editorLines editor = case editor of
  Editor priorLines currentLine afterLines
    -> let (completedCurrentLine, cursorColumn) = completeCurrentLine currentLine
        in reverseLines priorLines <> singletonLines completedCurrentLine <> afterLines

-- | If it is possible to move left, do so. If not, silently don't.
tryMoveLeft
  :: Editor
  -> Editor
tryMoveLeft editor = case editor of
  Editor priorLines currentLine afterLines
    -> Editor priorLines (tryMoveCursorLeft currentLine) afterLines

-- | If it is possible to move right, do so. If not, silently don't.
tryMoveRight
  :: Editor
  -> Editor
tryMoveRight editor = case editor of
  Editor priorLines currentLine afterLines
    -> Editor priorLines (tryMoveCursorRight currentLine) afterLines

-- | If it is possible to move down, do so. If not, silently dont.
--
-- The cursor within the line is reset to the start of the line.
tryMoveDown
  :: Editor
  -> Editor
tryMoveDown editor = case editor of
  Editor priorLines currentLine afterLines
    -> case firstLine afterLines of
         Nothing
           -> editor

         Just (nextLine, remainingAfters)
           -> let (completedCurrentLine, cursorColumn) = completeCurrentLine currentLine
                  newCurrentLine = startCurrentLine nextLine
                  newPriorLines = prependLine completedCurrentLine priorLines
               in Editor newPriorLines newCurrentLine remainingAfters

-- | If it is possible to move up, do so. If not, silently dont.
--
-- The cursor within the line is reset to the start of the line.
tryMoveUp
  :: Editor
  -> Editor
tryMoveUp editor = case editor of
  Editor priorLines currentLine afterLines
    -> case firstLine priorLines of
         Nothing
           -> editor

         Just (nextLine, remainingPriors)
           -> let (completedCurrentLine, cursorColumn) = completeCurrentLine currentLine
                  newCurrentLine = startCurrentLine nextLine
                  newAfterLines = prependLine completedCurrentLine afterLines
               in Editor remainingPriors newCurrentLine newAfterLines

-- | Insert a single character into the current line.
insertCharacter
  :: Char
  -> Editor
  -> Editor
insertCharacter c editor = case editor of
  Editor priorLines currentLine afterLines
    -> Editor priorLines (insertAtCursor c currentLine) afterLines

-- | Delete a character in the current line.
--
-- 'deleteAtCursor' almost justifies the unusual type signature.
deleteCharacter
  :: Editor
  -> (Editor, Maybe Char)
deleteCharacter editor = case editor of
  Editor priorLine currentLine afterLines
    -> case deleteAtCursor currentLine of
         (newCurrentLine, mDeletedCharacter)
           -> (Editor priorLine newCurrentLine afterLines, mDeletedCharacter)

