{-# LANGUAGE
    PatternSynonyms
  , DataKinds
#-}
{-|
Module      : PLEditor.Editor
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

An Editor is a collection of lines with a notion of a 'current line'. This line
has a 'cursor' position where characters may be inserted or deleted. The cursor
may be moved within and across Lines.
-}
module PLEditor.Editor
  ( Editor ()
  , editorLines
  , makeEditor
  , viewEditor
  , tryMoveLeft
  , tryMoveRight
  , tryMoveDown
  , tryMoveUp
  , insertCharacter
  , deleteCharacter
  , newline
  )
  where

{- TODO:
 - Rendering an Editor via a view can likely be improved by changing/ merging
   data structures. In particular, views are expected to change size infrequently.
   Therefore lines could cache their visible and invisible portions such that
   rendering doesnt require an O(n) take for each visible line.
   Perhaps `data CachedLine = CachedLine Line (Maybe CachedText)`
           `data CachedText = CachedText Text Int`
   could allow a line not to be re-rendered if there is an existing cache or the
   correct width that hasnt been invalidated.

 - Line could be abstracted over the contained data type. Text/ ByteString/
   FormattedText etc.

 - Editors could be supplied with a PLGrammar. Only valid nodes could be
   permitted to be 'finalized'/ the Grammar could be used for tab completion.
   Such completion might also need a supplied state monad for things such as
   identifier names/ type safe suggestions. Alternatively, this could be the
   responsiblity of the user and we should remain a dumb text editor.

 - Multiple cursors.
-}

import Control.Arrow
import Data.Monoid

import PLEditor.CurrentLine
import PLEditor.Line
import PLEditor.Lines
import PLEditor.View

-- | An Editor is a collection of lines with a current cursor position which
-- can be (semi-)efficiently traversed and characters inserted and deleted.
data Editor = Editor
  { _priorLines  :: Lines 'BottomToTop -- Previous lines ordered bottom-to-top.
  , _currentLine :: CurrentLine        -- Current line with cursor.
  , _afterLines  :: Lines 'TopToBottom -- Lines after the current line ordered top-to-bottom.
  }

-- | Create an Editor from a collection of Lines.
makeEditor
  :: Lines 'TopToBottom
  -> Editor
makeEditor ls =
  let (currentLine, afterLines) = case firstLine ls of
                                    Nothing
                                      -> (emptyCurrentLine  , emptyLines)
                                    Just (l, ls)
                                      -> (startCurrentLine l, ls)
   in Editor
       { _priorLines  = emptyLines
       , _currentLine = currentLine
       , _afterLines  = afterLines
       }

-- | Convert an Editor to a collection of its lines top-to-bottom and
-- left-to-right.
editorLines
  :: Editor
  -> Lines 'TopToBottom
editorLines editor = case editor of
  Editor priorLines currentLine afterLines
    -> let (completedCurrentLine, cursorColumn) = completeCurrentLine currentLine
        in reverseLines priorLines <> singletonLines completedCurrentLine <> afterLines

-- | Convert an Editor to a collection of its lines top-to-bottom and
-- left-to-right where lines fall within a given view.
viewEditor
  :: View
  -> Editor
  -> Lines 'TopToBottom
viewEditor (ViewPattern w h) (Editor priorLines currentLine afterLines) =
  let (completedCurrentLine, cursorColumn) = first (takeFromLine w) $ completeCurrentLine currentLine

      -- Current line takes up a row.
      remainingAfterHeight = h - 1

      -- Take as many lines remaining as possible from after the after lines.
      (remainingAfterLines,remainingPriorHeight) = takeLines remainingAfterHeight afterLines

      -- Take as many lines remaining as possible from the prior lines.
      (remainingPriorLines,remainingHeight) = takeLines remainingPriorHeight priorLines
   in mconcat
        [ reverseLines $ mapLines (takeFromLine w) remainingPriorLines
        , singletonLines completedCurrentLine
        , mapLines (takeFromLine w) remainingAfterLines
        ]

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
                  newPriorLines = postpendLine completedCurrentLine priorLines
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

-- | Insert a new empty line below.
newline
  :: Editor
  -> Editor
newline editor = case editor of
  Editor priorLines currentLine afterLines
    -> let (completedCurrentLine, cursorColumn) = completeCurrentLine currentLine
           newCurrentLine = emptyCurrentLine
           newPriorLines = postpendLine completedCurrentLine priorLines
        in Editor newPriorLines newCurrentLine afterLines

