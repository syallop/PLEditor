# PLEditor - Experimental

This package exports datastructures and functions for building a text editor. It
is intended to be used with PL. It's gimics are briefly described below.

## Data Structures
### Line
A `Line` is a single line of Text which may be appended and taken from. A Line
encodes the horizontal direction it's text flows at the type-level like: 

```haskell
newtype Line (hDir :: HDir) = Line Text
```

This is used to force the consumer of the API to be explicit about performing
operations with high time-complexity. For example, as a Line is represented as a
string of `Text`, taking the first character is `O(1)` while taking the last is
`O(n)`. The API therefore only exports functions for taking the first character
of a `LeftToRight` Line and the last character of a `RightToLeft` Line.

```haskell
firstCharacter :: Line 'LeftToRight -> Maybe (Char, Line 'LeftToRight)
lastCharacter :: Line 'RightToLeft -> Maybe (Char, Line 'RightToLeft)
```

This aims to encourage using the datastructure in a performant manner. To take
the last character of a LeftToRight Line (or vice-versa), the caller must first
explicitly reverse the line:

```haskell
reverseLine :: Line hDir -> Line (ReverseH hDir)
reverseLine :: Line 'LeftToRight -> Line 'RightToLeft
reverseLine :: Line 'RightToLeft -> Line 'LeftToRight
```

### Lines
`Lines` extend `Line` into two dimensions and are an ordered collection of `Line`s
which may be appended together with `Line`s able to be inserted or removed.
As with single `Line`s which have a horizontal direction, a collection of
`Lines` have a vertical direction encoded in their type. This looks like:

```haskell
data Lines (vDir :: VDir) (hDir :: HDir) = Lines [Line hDir]
```

The API for `Lines` guides against inefficient operations in the same way as
`Line`. For example, the first and last line can only be taken from `TopToBottom`
and `BottomToTop` oriented lines, respectively.

```haskell
firstLine :: Lines 'TopToBottom hDir -> Maybe (Line hDir, Lines 'TopToBottom hDir)
lastLine  :: Lines 'BottomToTop hDir -> Maybe (Line hDir, Lines 'BottomToTop hDir)
```

### CurrentLine 
CurrentLine encodes a single `Line` with a cursor position at which characters
can be inserted or deleted. To make these operations efficient, a 'zipper' like
encoding is used. In short we break the line into two, with the first half
RightToLeft and the second half LeftToRight. Inserting and deleteing characters
are a cons or uncons to the first half. Moving left or right is an uncons
followed by a cons in the respective direction. Internally, the structure looks
something like:

```haskell
newtype CurrentLine = CurrentLine
  { _unCurrentLine :: ( Line 'RightToLeft -- First half of line ordered right-to-left. The cursor is considered on the first character.
                      , Line 'LeftToRight -- Second half of line ordered left-to-right.
                      )
}
```

Movement functions cannot currenly fail. For example, if we are at the leftmost
side of a line and try and move left, the function will not fail and no movement
will be performed. This behavior is chosen as it appears to be a good default
for a text-editor.

### Editor
An Editor is a collection of `Lines` with a notion of a `CurrentLine`. This line
has a cursor position where characters may be inserted or deleted. The cursor
may be moved within and across Lines.

This structure is represented as 'zipper' over `Lines` using `CurrentLine` as
it's focussed Line. I.E. it looks like 

```
data Editor = Editor
  { _priorLines  :: Lines 'BottomToTop 'LeftToRight -- Previous lines ordered bottom-to-top.
  , _currentLine :: CurrentLine                     -- Current line with cursor.
  , _afterLines  :: Lines 'TopToBottom 'LeftToRight -- Lines after the current line ordered top-to-bottom.
}

```

### View
A `View` is a two-dimensional view with a width and height that can be increased
and decreased to zero or greater. A View can be used to focus on a 'visible'
portion of an Editors Lines with
`viewEditor :: View -> Editor -> (Lines 'TopToBottom 'LeftToRight, (Int,Int))`.
This function returns the postion the cursor was in as a character offset from the top-left.

