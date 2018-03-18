{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : PLEditor.View
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

A View is a two-dimensional view with a width and height that can be increased
and decreased in the range 0..
A View can be used to focus on a 'visible' portion of an Editors Lines.
-}
module PLEditor.View
  ( View ()
  , pattern ViewPattern
  , emptyView
  , widerView
  , tallerView

  , Width ()
  , emptyWidth
  , makeWidth

  , Height ()
  , emptyHeight
  , makeHeight
  )
  where

{- TODO:
 - Width and Height can be defined in terms of a common type and newtype'd.
-}

-- | Width is an integer 0 or greater.
newtype Width = Width { _unWidth :: Int }
  deriving (Eq, Ord)

-- | A Width of 0.
emptyWidth
  :: Width
emptyWidth = Width 0

-- | Construct a positive integer Width.
makeWidth
  :: Int
  -> Maybe Width
makeWidth i
  | 0 < i     = Just $ Width i
  | otherwise = Nothing

-- | Height is an integer 0 or greater.
newtype Height = Height { _unHeight :: Int }
  deriving (Eq, Ord)

-- | Construct a positive integer Height.
emptyHeight
  :: Height
emptyHeight = Height 0

-- | Construct a positive integer Height.
makeHeight
  :: Int
  -> Maybe Height
makeHeight i
  | 0 < i     = Just $ Height i
  | otherwise = Nothing

-- | A View is a two-dimensional view with a width and height that can be
-- increased and decreased in the range 0..
data View = View
  { _maxWidth  :: Width
  , _maxHeight :: Height
  }

-- | ViewPattern is a view pattern on view which exposes the width and height as
-- integers. It has a confusing name.
pattern ViewPattern w h = View (Width w) (Height h)

-- | A View with no Width or Height.
emptyView
  :: View
emptyView = View (Width 0) (Height 0)

-- | A View with a given Width and Height.
makeView
  :: Width
  -> Height
  -> View
makeView = View

-- | Increase (or decrease) the width of a View.
widerView
  :: Int
  -> View
  -> View
widerView dw view = case view of
  View (Width w) h
    | w+dw >= 0 -> View (Width $ w+dw) h
    | otherwise -> view

-- | Increase (or decrease) the height of a View.
tallerView
  :: Int
  -> View
  -> View
tallerView dh view = case view of
  View w (Height h)
    | h+dh >= 0 -> View w (Height $ h+dh)
    | otherwise -> view

