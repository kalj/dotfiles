{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
{- | Add this file to ~\/.xmonad/lib

Example Usage:

> import MaxWidth
> narrowXTermTall = maxWidth [(ClassName "XTerm", 400)] (Tall 1 0.3 0.5)

-}
module MaxWidth (
    -- * Layout Modifier
    maxWidth,
    module XMonad.Util.WindowProperties
    -- * TODO
    -- $todo
    ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Util.WindowProperties

import Control.Monad.Cont
import Data.Maybe

maxWidth ps layout = ModifiedLayout (MaxWidth ps) layout

data MaxWidth a = MaxWidth [(Property, Dimension)] deriving (Read,Show)

instance LayoutModifier MaxWidth Window where
    redoLayout mw _ _ arrs = do
        arrs <- shrink mw arrs
        return (arrs, Nothing)


shrink (MaxWidth ps) arrs = forM arrs $ \(win, r) -> do
    wid <- flip runContT return $ callCC $ \k -> do
        forM_ ps $ \(property, wid) -> do
            b <- lift (hasProperty property win)
            -- get the width of the first property to match
            if b then k (Just wid) else return ()
        return Nothing
    return $
        (win, case r of
            Rectangle x y w h
                -> Rectangle x y (fromMaybe w wid `min` w) h)

{- $todo

* combine multiple properties that match (union of reduced dimensions),
  which also gets rid of the 'ContT' nonsense

* specify height

* consistently move windows towards the center (or edge of the screen)

* examples

-}
