-----------------------------------------------------------------------------
-- |
-- Module      :  Point
-- License     :  Unlicense
--
-- Maintainer  :  mail@johannesewald.de
-- Stability   :  unstable
-- Portability :  portable
--
-- The Point data type.
--
-----------------------------------------------------------------------------

module Point (
    Point
    ) where

{-|
    A tuple of x and y which describe a point in a 2-dimensional space.
-}
type Point = (Float, Float)
