module Test.Smoke.Maps where

import Control.Monad ((>=>), foldM)
import Data.Map.Strict

mapFromTraversable :: (Foldable f, Ord a) => f (a, b) -> Map a b
mapFromTraversable = foldMap (uncurry singleton)

mapWithKeyM :: (Monad m, Ord k) => (k -> v1 -> m v2) -> Map k v1 -> m (Map k v2)
mapWithKeyM f oldMap = foldM transform empty (assocs oldMap)
  where
    transform newMap (key, oldValue) = do
      newValue <- f key oldValue
      return $ insert key newValue newMap

forWithKeyM :: (Monad m, Ord k) => Map k v1 -> (k -> v1 -> m v2) -> m (Map k v2)
forWithKeyM = flip mapWithKeyM

forWithKeyM_ :: (Monad m, Ord k) => Map k v1 -> (k -> v1 -> m v2) -> m ()
forWithKeyM_ theMap = forWithKeyM theMap >=> const (return ())
