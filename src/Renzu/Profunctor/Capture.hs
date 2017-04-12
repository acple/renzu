module Renzu.Profunctor.Capture where

----------------------------------------------------------------

import Renzu.Profunctor.Class

----------------------------------------------------------------

newtype Capture r a b = Capture { runCapture :: a -> Either b r }

instance Profunctor (Capture r) where
    dimap f g (Capture k) = Capture $ either (Left . g) Right . k . f
    {-# INLINE dimap #-}

    lmap f (Capture k) = Capture $ k . f
    {-# INLINE lmap #-}

instance Strong (Capture r) where
    first (Capture k) = Capture $ \(a, c) -> either (\b -> Left (b, c)) Right $ k a
    {-# INLINE first #-}

    second (Capture k) = Capture $ \(c, a) -> either (\b -> Left (c, b)) Right $ k a
    {-# INLINE second #-}

instance Choice (Capture r) where
    left (Capture k) = Capture $ either (either (Left . Left) Right . k) (Left . Right)
    {-# INLINE left #-}

    right (Capture k) = Capture $ either (Left . Left) (either (Left . Right) Right . k)
    {-# INLINE right #-}
