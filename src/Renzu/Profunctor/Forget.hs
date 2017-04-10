{-# LANGUAGE FlexibleInstances #-}
module Renzu.Profunctor.Forget where

----------------------------------------------------------------

import Renzu.Profunctor.Class

----------------------------------------------------------------

newtype Forget r a b = Forget { runForget :: a -> r }

instance Profunctor (Forget r) where
    dimap f _ (Forget a) = Forget (a . f)
    {-# INLINE dimap #-}

    rmap _ = Forget . runForget
    {-# INLINE rmap #-}

instance Strong (Forget r) where
    first (Forget f) = Forget (f . fst)
    {-# INLINE first #-}

    second (Forget f) = Forget (f . snd)
    {-# INLINE second #-}

instance Monoid r => Choice (Forget r) where
    left (Forget f) = Forget $ either f (const mempty)
    {-# INLINE left #-}

    right (Forget f) = Forget $ either (const mempty) f
    {-# INLINE right #-}
