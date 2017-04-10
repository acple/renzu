module Renzu.Profunctor.Forget where

----------------------------------------------------------------

import Renzu.Profunctor.Class

----------------------------------------------------------------

newtype Forget r a b = Forget { runForget :: a -> r }

instance Profunctor (Forget r) where
    dimap f _ (Forget k) = Forget (k . f)
    {-# INLINE dimap #-}

    rmap _ = Forget . runForget
    {-# INLINE rmap #-}

instance Strong (Forget r) where
    first (Forget k) = Forget (k . fst)
    {-# INLINE first #-}

    second (Forget k) = Forget (k . snd)
    {-# INLINE second #-}

instance Monoid r => Choice (Forget r) where
    left (Forget k) = Forget (either k mempty)
    {-# INLINE left #-}

    right (Forget k) = Forget (either mempty k)
    {-# INLINE right #-}
