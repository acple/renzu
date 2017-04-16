module Renzu.Profunctor.Wrapped
    ( Wrapped(..)
    ) where

----------------------------------------------------------------

import Renzu.Profunctor.Class

----------------------------------------------------------------

newtype Wrapped p a b = Wrap { unWrap :: p a b }

_map :: (p a b -> q s t) -> Wrapped p a b -> Wrapped q s t
_map f = Wrap . f . unWrap
{-# INLINE _map #-}

instance Profunctor p => Profunctor (Wrapped p) where
    dimap f g = _map (dimap f g)
    {-# INLINE dimap #-}

    lmap = _map . lmap
    {-# INLINE lmap #-}

    rmap = _map . rmap
    {-# INLINE rmap #-}

instance Strong p => Strong (Wrapped p) where
    first = _map first
    {-# INLINE first #-}

    second = _map second
    {-# INLINE second #-}

instance Choice p => Choice (Wrapped p) where
    left = _map left
    {-# INLINE left #-}

    right = _map right
    {-# INLINE right #-}
