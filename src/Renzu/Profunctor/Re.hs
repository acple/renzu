module Renzu.Profunctor.Re where

----------------------------------------------------------------

import Renzu.Profunctor.Class

----------------------------------------------------------------

newtype Re p s t a b = Re { runRe :: p b a -> p t s } -- Optic p t s b a

instance Profunctor p => Profunctor (Re p s t) where
    dimap f g (Re l) = Re (l . dimap g f)
    {-# INLINE dimap #-}

instance Costrong p => Strong (Re p s t) where
    first (Re l) = Re (l . unfirst)
    {-# INLINE first #-}

    second (Re l) = Re (l . unsecond)
    {-# INLINE second #-}

instance Cochoice p => Choice (Re p s t) where
    left (Re l) = Re (l . unleft)
    {-# INLINE left #-}

    right (Re l) = Re (l . unright)
    {-# INLINE right #-}

instance Strong p => Costrong (Re p s t) where
    unfirst (Re l) = Re (l . first)
    {-# INLINE unfirst #-}

    unsecond (Re l) = Re (l . second)
    {-# INLINE unsecond #-}

instance Choice p => Cochoice (Re p s t) where
    unleft (Re l) = Re (l . left)
    {-# INLINE unleft #-}

    unright (Re l) = Re (l . right)
    {-# INLINE unright #-}
