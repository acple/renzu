module Renzu.Profunctor.Class where

----------------------------------------------------------------

import Data.Tuple

----------------------------------------------------------------

class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    lmap  :: (a -> b) -> p b c -> p a c
    rmap  :: (b -> c) -> p a b -> p a c
    {-# MINIMAL dimap | lmap, rmap #-}

    dimap f g = lmap f . rmap g
    {-# INLINE dimap #-}

    lmap f = dimap f id
    {-# INLINE lmap #-}

    rmap = dimap id
    {-# INLINE rmap #-}

instance Profunctor (->) where
    dimap ab cd bc = cd . bc . ab
    {-# INLINE dimap #-}

    lmap = flip (.)
    {-# INLINE lmap #-}

    rmap = (.)
    {-# INLINE rmap #-}

----------------------------------------------------------------

class Profunctor p => Strong p where
    first  :: p a b -> p (a, c) (b, c)
    second :: p a b -> p (c, a) (c, b)
    {-# MINIMAL first | second #-}

    first = dimap swap swap . second
    {-# INLINE first #-}

    second = dimap swap swap . first
    {-# INLINE second #-}

instance Strong (->) where
    first f ~(a, c) = (f a, c)
    {-# INLINE first #-}

    second f ~(c, a) = (c, f a)
    {-# INLINE second #-}

----------------------------------------------------------------

class Profunctor p => Choice p where
    left  :: p a b -> p (Either a c) (Either b c)
    right :: p a b -> p (Either c a) (Either c b)
    {-# MINIMAL left | right #-}

    left = dimap (either Right Left) (either Right Left) . right
    {-# INLINE left #-}

    right = dimap (either Right Left) (either Right Left) . left
    {-# INLINE right #-}

instance Choice (->) where
    left f (Left a)  = Left (f a)
    left _ (Right a) = Right a
    {-# INLINE left #-}

    right = fmap
    {-# INLINE right #-}

----------------------------------------------------------------

class Profunctor p => Costrong p where
    unfirst  :: p (a, c) (b, c) -> p a b
    unsecond :: p (c, a) (c, b) -> p a b
    {-# MINIMAL unfirst | unsecond #-}

    unfirst = unsecond . dimap swap swap
    {-# INLINE unfirst #-}

    unsecond = unfirst . dimap swap swap
    {-# INLINE unsecond #-}

----------------------------------------------------------------

class Profunctor p => Cochoice p where
    unleft  :: p (Either a c) (Either b c) -> p a b
    unright :: p (Either c a) (Either c b) -> p a b
    {-# MINIMAL unleft | unright #-}

    unleft = unright . dimap (either Right Left) (either Right Left)
    {-# INLINE unleft #-}

    unright = unleft . dimap (either Right Left) (either Right Left)
    {-# INLINE unright #-}
