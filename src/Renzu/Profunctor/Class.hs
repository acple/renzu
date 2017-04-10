module Renzu.Profunctor.Class where

----------------------------------------------------------------

class Profunctor p where
    {-# MINIMAL dimap | (lmap, rmap) #-}

    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g
    {-# INLINE dimap #-}

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id
    {-# INLINE lmap #-}

    rmap :: (b -> c) -> p a b -> p a c
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

instance Strong (->) where
    first f ~(a, c) = (f a, c)
    {-# INLINE first #-}

    second = fmap
    {-# INLINE second #-}

----------------------------------------------------------------

class Profunctor p => Choice p where
    left  :: p a b -> p (Either a c) (Either b c)
    right :: p a b -> p (Either c a) (Either c b)

instance Choice (->) where
    left f (Left a) = Left (f a)
    left _ (Right a) = Right a
    {-# INLINE left #-}

    right = fmap
    {-# INLINE right #-}
