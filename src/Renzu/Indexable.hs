{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, TypeFamilies #-}
module Renzu.Indexable where

----------------------------------------------------------------

import Renzu.Profunctor

----------------------------------------------------------------

class Indexable i p q | p -> q where
    indexed :: p a b -> q (i, a) b

instance Indexable i (->) (->) where
    indexed = uncurry . const
    {-# INLINE indexed #-}

instance Indexable i (Forget r) (Forget r) where
    indexed = lmap snd
    {-# INLINE indexed #-}

instance Profunctor p => Indexable i (Wrapped p) (Wrapped p) where
    indexed = lmap snd
    {-# INLINE indexed #-}

instance Indexable i (Capture a) (Capture a) where
    indexed = lmap snd
    {-# INLINE indexed #-}

instance Profunctor p => Indexable i (Re p s t) (Re p s t) where
    indexed = lmap snd
    {-# INLINE indexed #-}

instance i ~ j => Indexable i (Indexed j p) p where
    indexed = runIndexed
    {-# INLINE indexed #-}

