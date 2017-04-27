{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Renzu.Indexable where

----------------------------------------------------------------

import Renzu.Profunctor

----------------------------------------------------------------

class Indexable i p q | p -> q where
    indexed :: p a b -> q (i, a) b

    default indexed :: Profunctor p => p a b -> p (i, a) b
    indexed = lmap snd
    {-# INLINE indexed #-}

instance Indexable i (->) (->)

instance Indexable i (Forget r) (Forget r)

instance Indexable i (Capture a) (Capture a)

instance Profunctor p => Indexable i (Wrapped p) (Wrapped p)

instance Profunctor p => Indexable i (Re p s t) (Re p s t)

instance i ~ j => Indexable i (Indexed j p) p where
    indexed = runIndexed
    {-# INLINE indexed #-}
