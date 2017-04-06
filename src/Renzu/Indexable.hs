{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Renzu.Indexable where

----------------------------------------------------------------

import Data.Profunctor

----------------------------------------------------------------

newtype Wrapped p a b = Wrap { unWrap :: p a b }
    deriving (Profunctor, Strong, Choice)

----------------------------------------------------------------

newtype Indexed i p s t = Indexed { runIndexed :: p (i, s) t }

instance Profunctor p => Profunctor (Indexed i p) where
    dimap f g = Indexed . dimap (second' f) g . runIndexed
    {-# INLINE dimap #-}

instance Strong p => Strong (Indexed i p) where
    first'  = Indexed . lmap (\(i, (a, c)) -> ((i, a), c)) . first' . runIndexed
    second' = Indexed . lmap (\(i, (c, a)) -> (c, (i, a))) . second' . runIndexed
    {-# INLINE first' #-}
    {-# INLINE second' #-}

instance Choice p => Choice (Indexed i p) where
    left'  = Indexed . lmap (\(i, ac) -> either (\a -> Left (i, a)) Right ac) . left' . runIndexed
    right' = Indexed . lmap (\(i, ca) -> either Left (\a -> Right (i, a)) ca) . right' . runIndexed
    {-# INLINE left' #-}
    {-# INLINE right' #-}

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

instance i ~ j => Indexable i (Indexed j p) p where
    indexed = runIndexed
    {-# INLINE indexed #-}
