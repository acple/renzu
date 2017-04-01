{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module Renzu.Index where

----------------------------------------------------------------

import Data.Profunctor

----------------------------------------------------------------

newtype Indexed i p s t = Indexed { runIndexed :: p (i, s) t }

instance Profunctor p => Profunctor (Indexed i p) where
    dimap f g = Indexed . dimap (second' f) g . runIndexed

instance Strong p => Strong (Indexed i p) where
    first'  = Indexed . lmap (\(i, (a, c)) -> ((i, a), c)) . first' . runIndexed
    second' = Indexed . lmap (\(i, (c, a)) -> (c, (i, a))) . second' . runIndexed

instance Choice p => Choice (Indexed i p) where
    left'  = Indexed . lmap (\(i, ac) -> either (\a -> Left (i, a)) Right ac) . left' . runIndexed
    right' = Indexed . lmap (\(i, ca) -> either Left (\a -> Right (i, a)) ca) . right' . runIndexed

----------------------------------------------------------------

class Indexable i p q where
    indexed :: p a b -> q (i, a) b

instance Profunctor p => Indexable i p p where
    indexed = lmap snd

instance i ~ j => Indexable i (Indexed j p) p where
    indexed = runIndexed
