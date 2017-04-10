module Renzu.Profunctor.Indexed where

----------------------------------------------------------------

import Renzu.Profunctor.Class

----------------------------------------------------------------

newtype Indexed i p s t = Indexed { runIndexed :: p (i, s) t }

instance Profunctor p => Profunctor (Indexed i p) where
    dimap f g = Indexed . dimap (second f) g . runIndexed
    {-# INLINE dimap #-}

instance Strong p => Strong (Indexed i p) where
    first = Indexed . lmap (\(i, (a, c)) -> ((i, a), c)) . first . runIndexed
    {-# INLINE first #-}

    second = Indexed . lmap (\(i, (c, a)) -> (c, (i, a))) . second . runIndexed
    {-# INLINE second #-}

instance Choice p => Choice (Indexed i p) where
    left = Indexed . lmap (\(i, ac) -> either (\a -> Left (i, a)) Right ac) . left . runIndexed
    {-# INLINE left #-}

    right = Indexed . lmap (\(i, ca) -> either Left (\a -> Right (i, a)) ca) . right . runIndexed
    {-# INLINE right #-}
