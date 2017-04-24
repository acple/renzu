module Renzu.Profunctor.Indexed
    ( Indexed(..)
    ) where

----------------------------------------------------------------

import Renzu.Profunctor.Class

----------------------------------------------------------------

newtype Indexed i p s t = Indexed { runIndexed :: p (i, s) t }

_map :: (p (i, a) b -> q (j, s) t) -> Indexed i p a b -> Indexed j q s t
_map f = Indexed . f . runIndexed
{-# INLINE _map #-}

instance Profunctor p => Profunctor (Indexed i p) where
    dimap f g = _map $ dimap (second f) g
    {-# INLINE dimap #-}

instance Strong p => Strong (Indexed i p) where
    first = _map $ lmap (\(i, (a, c)) -> ((i, a), c)) . first
    {-# INLINE first #-}

    second = _map $ lmap (\(i, (c, a)) -> (c, (i, a))) . second
    {-# INLINE second #-}

instance Choice p => Choice (Indexed i p) where
    left = _map $ lmap (\(i, ac) -> either (Left . (,) i) Right ac) . left
    {-# INLINE left #-}

    right = _map $ lmap (\(i, ca) -> fmap ((,) i) ca) . right
    {-# INLINE right #-}

instance Costrong p => Costrong (Indexed i p) where
    unfirst = _map $ unfirst . lmap (\((i, a), c) -> (i, (a, c)))
    {-# INLINE unfirst #-}

    unsecond = _map $ unsecond . lmap (\(c, (i, a)) -> (i, (c, a)))
    {-# INLINE unsecond #-}
