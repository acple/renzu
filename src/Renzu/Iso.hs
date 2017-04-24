module Renzu.Iso where

----------------------------------------------------------------

import Renzu.Optic
import Renzu.Profunctor

----------------------------------------------------------------

type Iso s t a b = forall p. Profunctor p => Optic p s t a b
type Iso' s a = Iso s s a a

----------------------------------------------------------------

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap
{-# INLINE iso #-}

re :: Optic (Re p a b) s t a b -> Optic p b a t s
re l = runRe . l . Re $ id
{-# INLINE re #-}
