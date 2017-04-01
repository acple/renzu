module Renzu.Lens where

----------------------------------------------------------------

import Data.Profunctor
import Renzu.Index
import Renzu.Optic

----------------------------------------------------------------

type Lens s t a b = forall p. Strong p => Optic p s t a b
type Lens' s a = Lens s s a a

type IxLens i s t a b = forall p q. (Strong p, Strong q) => IxOptical p q i s t a b
type IxLens' i s a = IxLens i s s a a

----------------------------------------------------------------

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
     -- (s -> a) -> (s -> b -> t) -> (forall p. Strong p => p a b -> p s t)
lens get set = dimap (\s -> (s, get s)) (uncurry set) . second'
{-# INLINE lens #-}

ilens :: (s -> (i, a)) -> (s -> b -> t) -> IxLens i s t a b
ilens get set = lens get set . indexed
{-# INLINE ilens #-}
