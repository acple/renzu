module Renzu.Lens where

----------------------------------------------------------------

import Renzu.Indexable
import Renzu.Iso
import Renzu.Optic
import Renzu.Profunctor

----------------------------------------------------------------

type Lens s t a b = forall p. Strong p => Optic p s t a b
type Lens' s a = Lens s s a a

type IxLens i s t a b = forall p q. Strong q => IxOptical p q i s t a b
type IxLens' i s a = IxLens i s s a a

----------------------------------------------------------------

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
     -- (s -> a) -> (s -> b -> t) -> (forall p. Strong p => p a b -> p s t)
lens get set = dimap (\s -> (s, get s)) (uncurry set) . second
{-# INLINE lens #-}

ilens :: (s -> (i, a)) -> (s -> b -> t) -> IxLens i s t a b
ilens get set = lens get set . indexed
{-# INLINE ilens #-}

----------------------------------------------------------------

to :: (s -> a) -> Iso s b a b
to get = iso get id
{-# INLINE to #-}

like :: a -> Iso s b a b
like = to . const
{-# INLINE like #-}

ito :: (s -> (i, a)) -> IxLens i s b a b
ito get = ilens get (const id)
{-# INLINE ito #-}

ilike :: i -> a -> IxLens i s b a b
ilike i a = ito . const $ (i, a)
{-# INLINE ilike #-}
