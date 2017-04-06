{-# LANGUAGE FlexibleContexts #-}
module Renzu.Index where

----------------------------------------------------------------

import Data.Profunctor
import Renzu.Indexable
import Renzu.Optic

----------------------------------------------------------------

infixr 9 <., .>, <.>

----------------------------------------------------------------

(<.) :: Indexable i r q => IxOptic q i u v s t -> Optical (Wrapped p) (Wrapped r) s t a b -> IxOptical p q i u v a b
(<.) l r = l . Indexed . indexed . unWrap . r . Wrap
{-# INLINE (<.) #-}

(.>) :: Indexable i p q => Optical q r u v s t -> IxOptic q i s t a b -> IxOptical p r i u v a b
(.>) l r =  l . r . Indexed . indexed
{-# INLINE (.>) #-}

(<.>) :: Profunctor q => IxOptic q i u v s t -> IxOptic (Indexed i q) j s t a b -> IxOptical p q (i, j) u v a b
(<.>) = icompose (,)
{-# INLINE (<.>) #-}

icompose :: Profunctor q => (i -> j -> k) -> IxOptic q i u v s t -> IxOptic (Indexed i q) j s t a b -> IxOptical p q k u v a b
icompose f l r = l . r . Indexed . Indexed . lmap (\(i, (j, a)) -> (f i j, a)) . indexed
{-# INLINE icompose #-}

----------------------------------------------------------------

index :: (Profunctor q, Indexable i p q) => (a -> i) -> Optical q r s t a b -> IxOptical p r i s t a b
index f l = l . lmap (\a -> (f a, a)) . indexed
{-# INLINE index #-}

reindex :: Profunctor q => (i -> a -> j) -> IxOptic q i s t a b -> IxOptical p q j s t a b
reindex f l = l . Indexed . lmap (\(i, a) -> (f i a, a)) . indexed
{-# INLINE reindex #-}
