module Renzu.Getter where

----------------------------------------------------------------

import Data.Monoid
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Profunctor
import Renzu.Indexable
import Renzu.Optic

----------------------------------------------------------------

type Fold r s t a b = Optic (Forget r) s t a b
type Fold' r s a = Fold r s s a a

type Getter s t a b = Fold a s t a b
type Getter' s a = Getter s s a a

type IxFold r i s t a b = IxOptic (Forget r) i s t a b
type IxFold' r i s a = IxFold r i s s a a

type IxGetter i s t a b = IxFold (i, a) i s t a b
type IxGetter' i s a = IxGetter i s s a a

----------------------------------------------------------------

infixr 8 ^., ^?, ^@., ^@?

----------------------------------------------------------------

view :: MonadReader s m => Getter s t a b -> m a
view l = views l id
{-# INLINE view #-}

views :: MonadReader s m => Fold r s t a b -> (a -> r) -> m r
      -- (Forget r a b -> Forget r s t) -> (a -> r) -> (s -> r)
views l = asks . runForget . l . Forget
{-# INLINE views #-}

(^.) :: s -> Getter s t a b -> a
(^.) = flip view
{-# INLINE (^.) #-}

to :: (s -> a) -> Fold r s t a b
   -- (s -> a) -> Forget r a b -> Forget r s t
to sa = Forget . runForget . lmap sa
{-# INLINE to #-}

like :: a -> Fold r s t a b
like = to . const
{-# INLINE like #-}

use :: MonadState s m => Getter s t a b -> m a
use = gets . view
{-# INLINE use #-}

uses :: MonadState s m => Fold r s t a b -> (a -> r) -> m r
uses l = gets . views l
{-# INLINE uses #-}

preview :: MonadReader s m => Fold (Alt Maybe a) s t a b -> m (Maybe a)
preview l = previews l id
{-# INLINE preview #-}

previews :: MonadReader s m => Fold (Alt Maybe r) s t a b -> (a -> r) -> m (Maybe r)
previews l f = fmap getAlt . views l $ Alt . Just . f
{-# INLINE previews #-}

(^?) :: s -> Fold (Alt Maybe a) s t a b -> Maybe a
(^?) = flip preview
{-# INLINE (^?) #-}

----------------------------------------------------------------

iview :: MonadReader s m => IxGetter i s t a b -> m (i, a)
iview l = iviews l (,)
{-# INLINE iview #-}

iviews :: MonadReader s m => IxFold r i s t a b -> (i -> a -> r) -> m r
iviews l = asks . runForget . l . Indexed . Forget . uncurry
{-# INLINE iviews #-}

(^@.) :: s -> IxGetter i s t a b -> (i, a)
(^@.) = flip iview
{-# INLINE (^@.) #-}

ito :: (s -> (i, a)) -> IxFold r i s t a b
ito sia = to sia . runIndexed
{-# INLINE ito #-}

ilike :: (i, a) -> IxFold r i s t a b
ilike = ito . const
{-# INLINE ilike #-}

iuse :: MonadState s m => IxGetter i s t a b -> m (i, a)
iuse = gets . iview
{-# INLINE iuse #-}

iuses :: MonadState s m => IxFold r i s t a b -> (i -> a -> r) -> m r
iuses l = gets . iviews l
{-# INLINE iuses #-}

ipreview :: MonadReader s m => IxFold (Alt Maybe (i, a)) i s t a b -> m (Maybe (i, a))
ipreview l = ipreviews l (,)
{-# INLINE ipreview #-}

ipreviews :: MonadReader s m => IxFold (Alt Maybe r) i s t a b -> (i -> a -> r) -> m (Maybe r)
ipreviews l f = fmap getAlt . iviews l $ \i -> Alt . Just . f i
{-# INLINE ipreviews #-}

(^@?) :: s -> IxFold (Alt Maybe (i, a)) i s t a b -> Maybe (i, a)
(^@?) = flip ipreview
{-# INLINE (^@?) #-}
