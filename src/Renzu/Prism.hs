module Renzu.Prism where

----------------------------------------------------------------

import Control.Monad
import Renzu.Profunctor
import Text.Read
import Renzu.Optic

----------------------------------------------------------------

type Prism s t a b = forall p. Choice p => Optic p s t a b
type Prism' s a = Prism s s a a

----------------------------------------------------------------

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
      -- (b -> t) -> (s -> Either t a) -> (forall p. Choice p => p a b -> p s t)
prism to from = dimap from (either id to) . right
{-# INLINE prism #-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' to from = prism to $ \s -> maybe (Left s) Right $ from s
{-# INLINE prism' #-}

----------------------------------------------------------------

_Left :: Prism (Either a c) (Either b c) a b
_Left = left
{-# INLINE _Left #-}

_Right :: Prism (Either c a) (Either c b) a b
_Right = right
{-# INLINE _Right #-}

----------------------------------------------------------------

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right
{-# INLINE _Just #-}

_Nothing :: Prism (Maybe a) (Maybe b) () b
_Nothing = prism Just $ maybe (Right ()) (Left . const Nothing)
{-# INLINE _Nothing #-}

----------------------------------------------------------------

_Show :: (Read a, Show a) => Prism' String a
_Show = prism' show readMaybe
{-# INLINE _Show #-}

----------------------------------------------------------------

only :: Eq a => a -> Prism' a ()
only a = nearly a (== a)
{-# INLINE only #-}

nearly :: a -> (a -> Bool) -> Prism' a ()
nearly a p = prism' (const a) (guard . p)
{-# INLINE nearly #-}
