module Renzu.Setter
    ( (&)
    , over
    , (%~), (.~), (+~), (-~), (*~), (//~), (||~), (&&~), (<>~), (?~)
    , modifying
    , (%=), (.=), (+=), (-=), (*=), (//=), (||=), (&&=), (<>=), (?=)
    ) where

----------------------------------------------------------------

import Control.Monad.State.Class
import Data.Semigroup
import Renzu.Optic

----------------------------------------------------------------

type Setter s t a b = Optic (->) s t a b
type Setter' s a = Setter s s a a

----------------------------------------------------------------

infixr 4 %~, .~, +~, -~, *~, //~, ||~, &&~, <>~, ?~
infix  4 %=, .=, +=, -=, *=, //=, ||=, &&=, <>=, ?=

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixl 1 &
{-# INLINE (&) #-}

----------------------------------------------------------------

over :: Setter s t a b -> (a -> b) -> s -> t
     -- ((a -> b) -> (s -> t)) -> (a -> b) -> (s -> t)
over = id
(%~) :: Setter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE over #-}
{-# INLINE (%~) #-}

_apply :: (a -> c -> b) -> Setter s t a b -> c -> s -> t
_apply op = (. flip op)
-- _apply op l c = l %~ (`op` c)
{-# INLINE _apply #-}

(.~) :: Setter s t a b -> b -> s -> t
(.~) = _apply $ flip const
{-# INLINE (.~) #-}

(+~) :: Num a => Setter s t a a -> a -> s -> t
(+~) = _apply (+)
{-# INLINE (+~) #-}

(-~) :: Num a => Setter s t a a -> a -> s -> t
(-~) = _apply (-)
{-# INLINE (-~) #-}

(*~) :: Num a => Setter s t a a -> a -> s -> t
(*~) = _apply (*)
{-# INLINE (*~) #-}

(//~) :: Fractional a => Setter s t a a -> a -> s -> t
(//~) = _apply (/)
{-# INLINE (//~) #-}

(||~) :: Setter s t Bool Bool -> Bool -> s -> t
(||~) = _apply (||)
{-# INLINE (||~) #-}

(&&~) :: Setter s t Bool Bool -> Bool -> s -> t
(&&~) = _apply (&&)
{-# INLINE (&&~) #-}

(<>~) :: Semigroup a => Setter s t a a -> a -> s -> t
(<>~) = _apply (<>)
{-# INLINE (<>~) #-}

(?~) :: Setter s t a (Maybe b) -> b -> s -> t
(?~) = _apply $ flip (const . Just)
{-# INLINE (?~) #-}

----------------------------------------------------------------

modifying :: MonadState s m => Setter s s a b -> (a -> b) -> m ()
modifying l = modify . l
(%=) :: MonadState s m => Setter s s a b -> (a -> b) -> m ()
(%=) = modifying
{-# INLINE modifying #-}
{-# INLINE (%=) #-}

_modify :: MonadState s m => (a -> c -> b) -> Setter s s a b -> c -> m ()
_modify op = (. flip op) . (%=)
-- _modify op l c = l %= (`op` c)
{-# INLINE _modify #-}

(.=) :: MonadState s m => Setter s s a b -> b -> m ()
(.=) = _modify $ flip const
{-# INLINE (.=) #-}

(+=) :: (MonadState s m, Num a) => Setter' s a -> a -> m ()
(+=) = _modify (+)
{-# INLINE (+=) #-}

(-=) :: (MonadState s m, Num a) => Setter' s a -> a -> m ()
(-=) = _modify (-)
{-# INLINE (-=) #-}

(*=) :: (MonadState s m, Num a) => Setter' s a -> a -> m ()
(*=) = _modify (*)
{-# INLINE (*=) #-}

(//=) :: (MonadState s m, Fractional a) => Setter' s a -> a -> m ()
(//=) = _modify (/)
{-# INLINE (//=) #-}

(||=) :: MonadState s m => Setter' s Bool -> Bool -> m ()
(||=) = _modify (||)
{-# INLINE (||=) #-}

(&&=) :: MonadState s m => Setter' s Bool -> Bool -> m ()
(&&=) = _modify (&&)
{-# INLINE (&&=) #-}

(<>=) :: (MonadState s m, Semigroup a) => Setter' s a -> a -> m ()
(<>=) = _modify (<>)
{-# INLINE (<>=) #-}

(?=) :: MonadState s m => Setter s s a (Maybe b) -> b -> m ()
(?=) = _modify $ flip (const . Just)
{-# INLINE (?=) #-}
