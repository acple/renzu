module Renzu.Setter
    ( Setter, Setter', IxSetter, IxSetter'
    , (&)
    , over, set
    , (%~), (.~), (+~), (-~), (*~), (//~), (||~), (&&~), (<>~), (?~)
    , modifying, assign
    , (%=), (.=), (+=), (-=), (*=), (//=), (||=), (&&=), (<>=), (?=), (<~)
    , iover, iset, imodifying, iassign
    , (%@~), (.@~), (%@=), (.@=)
    ) where

----------------------------------------------------------------

import Control.Monad.State.Class
import Data.Semigroup
import Renzu.Index
import Renzu.Optic

----------------------------------------------------------------

type Setter s t a b = Optic (->) s t a b
type Setter' s a = Setter s s a a

type IxSetter i s t a b = IxOptic (->) i s t a b
type IxSetter' i s a = IxSetter i s s a a

----------------------------------------------------------------

infixr 4 %~, .~, +~, -~, *~, //~, ||~, &&~, <>~, ?~
infix  4 %=, .=, +=, -=, *=, //=, ||=, &&=, <>=, ?=
infixr 2 <~
infixr 4 %@~, .@~
infix  4 %@=, .@=

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

set :: Setter s t a b -> b -> s -> t
set l = l . const
(.~) :: Setter s t a b -> b -> s -> t
(.~) = set
{-# INLINE set #-}
{-# INLINE (.~) #-}

_apply :: (a -> c -> b) -> Setter s t a b -> c -> s -> t
_apply op = (. flip op)
-- _apply op l c = l %~ (`op` c)
{-# INLINE _apply #-}

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
(?~) = _apply (const Just)
{-# INLINE (?~) #-}

----------------------------------------------------------------

modifying :: MonadState s m => Setter s s a b -> (a -> b) -> m ()
modifying l = modify . l
(%=) :: MonadState s m => Setter s s a b -> (a -> b) -> m ()
(%=) = modifying
{-# INLINE modifying #-}
{-# INLINE (%=) #-}

assign :: MonadState s m => Setter s s a b -> b -> m ()
assign l = modifying l . const
(.=) :: MonadState s m => Setter s s a b -> b -> m ()
(.=) = assign
{-# INLINE assign #-}
{-# INLINE (.=) #-}

_modify :: MonadState s m => (a -> c -> b) -> Setter s s a b -> c -> m ()
_modify op = (. flip op) . (%=)
-- _modify op l c = l %= (`op` c)
{-# INLINE _modify #-}

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
(?=) = _modify (const Just)
{-# INLINE (?=) #-}

(<~) :: MonadState s m => Setter s s a b -> m b -> m ()
(<~) = (=<<) . (.=)
{-# INLINE (<~) #-}

----------------------------------------------------------------

iover :: IxSetter i s t a b -> (i -> a -> b) -> s -> t
iover l = l . Indexed . uncurry
(%@~) :: IxSetter i s t a b -> (i -> a -> b) -> s -> t
(%@~) = iover
{-# INLINE iover #-}
{-# INLINE (%@~) #-}

iset :: IxSetter i s t a b -> (i -> b) -> s -> t
iset l = iover l . (const .)
(.@~) :: IxSetter i s t a b -> (i -> b) -> s -> t
(.@~) = iset
{-# INLINE iset #-}
{-# INLINE (.@~) #-}

imodifying :: MonadState s m => IxSetter i s s a b -> (i -> a -> b) -> m ()
imodifying l = modify . iover l
(%@=) :: MonadState s m => IxSetter i s s a b -> (i -> a -> b) -> m ()
(%@=) = imodifying
{-# INLINE imodifying #-}
{-# INLINE (%@=) #-}

iassign :: MonadState s m => IxSetter i s s a b -> (i -> b) -> m ()
iassign l = modify . iset l
(.@=) :: MonadState s m => IxSetter i s s a b -> (i -> b) -> m ()
(.@=) = iassign
{-# INLINE iassign #-}
{-# INLINE (.@=) #-}
