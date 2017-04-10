{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Renzu.Profunctor.Wrapped where

----------------------------------------------------------------

import Renzu.Profunctor.Class

----------------------------------------------------------------

newtype Wrapped p a b = Wrap { unWrap :: p a b }
    deriving (Profunctor, Strong, Choice)
