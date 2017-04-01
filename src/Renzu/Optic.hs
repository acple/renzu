module Renzu.Optic where

----------------------------------------------------------------

import Renzu.Index

----------------------------------------------------------------

type Optical p q s t a b = p a b -> q s t

type Optic p s t a b = Optical p p s t a b
type Optic' p s a = Optic p s s a a

type IxOptical p q i s t a b = Indexable i p q => Optical p q s t a b

type IxOptic p i s t a b = Optical (Indexed i p) p s t a b
type IxOptic' p i s a = IxOptic p i s s a a
