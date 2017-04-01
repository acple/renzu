{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Renzu.Tuple where

----------------------------------------------------------------

import Data.Profunctor
import Renzu.Lens

----------------------------------------------------------------

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _1 :: Lens s t a b

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _2 :: Lens s t a b

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _3 :: Lens s t a b

class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _4 :: Lens s t a b

class Field5 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _5 :: Lens s t a b

class Field6 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _6 :: Lens s t a b

class Field7 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _7 :: Lens s t a b

class Field8 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _8 :: Lens s t a b

class Field9 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _9 :: Lens s t a b

----------------------------------------------------------------

instance Field1 (a, b) (a', b) a a' where
    _1 = first'

instance Field1 (a, b, c) (a', b, c) a a' where
    _1 = lens (\(a, _, _) -> a) (\(_, b, c) a' -> (a', b, c))

instance Field1 (a, b, c, d) (a', b, c, d) a a' where
    _1 = lens (\(a, _, _, _) -> a) (\(_, b, c, d) a' -> (a', b, c, d))

instance Field1 (a, b, c, d, e) (a', b, c, d, e) a a' where
    _1 = lens (\(a, _, _, _, _) -> a) (\(_, b, c, d, e) a' -> (a', b, c, d, e))

instance Field1 (a, b, c, d, e, f) (a', b, c, d, e, f) a a' where
    _1 = lens (\(a, _, _, _, _, _) -> a) (\(_, b, c, d, e, f) a' -> (a', b, c, d, e, f))

instance Field1 (a, b, c, d, e, f, g) (a', b, c, d, e, f, g) a a' where
    _1 = lens (\(a, _, _, _, _, _, _) -> a) (\(_, b, c, d, e, f, g) a' -> (a', b, c, d, e, f, g))

instance Field1 (a, b, c, d, e, f, g, h) (a', b, c, d, e, f, g, h) a a' where
    _1 = lens (\(a, _, _, _, _, _, _, _) -> a) (\(_, b, c, d, e, f, g, h) a' -> (a', b, c, d, e, f, g, h))

instance Field1 (a, b, c, d, e, f, g, h, i) (a', b, c, d, e, f, g, h, i) a a' where
    _1 = lens (\(a, _, _, _, _, _, _, _, _) -> a) (\(_, b, c, d, e, f, g, h, i) a' -> (a', b, c, d, e, f, g, h, i))

----------------------------------------------------------------

instance Field2 (a, b) (a, b') b b' where
    _2 = second'

instance Field2 (a, b, c) (a, b', c) b b' where
    _2 = lens (\(_, b, _) -> b) (\(a, _, c) b' -> (a, b', c))

instance Field2 (a, b, c, d) (a, b', c, d) b b' where
    _2 = lens (\(_, b, _, _) -> b) (\(a, _, c, d) b' -> (a, b', c, d))

instance Field2 (a, b, c, d, e) (a, b', c, d, e) b b' where
    _2 = lens (\(_, b, _, _, _) -> b) (\(a, _, c, d, e) b' -> (a, b', c, d, e))

instance Field2 (a, b, c, d, e, f) (a, b', c, d, e, f) b b' where
    _2 = lens (\(_, b, _, _, _, _) -> b) (\(a, _, c, d, e, f) b' -> (a, b', c, d, e, f))

instance Field2 (a, b, c, d, e, f, g) (a, b', c, d, e, f, g) b b' where
    _2 = lens (\(_, b, _, _, _, _, _) -> b) (\(a, _, c, d, e, f, g) b' -> (a, b', c, d, e, f, g))

instance Field2 (a, b, c, d, e, f, g, h) (a, b', c, d, e, f, g, h) b b' where
    _2 = lens (\(_, b, _, _, _, _, _, _) -> b) (\(a, _, c, d, e, f, g, h) b' -> (a, b', c, d, e, f, g, h))

instance Field2 (a, b, c, d, e, f, g, h, i) (a, b', c, d, e, f, g, h, i) b b' where
    _2 = lens (\(_, b, _, _, _, _, _, _, _) -> b) (\(a, _, c, d, e, f, g, h, i) b' -> (a, b', c, d, e, f, g, h, i))

----------------------------------------------------------------

instance Field3 (a, b, c) (a, b, c') c c' where
    _3 = lens (\(_, _, c) -> c) (\(a, b, _) c' -> (a, b, c'))

instance Field3 (a, b, c, d) (a, b, c', d) c c' where
    _3 = lens (\(_, _, c, _) -> c) (\(a, b, _, d) c' -> (a, b, c', d))

instance Field3 (a, b, c, d, e) (a, b, c', d, e) c c' where
    _3 = lens (\(_, _, c, _, _) -> c) (\(a, b, _, d, e) c' -> (a, b, c', d, e))

instance Field3 (a, b, c, d, e, f) (a, b, c', d, e, f) c c' where
    _3 = lens (\(_, _, c, _, _, _) -> c) (\(a, b, _, d, e, f) c' -> (a, b, c', d, e, f))

instance Field3 (a, b, c, d, e, f, g) (a, b, c', d, e, f, g) c c' where
    _3 = lens (\(_, _, c, _, _, _, _) -> c) (\(a, b, _, d, e, f, g) c' -> (a, b, c', d, e, f, g))

instance Field3 (a, b, c, d, e, f, g, h) (a, b, c', d, e, f, g, h) c c' where
    _3 = lens (\(_, _, c, _, _, _, _, _) -> c) (\(a, b, _, d, e, f, g, h) c' -> (a, b, c', d, e, f, g, h))

instance Field3 (a, b, c, d, e, f, g, h, i) (a, b, c', d, e, f, g, h, i) c c' where
    _3 = lens (\(_, _, c, _, _, _, _, _, _) -> c) (\(a, b, _, d, e, f, g, h, i) c' -> (a, b, c', d, e, f, g, h, i))

----------------------------------------------------------------

instance Field4 (a, b, c, d) (a, b, c, d') d d' where
    _4 = lens (\(_, _, _, d) -> d) (\(a, b, c, _) d' -> (a, b, c, d'))

instance Field4 (a, b, c, d, e) (a, b, c, d', e) d d' where
    _4 = lens (\(_, _, _, d, _) -> d) (\(a, b, c, _, e) d' -> (a, b, c, d', e))

instance Field4 (a, b, c, d, e, f) (a, b, c, d', e, f) d d' where
    _4 = lens (\(_, _, _, d, _, _) -> d) (\(a, b, c, _, e, f) d' -> (a, b, c, d', e, f))

instance Field4 (a, b, c, d, e, f, g) (a, b, c, d', e, f, g) d d' where
    _4 = lens (\(_, _, _, d, _, _, _) -> d) (\(a, b, c, _, e, f, g) d' -> (a, b, c, d', e, f, g))

instance Field4 (a, b, c, d, e, f, g, h) (a, b, c, d', e, f, g, h) d d' where
    _4 = lens (\(_, _, _, d, _, _, _, _) -> d) (\(a, b, c, _, e, f, g, h) d' -> (a, b, c, d', e, f, g, h))

instance Field4 (a, b, c, d, e, f, g, h, i) (a, b, c, d', e, f, g, h, i) d d' where
    _4 = lens (\(_, _, _, d, _, _, _, _, _) -> d) (\(a, b, c, _, e, f, g, h, i) d' -> (a, b, c, d', e, f, g, h, i))

----------------------------------------------------------------

instance Field5 (a, b, c, d, e) (a, b, c, d, e') e e' where
    _5 = lens (\(_, _, _, _, e) -> e) (\(a, b, c, d, _) e' -> (a, b, c, d, e'))

instance Field5 (a, b, c, d, e, f) (a, b, c, d, e', f) e e' where
    _5 = lens (\(_, _, _, _, e, _) -> e) (\(a, b, c, d, _, f) e' -> (a, b, c, d, e', f))

instance Field5 (a, b, c, d, e, f, g) (a, b, c, d, e', f, g) e e' where
    _5 = lens (\(_, _, _, _, e, _, _) -> e) (\(a, b, c, d, _, f, g) e' -> (a, b, c, d, e', f, g))

instance Field5 (a, b, c, d, e, f, g, h) (a, b, c, d, e', f, g, h) e e' where
    _5 = lens (\(_, _, _, _, e, _, _, _) -> e) (\(a, b, c, d, _, f, g, h) e' -> (a, b, c, d, e', f, g, h))

instance Field5 (a, b, c, d, e, f, g, h, i) (a, b, c, d, e', f, g, h, i) e e' where
    _5 = lens (\(_, _, _, _, e, _, _, _, _) -> e) (\(a, b, c, d, _, f, g, h, i) e' -> (a, b, c, d, e', f, g, h, i))

----------------------------------------------------------------

instance Field6 (a, b, c, d, e, f) (a, b, c, d, e, f') f f' where
    _6 = lens (\(_, _, _, _, _, f) -> f) (\(a, b, c, d, e, _) f' -> (a, b, c, d, e, f'))

instance Field6 (a, b, c, d, e, f, g) (a, b, c, d, e, f', g) f f' where
    _6 = lens (\(_, _, _, _, _, f, _) -> f) (\(a, b, c, d, e, _, g) f' -> (a, b, c, d, e, f', g))

instance Field6 (a, b, c, d, e, f, g, h) (a, b, c, d, e, f', g, h) f f' where
    _6 = lens (\(_, _, _, _, _, f, _, _) -> f) (\(a, b, c, d, e, _, g, h) f' -> (a, b, c, d, e, f', g, h))

instance Field6 (a, b, c, d, e, f, g, h, i) (a, b, c, d, e, f', g, h, i) f f' where
    _6 = lens (\(_, _, _, _, _, f, _, _, _) -> f) (\(a, b, c, d, e, _, g, h, i) f' -> (a, b, c, d, e, f', g, h, i))

----------------------------------------------------------------

instance Field7 (a, b, c, d, e, f, g) (a, b, c, d, e, f, g') g g' where
    _7 = lens (\(_, _, _, _, _, _, g) -> g) (\(a, b, c, d, e, f, _) g' -> (a, b, c, d, e, f, g'))

instance Field7 (a, b, c, d, e, f, g, h) (a, b, c, d, e, f, g', h) g g' where
    _7 = lens (\(_, _, _, _, _, _, g, _) -> g) (\(a, b, c, d, e, f, _, h) g' -> (a, b, c, d, e, f, g', h))

instance Field7 (a, b, c, d, e, f, g, h, i) (a, b, c, d, e, f, g', h, i) g g' where
    _7 = lens (\(_, _, _, _, _, _, g, _, _) -> g) (\(a, b, c, d, e, f, _, h, i) g' -> (a, b, c, d, e, f, g', h, i))

----------------------------------------------------------------

instance Field8 (a, b, c, d, e, f, g, h) (a, b, c, d, e, f, g, h') h h' where
    _8 = lens (\(_, _, _, _, _, _, _, h) -> h) (\(a, b, c, d, e, f, g, _) h' -> (a, b, c, d, e, f, g, h'))

instance Field8 (a, b, c, d, e, f, g, h, i) (a, b, c, d, e, f, g, h', i) h h' where
    _8 = lens (\(_, _, _, _, _, _, _, h, _) -> h) (\(a, b, c, d, e, f, g, _, i) h' -> (a, b, c, d, e, f, g, h', i))

----------------------------------------------------------------

instance Field9 (a, b, c, d, e, f, g, h, i) (a, b, c, d, e, f, g, h, i') i i' where
    _9 = lens (\(_, _, _, _, _, _, _, _, i) -> i) (\(a, b, c, d, e, f, g, h, _) i' -> (a, b, c, d, e, f, g, h, i'))
