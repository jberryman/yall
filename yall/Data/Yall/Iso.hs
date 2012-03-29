{-# LANGUAGE TypeOperators #-}
module Data.Yall.Iso (
 {- |
  Iso is similar but more flexible than Lens in that they have no dependency on
  context. This flexibility affords a number of nice class instances that we
  don't get with Lens, so these can be quite useful in combination. See 'isoL'
  for converting to 'Lens'.
 -}
    Iso(..)
  , inverseI
  -- * Convenient Iso types
  -- ** Pure isomorphisms
  , (:<->)
  , iso
  , ($-)
  , (-$)
  -- *** Pre-defined isomorphisms
  {- | Note: while all of these are pure and could be expressed as '(:<->)', we
     define them polymorphically in @Monad@ for maximum flexibility in
     composing with other @Lens@ or @Iso@.

     Also note that for most of these @apply . unapply@ is not identity. A
     better name for this might be Bijection? I don't know. The functionality
     of these should be obvious.
  -}
  , wordsI, showI, linesI, curryI, enumI, integerI, rationalI, zipI

  -- ** Partial isomorphisms
  , (:<~>)
  )  where


-- TODO: 
-- lots of instances, other useful pre-defined Iso s
-- refer to: http://hackage.haskell.org/packages/archive/partial-isomorphisms/0.2/doc/html/Control-Isomorphism-Partial-Prim.html
-- ...for ideas on which of these abstractions apply

import Data.Functor.Identity

-- | An Isomorphism or one-to-one mapping between types. These are very similar
-- to a 'Lens', but are not dependent on context, making them more flexible. The
-- functions also alow a Monadic context, supporting partial isomorphisms, and 
-- other interesting functionality.
data Iso w m a b = Iso { apply   :: a -> m b
                       , unapply :: b -> w a }

inverseI :: (Monad m, Monad w)=> Iso w m a b -> Iso m w b a
inverseI (Iso f g) = Iso g f

-- | a partial Isomorphism
type a :<~> b = Iso Maybe Maybe a b

----- re-export in Data.Yall:

-- | pure 
type a :<-> b = Iso Identity Identity a b

iso :: (Monad m, Monad w)=> (a -> b) -> (b -> a) -> Iso w m a b
iso f g = Iso (fmap return f) (fmap return g)

($-) :: (a :<-> b) -> a -> b
i $- a = runIdentity $ apply i a

(-$) :: (a :<-> b) -> b -> a
i -$ b = runIdentity $ unapply i b


------------- 
wordsI :: (Monad m, Monad w)=> Iso w m String [String]
wordsI = iso words unwords

linesI :: (Monad m, Monad w)=> Iso w m String [String]
linesI = iso lines unlines

showI :: (Read s, Show s, Monad w, Monad m)=> Iso w m s String
showI = iso show read

-- TODO or leave this as the instance above???
curryI :: (Monad m, Monad w)=> Iso w m ((a,b) -> c) (a -> b -> c)
curryI = iso curry uncurry

enumI :: (Enum a, Monad m, Monad w)=> Iso w m Int a
enumI = iso toEnum fromEnum

integerI :: (Integral a, Monad m, Monad w)=> Iso w m a Integer
integerI = iso toInteger fromInteger

rationalI :: (Real a, Fractional a, Monad m, Monad w)=> Iso w m a Rational
rationalI = iso toRational fromRational

zipI :: (Monad m, Monad w)=> Iso w m ([a],[b]) [(a,b)]
zipI = iso (uncurry zip) unzip

