{-# LANGUAGE TypeOperators , MultiParamTypeClasses , FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
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
  , ($-), (-$)
  -- *** Wrapped pure Iso
  , IsoPure(..), ifmap, fromPure
  -- *** Pre-defined isomorphisms
  {- | Note: while all of these are pure and could be expressed as '(:<->)', we
     define them polymorphically in @Monad@ for maximum flexibility in
     composing with other @Lens@ or @Iso@.

     Also note that for most of these @apply . unapply@ is not identity. A
     better name for this might be Bijection? I don't know. The functionality
     of these should be obvious.
  -}
  , wordsI, showI, linesI, curryI, enumI, integerI, rationalI, zipI
  , incrementI, incrementByI, consI

  -- ** Partial isomorphisms
  , (:<~>)
  )  where


-- TODO: 
-- lots of instances, other useful pre-defined Iso s
-- refer to: http://hackage.haskell.org/packages/archive/partial-isomorphisms/0.2/doc/html/Control-Isomorphism-Partial-Prim.html
-- ...for ideas on which of these abstractions apply


import Prelude hiding ((.),id)
import Control.Category
import Data.Functor.Identity
import Control.Monad
--import qualified Data.Traversable as T

-- from 'categories':
import qualified Control.Categorical.Functor as C
import Control.Categorical.Bifunctor

-- | An Isomorphism or one-to-one mapping between types. These are very similar
-- to a 'Lens', but are not dependent on context, making them more flexible. The
-- functions also alow a Monadic context, supporting partial isomorphisms, and 
-- other interesting functionality.
data Iso w m a b = Iso { apply   :: a -> m b
                       , unapply :: b -> w a }

instance (Monad m, Monad w)=> Category (Iso w m) where
    id = iso id id
    g . f = Iso (apply f >=> apply g) (unapply g >=> unapply f)

-- | A wrapper for a more @(->)@-like Functor instances
newtype IsoPure a b = IsoPure { isoPure :: Iso Identity Identity a b }
    deriving (Category) -- ...

-- | A more categorical 'fmap', with wrapping / unwrapping for convenience. See
-- also the 'C.Functor' instances for 'Iso'.
--
-- > ifmap = fromPure . C.fmap . IsoPure
ifmap :: (Monad w, Monad m, C.Functor f IsoPure IsoPure)=> Iso Identity Identity a b -> Iso w m (f a) (f b)
ifmap = fromPure . C.fmap . IsoPure

-- | Unwrap and make polymorphic an 'IsoPure'
fromPure :: (Monad w, Monad m)=> IsoPure a b -> Iso w m a b
fromPure (IsoPure (Iso f g)) = iso (fmap runIdentity f) (fmap runIdentity g)


-- Control.Categorical.Functor
instance (Functor f)=> C.Functor f IsoPure IsoPure where
    fmap (IsoPure (Iso f g)) = IsoPure $ iso (ifmap' f) (ifmap' g)
        where ifmap' = fmap . fmap runIdentity

instance (Monad m)=> C.Functor m (Iso m m) (Iso Identity Identity) where
    fmap (Iso f g) = iso (>>= f) (>>= g)




-- Control.Categorical.Bifunctor
instance (Monad m, Monad w)=> PFunctor (,) (Iso w m) (Iso w m) where
    first = firstDefault
instance (Monad m, Monad w)=> QFunctor (,) (Iso w m) (Iso w m) where
    second = secondDefault

instance (Monad m, Monad w)=> Bifunctor (,) (Iso w m) (Iso w m) (Iso w m) where
    bimap (Iso f g) (Iso f' g') = Iso (bimapM f f') (bimapM g g')
        where bimapM x = fmap (uncurry(liftM2 (,))) . bimap x

instance (Monad m, Monad w)=> PFunctor Either (Iso w m) (Iso w m) where
    first = firstDefault
instance (Monad m, Monad w)=> QFunctor Either (Iso w m) (Iso w m) where
    second = secondDefault

instance (Monad m, Monad w)=> Bifunctor Either (Iso w m) (Iso w m) (Iso w m) where
    bimap (Iso f g) (Iso f' g') = Iso (bimapM f f') (bimapM g g')
        where bimapM x = fmap (either (liftM Left) (liftM Right)) . bimap x

-- Does this already exist in Categories? 
--   :: k (m a) (m b) -> m (k a b)
--   For k = Either / (,)
--       m = any Monad

{- WORKS
instance (Monad m, Monad w)=> PFunctor (,) (Iso w m) (Iso w m) where
    first = firstDefault
instance (Monad m, Monad w)=> QFunctor (,) (Iso w m) (Iso w m) where
    second = secondDefault
instance (Monad m, Monad w)=> Bifunctor (,) (Iso w m) (Iso w m) (Iso w m) where
    bimap (Iso f g) (Iso f' g') = Iso (bimapM f f') (bimapM g g')
        where bimapM x = fmap (uncurry(liftM2 (,))) . bimap x
-}
{-
instance (Monad m, Monad w)=> PFunctor Either (Iso w m) (Iso w m) where
    first = firstDefault
instance (Monad m, Monad w)=> QFunctor Either (Iso w m) (Iso w m) where
    second = secondDefault
instance (Monad m, Monad w)=> Bifunctor Either (Iso w m) (Iso w m) (Iso w m) where
    bimap (Iso f g) (Iso f' g') = Iso (bimap f f') (bimap g g')
-}


{-
-- Control.Category.Associative
instance (Monad m, Monad w)=> Associative (Iso w m) (,) where
instance (Monad m, Monad w)=> Associative (Iso w m) Either where

instance (Monad m, Monad w)=> Disassociative (Iso w m) (,) where
instance (Monad m, Monad w)=> Disassociative (Iso w m) Either where

-- Control.Category.Braided
instance (Monad m, Monad w)=> Braided (Iso w m) (,) where
    braid = 
instance (Monad m, Monad w)=> Braided (Iso w m) Either where
    braid = 

instance (Monad m, Monad w)=> Symmetric (Iso w m) (,) where
instance (Monad m, Monad w)=> Symmetric (Iso w m) Either where

distributeI :: Iso (a, Either b c) (Either (a,b) (a,c))

factorI :: (Either (a,b) (a,c)) (a, Either b c)

-- Control.Category.Monoidal
type instance Id (Iso w m) (,) = ()

instance (Monad m, Monad w)=> Monoidal (Lens w m) (,) where
    idl =
    idr =
instance (Monad m, Monad w)=> Comonoidal (Lens w m) (,) where
    coidl = 
    coidr =
-}




-- | See also an Iso wrapped in 'Dual'
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

incrementI :: (Monad m, Monad w, Num a)=> Iso w m a a
incrementI = incrementByI 1

incrementByI :: (Monad m, Monad w, Num a)=> a -> Iso w m a a
incrementByI n = iso (+n) (subtract n)

-- | Calls 'fail' on the empty list.
consI :: (Monad m, Monad w)=> Iso w m (a,[a]) [a]
consI = Iso (\(a,as)-> return (a:as)) unconsI
    where unconsI [] = fail "empty list"
          unconsI (a:as) = return (a,as)
