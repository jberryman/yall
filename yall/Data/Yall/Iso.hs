{-# LANGUAGE DefaultSignatures, TypeOperators , MultiParamTypeClasses , FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving , TypeFamilies #-}
module Data.Yall.Iso (
 {- |
  Iso is similar but more flexible than Lens in that they have no dependency on
  context. This flexibility affords a number of nice class instances that we
  don't get with Lens, so these can be quite useful in combination. See 'isoL'
  for converting to 'Lens'.

  A less imprecise name for the code here might be @Bijection@ but no one wants
  to type that.
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

     Also note that for most of these @apply i . unapply i@ is not strictly
     @id@ for the entire input domain, e.g. @zipI@ obviously truncates lists of
     differing length, etc.  
  -}
  , wordsI, showI, linesI, curryI, enumI, integerI, rationalI, zipI
  , incrementI, incrementByI, consI
  , distributeI, factorI

  -- ** Partial isomorphisms
  , (:<~>)
  )  where



import Prelude hiding ((.),id)
import Control.Category
import Data.Functor.Identity
import Control.Monad

-- from 'categories':
import qualified Control.Categorical.Functor as C
import Control.Categorical.Bifunctor
import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Monoidal
import Control.Category.Distributive


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
    deriving (Category) 

-- ghetto deriving:
pureWrapped :: (Iso Identity Identity a1 b1 -> Iso Identity Identity a b)
                              -> IsoPure a1 b1
                              -> IsoPure a b
pureWrapped2 ::                (Iso Identity Identity a1 b1
                                -> Iso Identity Identity a2 b2
                                -> Iso Identity Identity a b)
                               -> IsoPure a1 b1
                               -> IsoPure a2 b2
                               -> IsoPure a b
pureWrapped2 f a b = IsoPure $ f (isoPure a) (isoPure b)
pureWrapped f = IsoPure . f . isoPure

instance PFunctor (,) IsoPure IsoPure where
    first = pureWrapped first
instance QFunctor (,) IsoPure IsoPure where
    second = pureWrapped second
instance Bifunctor (,) IsoPure IsoPure IsoPure where
    bimap = pureWrapped2 bimap
instance PFunctor Either IsoPure IsoPure where
    first = pureWrapped first
instance QFunctor Either IsoPure IsoPure where
    second = pureWrapped second
instance Bifunctor Either IsoPure IsoPure IsoPure where
    bimap = pureWrapped2 bimap

instance Associative IsoPure (,) where
    associate = IsoPure associate
    disassociate = IsoPure disassociate
instance Associative IsoPure Either where
    associate = IsoPure associate
    disassociate = IsoPure disassociate

instance Braided IsoPure (,) where
    braid = IsoPure braid
instance Braided IsoPure Either where
    braid = IsoPure braid
instance Symmetric IsoPure Either where
instance Symmetric IsoPure (,) where

instance Monoidal IsoPure (,) where
    type Id IsoPure (,) = ()
    idl = IsoPure idl
    idr = IsoPure idr
    coidl = IsoPure coidl
    coidr = IsoPure coidr


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
    fmap (IsoPure (Iso f g)) = 
        IsoPure $ iso (fmap $ fmap runIdentity f) (fmap $ fmap runIdentity g)

instance (Monad m)=> C.Functor m (Iso m m) (Iso Identity Identity) where
    fmap (Iso f g) = iso (>>= f) (>>= g)



-- Control.Categorical.Bifunctor
instance (Monad m, Monad w)=> PFunctor (,) (Iso w m) (Iso w m)  where
    first f = bimap f id
instance (Monad m, Monad w)=> QFunctor (,) (Iso w m) (Iso w m) where
    second = bimap id
instance (Monad m, Monad w)=> Bifunctor (,) (Iso w m) (Iso w m) (Iso w m) where
    bimap (Iso f g) (Iso f' g') = Iso (bimapM f f') (bimapM' g g')
        -- WHY DOES TypeFamilies CAUSE PROBLEMS WITH THIS?:
        where bimapM x = fmap extractJoinT . bimap x
              bimapM' x = fmap extractJoinT . bimap x

instance (Monad m, Monad w)=> PFunctor Either (Iso w m) (Iso w m) where
    first f = bimap f id
instance (Monad m, Monad w)=> QFunctor Either (Iso w m) (Iso w m) where
    second = bimap id
instance (Monad m, Monad w)=> Bifunctor Either (Iso w m) (Iso w m) (Iso w m) where
    bimap (Iso f g) (Iso f' g') = Iso (bimapM f f') (bimapM' g g')
        where bimapM x = fmap extractJoinE . bimap x
              bimapM' x = fmap extractJoinE . bimap x


-- Does this already exist in Categories? 
--  :: k (m a) (m b) -> m (k a b)
--   For k = Either / (,)
--       m = any Monad
extractJoinE :: (Monad m)=> Either (m a) (m b) -> m (Either a b)
extractJoinE = either (liftM Left) (liftM Right)
extractJoinT :: (Monad m)=> (m a, m b) -> m (a,b)
extractJoinT = uncurry $ liftM2 (,)

-- Control.Category.Associative
instance (Monad m, Monad w)=> Associative (Iso w m) (,) where
    associate = iso associate disassociate
    disassociate = iso disassociate associate

instance (Monad m, Monad w)=> Associative (Iso w m) Either where
    associate = iso associate disassociate
    disassociate = iso disassociate associate
    

-- Control.Category.Braided
instance (Monad m, Monad w)=> Braided (Iso w m) (,) where
    braid = iso braid braid

instance (Monad m, Monad w)=> Braided (Iso w m) Either where
    braid = iso braid braid

instance (Monad m, Monad w)=> Symmetric (Iso w m) (,) where
instance (Monad m, Monad w)=> Symmetric (Iso w m) Either where

distributeI :: (Monad m, Monad w)=> Iso w m (a, Either b c) (Either (a,b) (a,c))
distributeI = iso distribute factor

factorI :: (Monad m, Monad w)=> Iso w m (Either (a,b) (a,c)) (a, Either b c)
factorI = iso factor distribute

-- Control.Category.Monoidal
instance (Monad m, Monad w)=> Monoidal (Iso w m) (,) where
    type Id (Iso w m) (,) = ()
    idl = iso idl coidl 
    idr = iso idr coidr
    coidl =  iso coidl idl 
    coidr = iso coidr idr 


-- | See also an Iso wrapped in 'Dual'
inverseI :: (Monad m, Monad w)=> Iso w m a b -> Iso m w b a
inverseI (Iso f g) = Iso g f

-- | a partial Isomorphism
type a :<~> b = Iso Maybe Maybe a b

-- | pure Iso
type a :<-> b = Iso Identity Identity a b

iso :: (Monad m, Monad w)=> (a -> b) -> (b -> a) -> Iso w m a b
iso f g = Iso (fmap return f) (fmap return g)

-- | apply the forward function
-- 
-- > i $- a = runIdentity $ apply i a
($-) :: (a :<-> b) -> a -> b
i $- a = runIdentity $ apply i a

-- | apply the backward function
-- 
-- > i -$ b = runIdentity $ unapply i b
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
