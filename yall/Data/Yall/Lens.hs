{-# LANGUAGE MultiParamTypeClasses, TypeOperators, TypeFamilies , FlexibleInstances #-}
module Data.Yall.Lens (
    {- | 
     The Lenses here are parameterized over two Monads (by convention @m@ and
     @w@), so that the \"extract\" and \"rebuild\" phases of a lens set operation
     each happen within their own environment. 
     
     Concretely, a lens like (':->') with both environments set to the trivial
     'Identity' Monad, gives us the usual pure lens, whereas something like
     (':~>'), where the @m@ environment is @Maybe@ gives one possibility for a
     partial lens. These would be suitable for multi-constructor data types.
     
     One might also like to use a lens as an interface to a type, capable of performing
     validation (beyond the capabilities of the typechecker). In that case the
     @w@ environment becomes useful, and you might have @:: Lens Maybe Identity
     PhoneNumber [Int]@.

     See \"Monadic API\" below for a concrete example.
     -}
      Lens(..)
    -- * Simple API
    -- ** Pure lenses
    , (:->)
    , lens, get, set, modify
    -- ** Partial lenses
    , (:~>)

    -- * Monadic API
    {- |
    In addition to defining lenses that can fail and perform validation, we
    have the ability to construct more abstract and expressive Lenses. Here is
    an example of a lens on the \"N-th\" element of a list, that returns its
    results in the [] monad:
    -}

-- |
-- > nth :: LensM [] [a] a
-- > nth = Lens $ foldr build []
-- >     where build n l = (return . (: map snd l), n) : map (prepend n) l
-- >           prepend = first . fmap . liftM . (:)
--
--  We can compose this with other lenses like the lens on the @snd@ of a
--  tuple, just as we would like:
--
-- >>> setM (sndL . nth) 0 [('a',1),('b',2),('c',3)]
-- [[('a',0),('b',2),('c',3)],[('a',1),('b',0),('c',3)],[('a',1),('b',2),('c',0)]]


    -- ** Lenses with monadic getters
    , LensM
    , lensM
    , getM, setM, modifyM 
    -- ** Monadic variants
    {- | The setter continuation is embedded in the getter\'s Monadic
       environment, so we offer several ways of combining different types of
       getter environments(@m@) and setter environments (@w@), for Lenses
       with complex effects.
    -}
    , lensMW
    , setW, modifyW
    , setLiftM, setLiftW, setJoin
    -- *** Monoid setters
    , setEmpty, setEmptyM, setEmptyW

    -- * Composing Lenses
    {- |
    In addition to the usual 'Category' instance, we define instances for
    'Lens' for a number of category-level abstractions from the "categories"
    package. Here are the various combinators and pre-defined lenses from these
    classes, with types shown for a simplified @Lens@ type.
    -}

-- |
-- > import Control.Categorical.Bifunctor
-- > first :: Lens a b -> Lens (a,x) (b,x)
-- > second :: Lens a b -> Lens (x,a) (x,b)
-- > bimap :: Lens a b -> Lens x y -> Lens (a,x) (b,y)
--  
-- > import Control.Categorical.Object
-- > terminate :: Lens a ()
-- 
-- > import Control.Category.Associative
-- > associate :: Lens ((a,b),c) (a,(b,c))
-- > disassociate :: Lens (a,(b,c)) ((a,b),c)
-- 
-- > import Control.Category.Braided
-- > braid :: Lens (a,b) (b,a)
-- 
-- > import Control.Category.Monoidal
-- > idl :: Lens ((), a) a
-- > idr :: Lens (a,()) a
-- > coidl :: Lens a ((),a)
-- > coidr :: Lens a (a,())
-- 
-- > import qualified Control.Categorical.Functor as C
-- > C.fmap :: (Monad m)=> Lens m m a b -> (m a :-> m b)

    {- |
    In addition the following combinators and pre-defined lenses are provided.
    -}
    , fstL, sndL
    , eitherL, (|||)
    , factorL, distributeL
    -- ** Lenses from Isomorphisms
    , isoL , residualL

    -- * Convenience operators
    {- | The little \"^\" hats are actually superscript \"L\"s (for "Lens") that have fallen over.
    -}
       
    , (^$), (^>>=)
    ) where

-- TODO
--     - for GHC 7.2, EK has switched to using DefaultSignatures in e.g. Bifunctor. See:
--          https://github.com/ekmett/categories/commit/81857ce79d6c24be08d827f115109f1c6b8971ea
--       at some point we'll want to upgrade this, and make the appropriate changes
--     - look at some of the looping combinators we use in pez and include here, e.g. untilL :: (a -> Bool) -> Lens a a -> Lens a a


import Data.Yall.Iso

import Prelude hiding (id,(.))
import Control.Category

-- from 'categories':
import Control.Categorical.Bifunctor
import qualified Control.Categorical.Functor as C
import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Monoidal
import Control.Category.Distributive
import Control.Categorical.Object

-- from 'semigroups':
--import Data.Semigroup

import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Monoid


{-
  TODO initial release
       template haskell library
-}

-- constrain these 'm's to Monad?
newtype Lens w m a b = Lens { runLens :: a -> m (b -> w a, b) }

-- | A lens in which the setter returns its result in the trivial identity 
-- monad. This is appropriate e.g. for traditional partial lenses, where there is
-- a potential that the lens could fail only on the /outer/ constructor.
type LensM = Lens Identity

-- | Create a monadic lens from a getter and setter
lensM :: (Monad m)=> (a -> m b) -> (a -> m (b -> a)) -> LensM m a b
lensM g = lensMW g . fmap (liftM $ fmap return)

-- | get, returning the result in a Monadic environment. This is appropriate
-- e.g. for traditional partial lenses on multi-constructor types. See also
-- 'setM'
getM :: (Monad m)=> Lens w m a b -> a -> m b
getM (Lens f) = liftM snd . f

-- | set, returning the result in the getter\'s Monadic environment, running 
-- the setter\'s trivial Identity monad. 
setM :: (Monad m)=> LensM m a b -> b -> a -> m a
setM (Lens f) b = liftM (runIdentity . ($ b) . fst) . f

-- | modify the inner value within the getter\'s Monadic environment 
modifyM :: (Monad m)=> LensM m a b -> (b -> b) -> a -> m a
modifyM (Lens f) g a = do
    (bWa, b) <- f a
    return (runIdentity $ bWa $ g b)

modifyW :: (Monad w)=> Lens w Identity a b -> (b -> b) -> a -> w a
modifyW (Lens f) g = uncurry ($) . second g . runIdentity . f

-- | Create a monadic Lens from a setter and getter.
--
-- > lensMW g s = Lens $ \a-> liftM2 (,) (s a) (g a)
lensMW :: (Monad m)=> (a -> m b) -> (a -> m (b -> w a)) -> Lens w m a b
lensMW g s = Lens $ \a-> liftM2 (,) (s a) (g a)

-- | set, with Monadic setter & pure getter
setW :: (Monad w)=> Lens w Identity a b -> b -> a -> w a
setW (Lens f) b = ($ b) . fst . runIdentity . f 

-- | set, 'lift'ing the outer (getter\'s) Monadic environment to the type of
-- the setter monad transformer.
setLiftM :: (Monad (t m), MonadTrans t, Monad m)=> Lens (t m) m a b -> b -> a -> t m a  
setLiftM (Lens f) b = join . liftM (($ b) . fst) . lift . f 

-- | set, like 'setLiftM' but we 'lift' the /inner/ setter\'s environment to
-- the outer getter monad transformer.
setLiftW :: (MonadTrans t, Monad (t w), Monad w)=> Lens w (t w) a b -> b -> a -> t w a
setLiftW (Lens f) b a = lift . ($ b) . fst =<< f a 

    

-- | set, combining the effects of the identical setter and getter Monads with
-- 'join'.
setJoin :: (Monad m)=> Lens m m a b -> b -> a -> m a
setJoin (Lens f) b a = f a >>= ($ b) . fst

instance (Monad w, Monad m)=> Category (Lens w m) where
    id = Lens $ return . (,) return
    (Lens f) . (Lens g) = 
        Lens $ \a-> do 
            (bWa,b) <- g a
            (cMb,c) <- f b
            return (cMb >=> bWa, c)

-- BIFUNCTOR: --
instance (Monad w, Monad m)=> PFunctor (,) (Lens w m) (Lens w m) where
  --first :: Lens a b -> Lens (a,x) (b,x)
    first = firstDefault

instance (Monad w, Monad m)=> QFunctor (,) (Lens w m) (Lens w m) where
    second = secondDefault

instance (Monad w, Monad m)=> Bifunctor (,) (Lens w m) (Lens w m) (Lens w m) where
    bimap (Lens f) (Lens g) = 
        Lens $ \(a,c)-> do
            (bWa,b) <- f a                       
            (dMc,d) <- g c
            let setCont (b',d') = liftM2 (,) (bWa b') (dMc d')
            return (setCont, (b,d))


-- This lets us turn an effect-ful lens into a pure lens on Monad-wrapped
-- values. 
-- TODO     - useful to be able to go the other direction? e.g. :: (m a :-> m b) -> Lens m m a b
instance (Monad m)=>C.Functor m (Lens m m) (Lens Identity Identity) where
    fmap (Lens f) = Lens $ \ma ->
        let t = ma >>= f
            mb2Ima = return . join . ap (liftM fst t)
         in return (mb2Ima, liftM snd t)



{-  
 -  What is this for, and why isn't (->) an instance?
 -  It is mentioned in the lib that (->) lacks an /initial/ object, but I
 -  would guess the missing HasTerminalObject is an oversight
-}
instance (Monad w, Monad m)=> HasTerminalObject (Lens w m) where
    type Terminal (Lens w m) = ()
  --terminate :: Lens a ()
    terminate = Lens $ \a-> return (\()-> return a, ()) 

instance (Monad w, Monad m)=> Associative (Lens w m) (,) where
  --associate :: Lens ((a,b),c) (a,(b,c))
    associate = Lens $ \((a,b),c)-> return (\(a',(b',c'))-> return ((a',b'),c'), (a,(b,c)))

instance (Monad w, Monad m)=> Disassociative (Lens w m) (,) where
  --disassociate :: Lens (a,(b,c)) ((a,b),c)
    disassociate =Lens $ \(a,(b,c))-> return (\((a',b'),c') -> return (a',(b',c')), ((a,b),c))

instance (Monad w, Monad m)=> Braided (Lens w m) (,) where
  --braid :: Lens (a,b) (b,a)
    braid = Lens $ \(a,b) -> return (\(b',a')-> return (a',b') , (b,a))

instance (Monad w, Monad m)=> Symmetric (Lens w m) (,)

-- (CO)MONOIDAL ----------------------------------------

type instance Id (Lens w m) (,) = ()

-- THIS ABSTRACTS THE dropl/r FUNCTIONS FROM GArrow:
instance (Monad w, Monad m)=> Monoidal (Lens w m) (,) where  
  --idl :: Lens ((), a)  a
    idl = Lens $ \((),a)-> return (\a'-> return ((),a'), a)
    idr = Lens $ \(a,())-> return (\a'-> return (a',()), a)

instance (Monad w, Monad m)=> Comonoidal (Lens w m) (,) where
  --coidl :: Lens a ((),a)
    coidl = Lens $ \a-> return (\((),a')-> return a', ((),a))
    coidr = Lens $ \a-> return (\(a',())-> return a', (a,()))

-- combinators from PreCartesian that preserve strict well-behavedness

fstL :: (Monad m, Monad w)=> Lens w m (a,b) a
fstL = Lens $ \(a,b)-> return (\a'-> return (a',b) , a)

sndL :: (Monad m, Monad w)=> Lens w m (a,b) b
sndL = Lens $ \(a,b)-> return (\b'-> return (a,b') , b)


-- borrowed from PreCoCartesian that preserve strict well-behavedness

-- | codiag from Cartesian
--
-- > eitherL = id ||| id
eitherL :: (Monad m, Monad w)=> Lens w m (Either a a) a
eitherL = id ||| id -- (codiag) DEFAULT


(|||) :: (Monad m, Monad w)=> Lens w m a c -> Lens w m b c -> Lens w m (Either a b) c
Lens f ||| Lens g = Lens $ either (handleL . f) (handleR . g) 
    where handleL = liftM $ first (liftM Left .)
          handleR = liftM $ first (liftM Right .)



-- borrowed from Control.Categorical.Distributive :

--RULES
--"factor . distribute" factor . distribute = id
--"distribute . factor" distribute . factor = id

factorL :: (Monad m, Monad w)=>Lens w m (Either (a,b) (a,c)) (a, Either b c)
factorL = Lens $ either (\(a,b)-> return (s ,(a, Left b))) (\(a,c)-> return (s, (a, Right c)))
    where s (a, ebc) = return $ either (Left . (,) a) (Right . (,) a) ebc
--factor = second inl ||| second inr -- DEFAULT


distributeL :: (Monad m, Monad w)=>Lens w m (a, Either b c) (Either (a,b) (a,c))
distributeL = Lens $ \(a,ebc)-> 
    return (return . factor, bimap ((,) a) ((,) a) ebc)



-- | Convert an isomorphism @i@ to a 'Lens'. When @apply i . unapply i =
-- unapply i . apply i = id@, the resulting lens will be well-behaved.
isoL :: (Monad m, Monad w)=> Iso m w a b -> Lens m w a b
isoL (Iso f g) = Lens $ fmap (liftM ((,) g)) f

-- | Convert an isomorphism between a value @a@ and a tuple of a value @b@ with
-- some \"residual\" value @r@. 
residualL :: (Monad m, Monad w)=> Iso m w a (b,r) -> Lens m w a b
residualL (Iso f g) = Lens $ \a-> do
    (b,r) <- f a
    return (\b'-> g (b',r), b)

-- -------------------
-- Simple API

-- | a simple lens, suitable for single-constructor types
type (:->) = LensM Identity

-- | Create a pure Lens from a getter and setter
--
-- > lens g = lensM (fmap return g) . fmap (fmap return)
lens :: (a -> b) -> (a -> b -> a) -> (a :-> b)
lens g = lensM (fmap return g) . fmap return

-- | Run the getter function of a pure lens
--
-- > get l = runIdentity . getM l
get :: Lens w Identity a b -> a -> b
get l = runIdentity . getM l

-- | Run the getter function of a pure lens
--
-- > set l b = runIdentity . setM l b
set :: (a :-> b) -> b -> a -> a
set l b = runIdentity . setM l b

modify :: (a :-> b) -> (b -> b) -> a -> a
modify l b = runIdentity . modifyM l b

-- | a lens that can fail in the Maybe monad on the outer type. Suitable for a
-- normal lens on a multi-constructor type. The more general 'setM', 'getM', etc.
-- can be used with this type.
type (:~>) = LensM Maybe


-- TODO: get rid of this and instead have a conversion to Iso function
--       rename other to-Iso conversion functions to make some kind of sense
--           lensI :: Monoid a=> Lens a b -> Iso a b
--           lensI l = Iso (getM l) (flip (setM l) mempty)
--       does this make an isomorphism of a well-behaved lens?
--           get l . flip (set l) mempty == id ?
--              get . set mempty OKAY
--           flip (set l) mempty . get l == id ?
--              set mempty $ get a  -- only 'id' when 'a' is 'mempty' as well.
--              LAW WE ASSUME HOLDS: set a $ get a
--       and is isoL . lensI == id?
--  IDEA:
--       Our usage of setEmpty is actually on setEmpty :: (Monoid a, Monoid b)=> (a :-> b) -> b -> a
--       Is it the case that a well-behaved lens with Monoid a, must have Monoid b or else  perhaps be partial?
--           set mempty $ get mempty == mempty
--           get $ set mempty x  == x
--               get $ set mempty (get mempty) == get mempty
--  IDEA2: 
--       shouldThisBeAStructure :: (Monoid a)=> Lens a b -> (b -> a, b)
--  CONSIDER...
--       what properties does an Iso have to have for isoL to produce a well-behaved lens?
--          put (Iso _ g) b _ = g b
--          get (Iso f _) a   = f a
--
--          get . put b  =  f (g b) -- f and g must be truly isomorphic for resulting lens to be well-behaved
--          put . get  =  g (f a)   
--          put b . put b  =   g b  -- doesn't matter, as put only depends on 'b'
--  OR... 
--       rename these below "fill" (since we're "setting" an empty)
--  OR... 
--       remove and just use Isos in our example.

-- | Set an inner value on an initially 'mempty' value.
--
-- > setZero l b = set l b mzero
setEmpty :: Monoid a => (a :-> b) -> b -> a
setEmpty l b = set l b mempty

-- | > setZeroM l b = setM l b mzero
setEmptyM :: (Monoid a, Monad m) => LensM m a b -> b -> m a
setEmptyM l b = setM l b mempty

-- | > setEmptyW l b = setW l b mempty
setEmptyW :: (Monoid a, Monad w) => Lens w Identity a b -> b -> w a
setEmptyW l b = setW l b mempty

--TODO: more set variations? getEmpty?


-- OPERATORS: ------------------------

-- | > (^$) = get
(^$) :: Lens w Identity a b -> a -> b 
(^$) = get

-- | > ma ^>>= l = ma >>= getM l
(^>>=) :: (Monad m)=> m a -> Lens w m a b -> m b
ma ^>>= l = ma >>= getM l
