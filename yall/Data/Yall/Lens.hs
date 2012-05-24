{-# LANGUAGE MultiParamTypeClasses, TypeOperators, TypeFamilies , FlexibleInstances, FlexibleContexts #-}
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


    , Lenses(..)

    , LensM
    , lensM
    -- ** Monadic variants
    {- | The setter continuation is embedded in the getter\'s Monadic
       environment, so we offer several ways of combining different types of
       getter environments (@m@) and setter environments (@w@), for Lenses
       with complex effects.

       Newtype wrappers around 'Lens' let us use the same 'Lenses' interface
       for getting and setting for these various monad-combining schemes.
    -}
    , lensMW
    , LensLift(..) , LensJoin(..) , LensW(..)

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

-- | A lens in which the setter returns its result in the trivial 'Identity'
-- monad. This is appropriate e.g. for traditional partial lenses on sum types,
-- where there is a potential that the lens could fail only on the /outer/
-- constructor.
type LensM = Lens Identity

-- | Create a monadic lens from a getter and setter
lensM :: (Monad m)=> (a -> m b) -> (a -> m (b -> a)) -> LensM m a b
lensM g = lensMW g . fmap (liftM $ fmap return)

-- | A class for our basic (monadic) lens operations. Minimal complete
-- definition is 'getM' and 'setM'
class (Monad m)=> Lenses l m where
    getM :: l m a b -> a -> m b
    setM :: l m a b -> a -> b -> m a
    modifyM :: l m a b -> (b -> b) -> a -> m a
    modifyM l f a = getM l a >>= setM l a . f

-- helpers:
getterM :: Monad m => Lens t m a r -> a -> m r
getterM (Lens f) = liftM snd . f
setterM :: Monad m => Lens t m a b -> a -> m (b -> t a)
setterM (Lens f) = liftM fst . f

instance (Monad m)=> Lenses (Lens Identity) m where
    getM = getterM
    -- is let-floating effective here? Can snd be GCed after 'setM l a'?
    setM l a = let mba = setterM l a
                in \b-> liftM (runIdentity . ($ b)) mba
    modifyM (Lens f) g a = do
        (bWa, b) <- f a
        return (runIdentity $ bWa $ g b)


-- | Create a monadic Lens from a setter and getter.
--
-- > lensMW g s = Lens $ \a-> liftM2 (,) (s a) (g a)
lensMW :: (Monad m)=> (a -> m b) -> (a -> m (b -> w a)) -> Lens w m a b
lensMW g s = Lens $ \a-> liftM2 (,) (s a) (g a)


------------------ MONADIC VARIANTS: ---------------------
-- TODO: derive all classes

-- TODO: make this parameterized by 't':
-- | lenses in which set/get should 'lift' the inner monad @w@ to @m@
newtype LensLift w m a b = LLift (Lens w m a b)

instance (MonadTrans t, Monad (t w), Monad w)=> Lenses (LensLift w) (t w) where
    getM (LLift l) = getterM l
    setM (LLift l) a = let mba = setterM l a
                        in \b-> lift . ($ b) =<< mba


-- | lenses in which @m@ == @w@ and we would like to 'join' the two in get/set
newtype LensJoin m a b = LJoin (Lens m m a b)

instance (Monad m)=> Lenses LensJoin m where
    getM (LJoin l) = getterM l
    setM (LJoin l) a = let mba = setterM l a
                        in \b-> mba >>= ($ b)

-- | lenses in which only the setter @w@ is monadic
newtype LensW w a b = LW (Lens w Identity a b)

instance (Monad w)=> Lenses LensW w where
    getM (LW l) = return . get l
    setM (LW (Lens f)) a = let bwa = fst $ runIdentity $ f a
                            in bwa
    modifyM (LW (Lens f)) g = uncurry ($) . second g . runIdentity . f


-- -- | set, 'lift'ing the outer (getter\'s) Monadic environment to the type of
-- -- the setter monad transformer.
-- setLiftM :: (Monad (t m), MonadTrans t, Monad m)=> Lens (t m) m a b -> b -> a -> t m a  
-- setLiftM (Lens f) b = join . liftM (($ b) . fst) . lift . f 

    


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

-- | Convert to a Lens an isomorphism between a value @a@ and a tuple of a
-- value @b@ with some \"residual\" value @r@. 
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
get l = runIdentity . liftM snd . (runLens l)

-- | Run the getter function of a pure lens
--
-- > set l b = runIdentity . setM l a
set :: (a :-> b) -> a -> b -> a
set l a = runIdentity . setM l a

modify :: (a :-> b) -> (b -> b) -> a -> a
modify l f = runIdentity . modifyM l f

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



-- OPERATORS: ------------------------

-- | > (^$) = get
(^$) :: Lens w Identity a b -> a -> b 
(^$) = get

-- | > ma ^>>= l = ma >>= getM l
(^>>=) :: (Lenses l m)=> m a -> l m a b -> m b
ma ^>>= l = ma >>= getM l
