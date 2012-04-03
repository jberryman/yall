{-# LANGUAGE MultiParamTypeClasses, TypeOperators, TypeFamilies , FlexibleInstances #-}
module Data.Yall.Lens (
    {- | 
       TODO General explanatory notes about monadic lenses + motivation
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
       TODO Insert cool example of Monadic lenses:
       - polymorphic Failure
       - IO or []
    -}
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
    , setLiftM, setLiftW, setJoin

    -- * Composing Lenses
    {- |
    In addition to the usual 'Category' instance, we define instances for
    'Lens' for a number of category-level abstractions from the "categories"
    package. Here are the various combinators and pre-defined lenses from these
    classes, with types shown for a simplified @Lens@ type.
    -}
    -- TODO: PUT THESE AS COMMENTS TO INSTANCE DECLs? 
    -- |
    -- > import Control.Categorical.Bifunctor
    -- > first :: Lens a b -> Lens (a,x) (b,x)
    -- > second :: Lens a b -> Lens (x,a) (x,b)
    -- > bimap :: Lens a b -> Lens x y -> Lens (a,x) (b,y)
    -- . 
    -- > import Control.Categorical.Object
    -- > terminate :: Lens a ()
    -- .
    -- > import Control.Category.Associative
    -- > associate :: Lens ((a,b),c) (a,(b,c))
    -- > disassociate :: Lens (a,(b,c)) ((a,b),c)
    -- .
    -- > import Control.Category.Braided
    -- > braid :: Lens (a,b) (b,a)
    -- .
    -- > import Control.Category.Monoidal
    -- > idl :: Lens ((), a) a
    -- > idr :: Lens (a,()) a
    -- > coidl :: Lens a ((),a)
    -- > coidr :: Lens a (a,())
    -- .
    -- > import qualified Control.Categorical.Functor as C
    -- > C.fmap :: (Monad m)=> Lens m m a b -> (m a :-> m b)

    {- |
    In addition the following cominators and pre-defined lenses are provided.
    -}
    , fstL, sndL
    , eitherL, (|||)
    , factorL, distributeL
    , isoL
    ) where

-- TODO
--     - for GHC 7.2, EK has switched to using DefaultSignatures in e.g. Bifunctor. See:
--          https://github.com/ekmett/categories/commit/81857ce79d6c24be08d827f115109f1c6b8971ea
--       at some point we'll want to upgrade this, and make the appropriate changes
--     - 


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


{-
 TODO: explore monadic lenses (is the concept sound)
       initial release
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
    (bMa, b) <- f a
    return (runIdentity $ bMa $ g b)

-- | Create a monadic Lens from a setter and getter.
--
-- > lensMW g s = Lens $ \a-> liftM2 (,) (s a) (g a)
lensMW :: (Monad m)=> (a -> m b) -> (a -> m (b -> w a)) -> Lens w m a b
lensMW g s = Lens $ \a-> liftM2 (,) (s a) (g a)


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
            (bMa,b) <- g a
            (cMb,c) <- f b
            return (cMb >=> bMa, c)

-- BIFUNCTOR: --
instance (Monad w, Monad m)=> PFunctor (,) (Lens w m) (Lens w m) where
  --first :: Lens a b -> Lens (a,x) (b,x)
    first = firstDefault

instance (Monad w, Monad m)=> QFunctor (,) (Lens w m) (Lens w m) where
    second = secondDefault

instance (Monad w, Monad m)=> Bifunctor (,) (Lens w m) (Lens w m) (Lens w m) where
    bimap (Lens f) (Lens g) = 
        Lens $ \(a,c)-> do
            (bMa,b) <- f a                       
            (dMc,d) <- g c
            let setCont (b',d') = liftM2 (,) (bMa b') (dMc d')
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



-- | Convert an isomorphism to a 'Lens'
isoL :: (Monad m, Monad w)=> Iso m w a b -> Lens m w a b
isoL (Iso f g) = Lens $ fmap (liftM ((,) g)) f


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
