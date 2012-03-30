{-# LANGUAGE MultiParamTypeClasses, TypeOperators, TypeFamilies #-}
module Data.Yall.Lens (
    {- | 
       TODO General explanatory notes
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
    , LensM
    , lensM
    , getM, setM, modifyM 
    -- ** Monadic set / create / modify variants
    {- | The setter continuation is embedded in the getter\'s Monadic
       environment, so we offer several ways of combining different types of
       getter environments(@m@) and setter environments (@w@), for Lenses
       with complex effects.
    -}
    , lensMW
    , setLiftM, setLiftW, setJoin

    ) where

import Data.Yall.Iso

import Prelude hiding (id,(.))
import Control.Category

-- from 'categories':
import Control.Categorical.Bifunctor
import qualified Control.Categorical.Functor as C
import Control.Category.Associative
import Control.Category.Braided
--import qualified Control.Category.Cartesian as Cart
import Control.Category.Monoidal
import Control.Category.Distributive

-- from 'semigroups':
--import Data.Semigroup

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Data.Functor.Identity


{-
 TODO: explore monadic lenses (is the concept sound)
            alter set monad to w, get monad to m
       initial release
       template haskell library
-}

-- constrain these 'm's to Monad?
newtype Lens ms mg a b = Lens { runLens :: a -> mg (b -> ms a, b) }

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
getM :: (Monad mg)=> Lens ms mg a b -> a -> mg b
getM (Lens f) = liftM snd . f

-- | set, returning the result in the getter\'s Monadic environment, running 
-- the setter\'s trivial Identity monad. 
setM :: (Monad mg)=> LensM mg a b -> b -> a -> mg a
setM (Lens f) b = liftM (runIdentity . ($ b) . fst) . f

-- | modify the inner value within the getter\'s Monadic environment 
modifyM :: (Monad mg)=> LensM mg a b -> (b -> b) -> a -> mg a
modifyM = undefined

-- | Create a monadic Lens from a setter and getter.
--
-- > lensMW g s = Lens $ \a-> liftM2 (,) (s a) (g a)
lensMW :: (Monad m)=> (a -> m b) -> (a -> m (b -> w a)) -> Lens w m a b
lensMW g s = Lens $ \a-> liftM2 (,) (s a) (g a)


-- | set, 'lift'ing the outer (getter\'s) Monadic environment to the type of
-- the setter monad transformer.
setLiftM :: (Monad (t mg), MonadTrans t, Monad mg)=> Lens (t mg) mg a b -> b -> a -> t mg a  
setLiftM (Lens f) b = join . liftM (($ b) . fst) . lift . f 

-- | set, like 'setLiftM' but we 'lift' the /inner/ setter\'s environment to
-- the outer getter monad transformer.
setLiftW :: (MonadTrans t, Monad (t ms), Monad ms)=> Lens ms (t ms) a b -> b -> a -> t ms a
setLiftW (Lens f) b = undefined

-- | set, combining the effects of the identical setter and getter Monads with
-- 'join'.
setJoin :: (Monad m)=> Lens m m a b -> b -> a -> m a
setJoin = undefined

instance (Monad ms, Monad mg)=> Category (Lens ms mg) where
    id = Lens $ return . (,) return
    (Lens f) . (Lens g) = 
        Lens $ \a-> do 
            (bMa,b) <- g a
            (cMb,c) <- f b
            return (cMb >=> bMa, c)

-- BIFUNCTOR: --
instance (Monad ms, Monad mg)=> PFunctor (,) (Lens ms mg) (Lens ms mg) where
  --first :: Lens a b -> Lens (a,x) (b,x)
    first = firstDefault

instance (Monad ms, Monad mg)=> QFunctor (,) (Lens ms mg) (Lens ms mg) where
    second = secondDefault

instance (Monad ms, Monad mg)=> Bifunctor (,) (Lens ms mg) (Lens ms mg) (Lens ms mg) where
    bimap (Lens f) (Lens g) = 
        Lens $ \(a,c)-> do
            (bMa,b) <- f a                       
            (dMc,d) <- g c
            let setCont (b',d') = liftM2 (,) (bMa b') (dMc d')
            return (setCont, (b,d))

instance (Monad ms, Monad mg)=> C.Functor ((,) x) (Lens ms mg) (Lens ms mg) where
    fmap = second

{-
TODO: EXPLORE THIS
instance (Monad ms, Monad mg)=> C.Functor ((->) r) (Lens ms mg) (Lens ms mg) where
instance (Monad ms, Monad mg)=> C.Functor IO (Lens ms mg) (Lens ms mg) where
    fmap (Lens f) = Lens $ \ioa-> do
        (bMa,b) <- liftIO f ioa
        return (??, return b)

-- It seems sum and recursive types will have issues with Functor, which we
-- wouldn't have for a type like Iso. Perhaps we should include such a type
-- instead of the straight iso function in data-lens, e.g.:
--     data Iso mg ms a b = Iso (a -> mg b) (b -> ms a)
--     isoL :: Iso a b -> (a :-> b)         -- this NEEDS to be polymorphic in monad, for composability
--     instance C.Functor (Either x) Iso Iso
--     instance C.Functor [] Iso Iso    ... etc.
--
-- Also take a look at notes. Many of those instances could potentially work
-- for an Iso type.

instance (Monad ms, Monad mg)=> C.Functor [] (Lens ms mg) (Lens ms mg) where
    fmap (Lens f) = Lens $ \as -> do
        bMabs <- mapM f as
        return (error "violates put-get if we just do a zip" , map snd bMabs)
instance (Monad ms, Monad mg)=> C.Functor (Either x) (Lens ms mg) (Lens ms mg) where
-}


{-  
 -  What is this for, and why isn't (->) an instance?
 -  It is mentioned in the lib that (->) lacks an /initial/ object, but I
 -  would guess the missing HasTerminalObject is an oversight
 -
instance HasTerminalObject Lens where
    type Terminal Lens = ()
  --terminate :: Lens a ()
    terminate = Lens $ \a-> (\()-> a, ())  -- this DOES obey the lens laws
-}

instance (Monad ms, Monad mg)=> Associative (Lens ms mg) (,) where
  --associate :: Lens ((a,b),c) (a,(b,c))
    associate = Lens $ \((a,b),c)-> return (\(a',(b',c'))-> return ((a',b'),c'), (a,(b,c)))

instance (Monad ms, Monad mg)=> Disassociative (Lens ms mg) (,) where
  --disassociate :: Lens (a,(b,c)) ((a,b),c)
    disassociate =Lens $ \(a,(b,c))-> return (\((a',b'),c') -> return (a',(b',c')), ((a,b),c))

instance (Monad ms, Monad mg)=> Braided (Lens ms mg) (,) where
  --braid :: Lens (a,b) (b,a)
    braid = Lens $ \(a,b) -> return (\(b',a')-> return (a',b') , (b,a))

instance (Monad ms, Monad mg)=> Symmetric (Lens ms mg) (,)

-- (CO)MONOIDAL ----------------------------------------

type instance Id (Lens ms mg) (,) = ()

-- THIS ABSTRACTS THE dropl/r FUNCTIONS FROM GArrow:
instance (Monad ms, Monad mg)=> Monoidal (Lens ms mg) (,) where  
  --idl :: Lens ((), a)  a
    idl = Lens $ \((),a)-> return (\a'-> return ((),a'), a)
    idr = Lens $ \(a,())-> return (\a'-> return (a',()), a)

instance (Monad ms, Monad mg)=> Comonoidal (Lens ms mg) (,) where
  --coidl :: Lens a ((),a)
    coidl = Lens $ \a-> return (\((),a')-> return a', ((),a))
    coidr = Lens $ \a-> return (\(a',())-> return a', (a,()))

-- combinators from PreCartesian that preserve strict well-behavedness

fstL :: (Monad mg, Monad ms)=> Lens ms mg (a,b) a
fstL = Lens $ \(a,b)-> return (\a'-> return (a',b) , a)

sndL :: (Monad mg, Monad ms)=> Lens ms mg (a,b) b
sndL = Lens $ \(a,b)-> return (\b'-> return (a,b') , b)


-- combinators from PreCoCartesian that preserve strict well-behavedness

-- | 
--
-- > eitherL = id ||| id
eitherL :: (Monad mg, Monad ms)=> Lens ms mg (Either a a) a
eitherL = id ||| id -- (codiag) DEFAULT


-- TODO: Iso will be a proper PreCrtesian, so maybe we don't want to steal this name:
(|||) :: (Monad mg, Monad ms)=> Lens ms mg a c -> Lens ms mg b c -> Lens ms mg (Either a b) c
Lens f ||| Lens g = Lens $ either (handleL . f) (handleR . g) 
    where handleL = liftM $ first (liftM Left .)
          handleR = liftM $ first (liftM Right .)



-- borrowed from Control.Categorical.Distributive :

--RULES
--"factor . distribute" factor . distribute = id
--"distribute . factor" distribute . factor = id

factorL :: (Monad mg, Monad ms)=>Lens ms mg (Either (a,b) (a,c)) (a, Either b c)
factorL = Lens $ either (\(a,b)-> return (s ,(a, Left b))) (\(a,c)-> return (s, (a, Right c)))
    where s (a, ebc) = return $ either (Left . (,) a) (Right . (,) a) ebc
--factor = second inl ||| second inr -- DEFAULT


distributeL :: (Monad mg, Monad ms)=>Lens ms mg (a, Either b c) (Either (a,b) (a,c))
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
get :: (a :-> b) -> a -> b
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
