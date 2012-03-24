{-# LANGUAGE MultiParamTypeClasses, TypeOperators, TypeFamilies #-}
module Data.Lens.Yall
    where

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

-- constrain these 'm's to Monad?
newtype Lens ms mg a b = Lens { runLens :: a -> mg (b -> ms a, b) }

get :: (Monad mg)=> Lens ms mg a b -> a -> mg b
get (Lens f) = liftM snd . f

set :: (MonadTrans t, Monad (t mg), Monad mg)=> Lens (t mg) mg a b -> b -> a -> t mg a  
set (Lens f) b = join . liftM (($ b) . fst) . lift . f 

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

-- EXPLORE MORE IF TIME:
-- get in terms of Functor
-- instance (Monad ms, Monad mg)=> C.Functor ((->) r) (Lens ms mg) (Lens ms mg) where
-- instance (Monad ms, Monad mg)=> C.Functor [] (Lens ms mg) (Lens ms mg) where
--     fmap (Lens f) = Lens $ \as -> do
--         bMabs <- mapM f as
--         return (error "violates put-get if we just do a zip" , map snd bMabs)
-- instance (Monad ms, Monad mg)=> C.Functor IO (Lens ms mg) (Lens ms mg) where
--     fmap (Lens f) = Lens $ \ioa-> do
--         (bMa,b) <- liftIO f ioa
--         return (??, return b)


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

(|||) :: (Monad mg, Monad ms)=> Lens ms mg a c -> Lens ms mg b c -> Lens ms mg (Either a b) c
Lens f ||| Lens g = Lens $ either (handleL . f) (handleR . g) 
    where handleL = liftM $ first ((liftM Left) .)
          handleR = liftM $ first ((liftM Right) .)



-- from Control.Categorical.Distributive :

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



-- TODO: explore monadic lenses (is the concept sound)
--       decide on naming
--       initial release
--       template haskell library



-- TODO: include or not?
unIso :: (a :-> b) -> (a -> b, b -> a -> a)
unIso l = (pureGet l, pureSet l)


-- -------------------
-- TODO: DECIDE ABOUT NAMING HERE AND CONSIDER MOVING TO SEPARATE MODULES:
-- perhaps we should make a setM function and not set/getMaybe

-- | a simple lens, suitable for single-constructor types
type (:->) = Lens (IdentityT Identity) Identity

-- | > pureGet l = runIdentity . runIdentity . get l
pureGet :: (a :-> b) -> a -> b
pureGet l = runIdentity . get l

-- | > pureSet l b = runIdentity . runIdentityT . set l b
pureSet :: (a :-> b) -> b -> a -> a
pureSet l b = runIdentity . runIdentityT . set l b

-- | a lens that can fail in the Maybe monad on the outer type, suitable for a
-- normal lens on a multi-constructor type
type (:~>) = Lens (IdentityT Maybe) Maybe

-- | > maybeGet = get
maybeGet :: (a :~> b) -> a -> Maybe b
maybeGet = get 

-- | > maybeSet l b = runIdentityT . set l b
maybeSet :: (a :~> b) -> b -> a -> Maybe a
maybeSet l b = runIdentityT . set l b



