{-# LANGUAGE MultiParamTypeClasses, TypeOperators, TypeFamilies #-}
module Data.Lens.Yall
    where

import Prelude hiding (id,(.))
import Control.Category

-- from 'categories':
import Control.Categorical.Bifunctor
import Control.Category.Associative
import Control.Category.Braided
--import qualified Control.Category.Cartesian as Cart

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

{-  
 -  What is this for? 
 -  and why isn't (->) an instance?
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
    
{- 
instance (Monad ms, Monad mg)=> Cart.PreCartesian (Lens ms mg) where
    type Cart.Product (Lens ms mg) = (,)
  --fst :: Lens (a,b) a
    fst = Lens $ \(a,b)-> return (\a'-> return (a',b) , a)

  --snd :: Lens (a,b) b
    snd = Lens $ \(a,b)-> return (\b'-> return (a,b') , b)

    -- The following two lenses are not traditionally "well-behaved" w/r/t the
    -- so-called "Lens Laws", violating "put-get". 
  --(&&&) :: Lens a b -> Lens a c -> Lens a (b,c)
  --f &&& g = bimap f g . diag -- DEFAULT
    (Lens f) &&& (Lens g) = Lens $ \a-> do
            (bMa,b) <- f a
            (cMa,c) <- g a
            -- run set on b', then set on c', sequencing effects
            let setbc (b',c') = bMa b' >> cMa c'
            return (setbc, (b, c))

  --diag :: Lens a (a,a)
  --diag = id &&& id --DEFAULT
  --diag = Lens $ \a -> return (\(_,a')-> return a', (a,a))


instance PreCoCartesian Lens where
    type Sum Lens = Either
    inl :: Lens a (Either a b)
    inr :: Lens b (Either a b)
    codiag :: Lens (Either a a) a
    (|||) :: Lens a c -> Lens b c -> Lens (Either a b) c

-- --------------------

-- PROMISING:

{- THESE SHOULD HOLD:
 -  first idr = second idl . associate 
 - second idl = first idr . associate
 -}
type instance Id Lens (,) = ()

-- THIS ABSTRACTS THE dropl/r FUNCTIONS FROM GArrow:
instance Monoidal Lens (,) where  
    idl :: Lens ((), a)  a
    idr :: ...

{- THESE LAWS SHOULD HOLD:
 idr . coidr = id 
 idl . coidl = id 
 coidl . idl = id 
 coidr . idr = id
 -}
instance Comonoidal Lens (,) where
    coidl :: Lens a ((),a)
    coidr :: ...

-- ALSO CONSIDER A Monoid INSTANCE FOR Lens

-- --------------------

-- THIS ACTUALLY LOOKS PROMISING TOO:
instance Distributive Lens where
    distribute :: Lens (a, Either b c) (Either (a,b) (a,c))


-- --------------------
-- DEPENDS ON MONOIDAL, PLUS VERY UNLIKELY:
instance CCC Lens where
    type Exp Lens = ??
    apply :: Lens (?? a b, a) b
    curry :: Lens (a,b) c -> Lens a (?? b c)
    uncurry :: ...


-}

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
