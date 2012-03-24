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

-- TODO: change name here:
-- | 
-- > codiag = id ||| id
codiag :: (Monad mg, Monad ms)=> Lens ms mg (Either a a) a
codiag = id ||| id -- DEFAULT
--codiag = Lens $ either (l Left) (l Right) where
--    l lr a = return (return . lr, a)

(|||) :: (Monad mg, Monad ms)=> Lens ms mg a c -> Lens ms mg b c -> Lens ms mg (Either a b) c
Lens f ||| Lens g = Lens $ either (handleL . f) (handleR . g) 
    where handleL = liftM $ first ((liftM Left) .)
          handleR = liftM $ first ((liftM Right) .)

-- TODO: commit
--       delete or stash notes
--       explore monadic lenses (is the concept sound)
--       decide on naming
--       initial release
--       template haskell library



{-
 - ----------------------------------
 - ATTEMPTED INSTANCES / EXPERIMENTS
 - ----------------------------------
  
-- ALSO what about a combinator that is passed an (b -> a) for cases of sum
--     types (see C.Functor attempt). This would be a kind of default setter.
--     If we do this, create a wrapper type and define instances for sums.




   C.FUNCTOR (Maybe/Either)
--
-- This doesn't work for similar reasons to some of these other instances I've
-- played with: see attempt at Bifunctor for sum type:

instance C.Functor Maybe Lens Lens where
    fmap (Lens f) = Lens l
        where l (Just a) = let (ba,b) = f a
                            in (fmap ba, Just b)
              -- this violates put-get:
              l Nothing = (const Nothing, Nothing)



   BIFUNCTOR (Either):
--
-- This has complications on the 'put' side, where a constructor mismatch
-- returns the original structure unchanged:
--    put l (Left 1) (Right 'a') -- ???
-- this, once again, violates put-get.

instance (Monad ms, Monad mg)=> Bifunctor Either (Lens ms mg) (Lens ms mg) (Lens ms mg) where
    --bimap :: Lens a b -> Lens c d -> Lens (Either a c) (Either b d)
  
  
  
   SEMIGROUP:
  
-- NOTE: This violates both get-put and put-get
-- Use case: 
--     combine two lenses fetching Int leafs of a tree:
--        Lens Tree Int <> Lens Tree Int
--     get: 
--          takes the sum of the leaves
--     put: 
--          creates two subtrees: 
--              tree with leaf1 = x
--              tree with leaf2 = x
--          combines the two trees under a new root
--
-- I'm not sure that would be useful.

instance (Semigroup b, Semigroup a)=> Semigroup (Lens ms mg a b) where
    (Lens f) <> (Lens g) = Lens $ \a-> do
        (bMa,b) <- f a
     -- simplified example, without monads:
    (Lens f) <> (Lens g) = Lens $ \a-> 
        (ba1, b1) = f a
        (ba2, b2) = g a
        in (\b'-> ba1 b' <> ba2 b', b1 <> b2)


   PRECARTESIAN:

   TODO:   - inspect rules and laws
           - flush out notion of "sequencing" in PreCartesian

 {- RULES
"fst . diag"      fst . diag = id  CHECK
"snd . diag"    snd . diag = id  CHECK
"fst . f &&& g" forall f g. fst . (f &&& g) = f  
"snd . f &&& g" forall f g. snd . (f &&& g) = g
 -}
 
-- TODO: what about instead of (,) here we use something named something
-- explicity like "Sequence", or at least use a type synonym if possible:
-- then we get functions like 
--     put :: Lens a (Sequence b c) -> Sequence b c -> a -> m a
-- perhaps we can have a nice type operator like
--     (:>>)
-- Can we justify the behavior of (&&&)? 
--
-- NOTE: well-behaved methods here are: fst/snd

instance (Monad ms, Monad mg)=> Cart.PreCartesian (Lens ms mg) where
    type Cart.Product (Lens ms mg) = (,)
  --fst :: Lens (a,b) a
    fst = Lens $ \(a,b)-> return (\a'-> return (a',b) , a)

  --snd :: Lens (a,b) b
    snd = Lens $ \(a,b)-> return (\b'-> return (a,b') , b)

    -- The following two lenses are not traditionally "well-behaved" w/r/t the
    -- so-called "Lens Laws", violating "put-get". However the Lenses here
    -- are in essence a composition of a put-put and get-get. This seems 
    -- perfectly reasonable to me: we allow lens /sequencing/ in addition to
    -- the lens /chaining/ offered by Category.
    --
    -- Actually: (&&&) doesn't behave as we would like. Maybe we have to
    -- /reverse/ the order on put? would that allow the kind of chaining we're
    -- looking for?

  --(&&&) :: Lens a b -> Lens a c -> Lens a (b,c)
    --f &&& g = bimap f g . diag -- DEFAULT (EQUIVALENT? CHECK)
    (Lens f) &&& (Lens g) = Lens $ \a-> do
            (bMa,b) <- f a
            (cMa,c) <- g a
            -- run set on b', then set on c', sequencing effects
            let setbc (b',c') = bMa b' >> cMa c'
            return (setbc, (b, c))
    -- OR... this violates 3rd/4th laws above. Still it's unfortunate we can't
    -- seem to get two sequenced puts in a row with the current lens formulation.
    (Lens f) &&& (Lens g) = Lens $ \a-> do
            (bMa,b) <- f a
            (cMa,c) <- g a  -- should the gets be chained in a similar way?
            let setbc (b',c') = bMa b' >>= g >>= \(cMa',_)-> cMa c'  -- i.e. set b, open, set c
            return (setbc, (b,c))
    -- OR... 
    (Lens f) &&& (Lens g) = Lens $ \a-> do 
            (bMa,b) <- f a
            (_,  c) <- g a
            return(\(b',c')->bMa b'

  --diag :: Lens a (a,a)
    --diag = id &&& id --DEFAULT
    --diag = Lens $ \a -> return (\(_,a')-> return a', (a,a))

-- --------------------

-- Here `inl` and `inr` definitely would violate put-get. 
--     e.g. put inl (Right foo)
-- Also we need an instance Bifunctor for Either as well
--     bimap :: Lens a b -> Lens c d -> Lens (Either a c) (Either b d)
-- which shows the same issue.

{- RULES
"codiag . inl"  codiag . inl = id
"codiag . inr"    codiag . inr = id
"(f ||| g) . inl" forall f g. (f ||| g) . inl = f
"(f ||| g) . inr" forall f g. (f ||| g) . inr = g
 -}

-- NOTE: well-behaved methods here are: codiag/(|||)
 
instance PreCoCartesian Lens where
    type Sum Lens = Either
  --inl :: Lens a (Either a b)
    inl = Lens $ \a-> return (either id (const a), Left a)
  --inr :: Lens b (Either a b)
    inr = Lens $ \b-> return (either (const b) id, Right b)
  --codiag :: Lens (Either a a) a
    --codiag = id ||| id -- DEFAULT
    codiag = Lens $ either (l Left) (l Right) where
        l lr a = return (return . lr, a)
  --(|||) :: Lens a c -> Lens b c -> Lens (Either a b) c
    --f ||| g = codiag . bimap f g  -- DEFAULT
    --(Lens f) ||| (Lens g) = Lens $ either (mkl Left f) (mkl Right g)
    --    where mkl c l = bimap (liftM c .) c . l


-- --------------------
-- DEPENDS ON MONOIDAL, IS Exp LENS? HMMMM...
-- LENS IS VERY UNLIKELY TO BE A CCC AFAICT:
instance CCC Lens where
    type Exp Lens = ??
    apply :: Lens (?? a b, a) b  -- implies at least: given a 'b', produce an 'a'
    curry :: Lens (a,b) c -> Lens a (?? b c) -- implies: given a (Lens (a,b) c) and a (Lens b c), produce an 'a'
    curry (Lens f) = 
        Lens $ \a ->
            \lbc-> 
            Lens $ \b-> do 
                (cMab,c) <- f (a,b)
                return(\c'-> liftM snd $ cMab c' , c)  -- ...erm. Djinn might be helpful here.
    uncurry :: ...
-- --------------------

RULES
"factor . distribute" factor . distribute = id
"distribute . factor" distribute . factor = id

  where...
    factor :: Lens (Either (a,b) (a,c)) (a, Either b c)
    factor = second inl ||| second inr


-- sub-class of PreCartesian
instance (Monad ms, Monad mg)=> Distributive (Lens ms mg) where
  --distribute :: Lens (a, Either b c) (Either (a,b) (a,c))
    distribute = Lens $ \(a,ebc)-> 
        return (return . factor, bimap ((,) a) ((,) a) ebc)

-}



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



