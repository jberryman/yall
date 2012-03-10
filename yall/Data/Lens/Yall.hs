{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}
module Data.Lens.Yall
    where

-- from 'categories':
import Control.Category
import Control.Categorical.Bifunctor

import Data.Functor.Identity
import Prelude hiding (id,(.))
import Control.Category
import Control.Monad

-- constrain these 'm's to Monad?
newtype Lens ms mg a b = Lens { runLens :: a -> mg (b -> ms a, b) }

instance (Monad ms, Monad mg)=> Category (Lens ms mg) where
    id = Lens $ \a -> return (\a'-> return a', a)
    (Lens f) . (Lens g) = 
        Lens $ \a-> do 
            (bMa,b) <- g a
            (cMb,c) <- f b
            return (cMb >=> bMa, c)

instance (Monad ms, Monad mg)=> PFunctor (,) (Lens ms mg) (Lens ms mg) where
    first = firstDefault

instance (Monad ms, Monad mg)=> QFunctor (,) (Lens ms mg) (Lens ms mg) where
    second = secondDefault

instance (Monad ms, Monad mg)=> Bifunctor (,) (Lens ms mg) (Lens ms mg) (Lens ms mg) where
    bimap (Lens f) (Lens g) = 
        Lens $ \(a,c)-> do
            (bMa,b) <- f a                       
            (dMc,d) <- g c
            let setCont (b',d') = do a' <- bMa b'
                                     c' <- dMc d'
                                     return (a',c')
            return (setCont, (b,d))
                          -- in store (\(b',d')->(ba b',dc d')) (b,d)
    -- bimap (Lens f) (Lens g) = Lens $ \(a,c)-> 
                         -- let (ba,b) = runStore $ f a                       
                          --    (dc,d) = runStore $ g c
                          -- in store (\(b',d')->(ba b',dc d')) (b,d) -- i.e. tuple up


-- | a simple lens, suitable for single-constructor types
type (:->) = Lens Identity Identity

-- | a lens that can fail in the Maybe monad on the outer type, suitable for a
-- normal lens on a multi-constructor type
type (:~>) = Lens Identity Maybe
