{-# LANGUAGE TypeOperators #-}
module Data.Yall (
    {- | 
       This is a subset of 'Data.Yall.Lens', exporting only the basic API.
       Furthermore, 'get', 'lensM', getM, setM, and modifyM are exported with
       more restrictive types than are found in Data.Yall.Lens, for simplicity.

       You should either import this module, or 'Data.Yall.Lens'.
    -}
    -- * Pure lenses
      (:->)
    , lens, get, set, modify
    -- * Partial lenses
    , (:~>)
    , lensM, getM, setM, modifyM
    ) where

import Data.Yall.Lens hiding (get, lensM, getM, setM, modifyM)
import qualified Data.Yall.Lens as L

-- | Run the getter function of a pure lens
get :: (a L.:-> b) -> a -> b
get = L.get

-- | Create a partial lens from a getter and setter
lensM :: (a -> Maybe b) -> (a -> Maybe (b -> a)) -> (a L.:~> b)
lensM = L.lensM

-- | Try to run the getter function on a value 
getM :: (a L.:~> b) -> a -> Maybe b
getM = L.getM

-- | try to run the setter function on an outer and new inner value
setM :: (a L.:~> b) -> a -> b -> Maybe a
setM = L.setM 

-- | try to modify the inner type of a value
modifyM :: (a L.:~> b) -> (b -> b) -> a -> Maybe a
modifyM = L.modifyM
