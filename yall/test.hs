import Data.Yall.Lens
import Data.Yall.Iso

import Control.Monad
import Prelude hiding ((.),id)
import Control.Category
import Control.Categorical.Bifunctor

import Control.Monad.Identity(Identity)


-- -------------------------------------------
-- PARTIAL LENSES


-- First, lenses for a sum type. This is something like what we'll generate
-- with template haskell.
data Test a = C1 { _testString :: String, _testA :: a }
            | C2 { _testString :: String, _testRec :: Test a }

-- pure lens, polymorphic in Monad for composability:
testString :: (Monad w, Monad m)=> Lens w m (Test a) String
testString = Lens $ \t-> return (\s-> return t{ _testString = s }, _testString t)

-- lenses that can fail. For now, use Maybe. In the TH library, I'll probably
-- generate lenses polymorphic in <http://hackage.haskell.org/package/failure>
testA :: LensM Maybe (Test a) a
testA = Lens f where
    f (C1 s a) = return (return . C1 s, a)
    f _ = Nothing

testRec :: LensM Maybe (Test a) (Test a)
testRec = Lens f where
    f (C2 s i) = return (return . C2 s, i)
    f _        = Nothing

-- conposing a pure and partial lens:
demo0 :: Maybe String
demo0 = getM (testString . testRec . testRec) (C2 "top" (C1 "lens will fail" True))


-- -------------------------------------------
-- LENSES WITH MORE ABSTRACT MONADIC GETTER 


-- Here we have a lens with a getter in the list monad, defining a mutable view
-- on the Nth element of the list:
nth :: LensM [] [a] a
nth = Lens $ foldr nthGS []
    where nthGS n l = (return . (: map snd l), n) : map (prepend n) l
          prepend = first . fmap . liftM . (:)

-- This composes nicely. Set the Nth element of our list to 0:
demo1 :: [ [(Char,Int)] ]
demo1 = setM (sndL . nth) 0 [('a',1),('b',2),('c',3)]


-- -------------------------------------------
-- LENSES WITH MONADIC SETTER

-- TODO: CONSIDER BIND FUNCTIONS TO INJECT EFFECTS WITH FUNCTIONS LIKE :: s -> m ()
--          effectId :: (a -> m ()) -> Iso w m a a -- id with effects
--
-- persist modifications to a type to a given file. An effect-ful identity Iso.
persistI :: (Monad m)=> FilePath -> Iso IO m String String
persistI nm = Iso return $ \s-> (writeFile nm s >> return s )

-- we'll use this one:
tmpFile = "/tmp/yall-test"
printFileContents = putStrLn . ("file contents: " ++) =<< readFile tmpFile

-- build a new Iso with some pre-defined Iso's that offers a [Int] view on a
-- string that looks like, e.g. "1 2 3 4 5":
unserializedI :: (Monad w, Monad m) => Iso w m String [Int]
unserializedI = ifmap (inverseI showI) . wordsI

-- now add "persistence" effects to the above Iso so everytime we do a "set"
-- we update the file "yall-test" to reflect the new type.
unserializedIP :: Iso IO Identity String [Int]
unserializedIP = unserializedI . persistI tmpFile

demo2 :: IO ()
demo2 = do
    -- return string representation of [1..5], writing it to disk
    str <- unapply unserializedIP [1..5]

    -- LOGGING: the string we got above (by setting [Int]) was written to a file:
    print str
    printFileContents

    -- convert Iso to a lens, so we can use 'modify' on the [Int] representation,
    -- once again persisting the modified string to disk:
    str' <- modifyW (isoL unserializedIP) (map (*2) . (6 :) . reverse) str

    -- LOGGING: now the file was modified to reflect the changed value:
    print str'
    printFileContents

{-
 - TODO SUMMARY:
 -
 - * remove the setEmpty stuff and build using all Iso's above
 -     - see also Lens.hs
 - * re-arrange order of types for set and use let-floating, so that we are
 -   most efficient when partially-applying in zipper scenario
 -     set :: (a :-> b) -> a -> b -> a
 - * make monadic interface a class: 
 -     class LensM l where
 -         setM :: l m a b -> ... -> m a
 -         getM :: ... -> m b
 -         modifyM f = setM . f . getM
 -     - make (Lens Identity) an instance
 -     - provide newtype wrappers for other "lifting" mechanisms to support this interface
 - * look into ambiguous type annoyance for polymorphic-in-monad code
 - * consider re-exporting 'Identity'
 -}
