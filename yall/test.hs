import Data.Yall.Lens
import Data.Yall.Iso

import Control.Monad
import Prelude hiding ((.),id)
import Control.Category
import Control.Categorical.Bifunctor


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

-- TODO: THIS COULD BE AN ISO. ALSO CONSIDER BIND FUNCTIONS TO INJECT FUNCS :: s -> m (), SIMPLY ADD EFFECTS
-- persist modifications to a type to a given file. An effect-ful identity lens.
persistL :: (Monad m) => FilePath -> Lens IO m String String
persistL nm = Lens $ \s-> return (\s'-> writeFile nm s' >> return s', s)

-- we'll use this one:
tmpFile = "/tmp/yall-test"
printFileContents = putStrLn . ("file contents: " ++) =<< readFile tmpFile

-- TODO: THIS OF COURSE COULD ALSO BE AN ISO
-- build a lens with some pre-defined Iso's that offers a [Int] view on a
-- string that looks like, e.g. "1 2 3 4 5":
unserializedL :: (Monad w, Monad m) => Lens w m String [Int]
unserializedL = isoL $ ifmap (inverseI showI) . wordsI

-- now add "persistence" effects to the above lens so everytime we do a "set"
-- we update the file "yall-test" to reflect the new type.
unserializedLP :: (Monad m) => Lens IO m String [Int]
unserializedLP = unserializedL . persistL tmpFile

demo2 :: IO ()
demo2 = do
    -- TODO: IF WE BUILT AN ISO ABOVE, WE COULD USE unapply HERE:
    -- apply the lens setter to `mempty` for some Monoid ([Char] in this case)
    str <- setEmptyW unserializedLP [1..5]

    -- LOGGING: the string we got above (by setting [Int]) was written to a file:
    print str
    printFileContents

    str' <- modifyW unserializedLP (map (*2) . (6 :) . reverse) str

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
 -}
