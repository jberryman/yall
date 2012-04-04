import Data.Yall.Lens
import Data.Yall.Iso

import Control.Monad.IO.Class
import Control.Monad

import Prelude hiding ((.),id)
import Control.Category
import Data.Functor.Identity


-- -------------------------------------------
-- LENSES WITH MONADIC GETTER

-- cool, a lens abstracted over "nth" term:
nth :: Lens Identity [] [a] a
nth = Lens $ foldr nthGS []
    where nthGS n l = (\n'-> return (n':map snd l), n) : map (prepend n) l
          prepend n0 (f,n1) = (fmap (liftM (n0:)) f,n1)

-- How does that compose? Try this:
composeDemo1 :: [ [(Char,Int)] ]
composeDemo1 = setM (sndL . nth) 0 [('a',1),('b',2),('c',3)]


-- -------------------------------------------
-- LENSES WITH MONADIC SETTER

-- persist modifications to a type to a given file. An effect-ful identity lens.
persistL :: (MonadIO w, Monad m) => FilePath -> Lens w m String String
persistL nm = Lens $ \s -> do
    return (\s'-> liftIO $ writeFile nm s' >> return s', s)

-- we'll use this one:
tmpFile = "/tmp/yall-test"
printFileContents = putStrLn . ("file contents: " ++) =<< readFile tmpFile

-- build a lens with some pre-defined Iso's that offers a [Int] view on a
-- string that looks like, e.g. "1 2 3 4 5":
unserializedL :: (Monad w, Monad m) => Lens w m String [Int]
unserializedL = isoL $ ifmap (inverseI showI) . wordsI

-- now add "persistence" effects to the above lens so everytime we do a "set"
-- we update the file "yall-test" to redlect the new type.
unserializedLP :: (Monad m) => Lens IO m String [Int]
unserializedLP = unserializedL . persistL tmpFile

-- TODO: consider nice operators for set / get / modify?
--        ^$ (get), ^>>= :: m a -> Lens w m a b -> m b (getM, sort of)
composeDemo2 :: IO ()
composeDemo2 = do
    -- apply the lens setter to `mempty` for some Monoid ([Char] in this case)
    str <- setEmptyW unserializedLP [1..5]

    -- LOGGING: the string we got above (by setting [Int]) was written to a file:
    print str
    printFileContents

    str' <- modifyW unserializedLP (map (*2) . (6 :) . reverse) str

    -- LOGGING: now the file was modified to reflect the changed value:
    print str'
    printFileContents


