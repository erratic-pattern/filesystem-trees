{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, 
             FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, 
             TypeSynonymInstances
  #-}
module System.File.Tree
       ( -- *Directory tree structure
         FSTree(..), FSForest, mkFSTree
         -- *Generic rose trees 
         -- |Re-exported from "Data.Tree"
       , Tree(..), Forest
         -- * Overloaded tree lenses
       , TreeLens(..)
         -- *Retrieve directory trees from the filesystem
       , getDirectory, getDirectory'
         -- * Operations on directory trees
         -- **basic operations
       , pop, pop_, flatten, flattenPostOrder
         -- ** mapping over subtrees
       , map, mapM, mapM_
         -- **find subtrees
       , find, findM
         -- **filter subtrees
       , filter, filterM
         -- ***Useful predicates
       , isSymLink, isSymDir, isSymFile, isDir
         -- **extract subtrees
       , extract, extractM
         -- **truncate tree to a maximum level
       , truncateAt
         -- **zip with destination tree
       , zipWithDest, zipWithDestM, zipWithDestM_
         -- **IO operations on directory trees
         -- ***copying
       , copyTo, copyTo_
         -- ***moving
       , moveTo, moveTo_
       , mergeInto, mergeInto_
         -- ***removing
       , remove, tryRemove, tryRemoveWith, removeEmptyDirectories
       )where

import System.IO.Unsafe (unsafeInterleaveIO)
import Unsafe.Coerce (unsafeCoerce)

import System.Directory (getDirectoryContents, doesDirectoryExist, copyFile, 
                         renameFile, removeFile, createDirectory, 
                         createDirectoryIfMissing, removeDirectory)
import System.FilePath ((</>))
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink)

import Data.Tree (Tree(..), Forest)
import qualified Data.Tree as Tree (flatten)
import Data.DList as DL (DList(..), cons, append, toList, empty, concat, snoc)

import Control.Exception (throwIO, catch, IOException)
import Control.Monad (forM, liftM, liftM2, void)
import Control.Monad.Identity (runIdentity)
import Control.Applicative ((<$>), (<*>), (<*))
import Control.Arrow (second)
import Data.Foldable (foldrM)
import qualified Data.Traversable as T (mapM)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Function (on)
import Data.Lens.Common (Lens, lens, getL, setL, modL)
import Control.Cond (ifM, (<&&>), notM, whenM)

import Data.Word (Word)
import Data.Typeable (Typeable)
import Data.Data (Data)

import Prelude hiding (filter, catch, map, mapM, mapM_)
import qualified Prelude as P

-- |A representation of a filesystem tree. The root label contains the
-- path context, and every child node is a single file/directory name.
--
-- For example, say we have the following directory structure on our
-- filesystem:
--
-- @ 
--   /example$ tree foo --charset ASCII
--   foo
--   `-- bar
--       `-- test
--           |-- a
--           |-- A
--           |   |-- x
--           |   `-- y
--           |-- b
--           `-- B
-- @
--
-- then calling 'getDirectory' \"\/example\/foo\/bar\/test\" will yield a FSTree with
-- the following structure:
-- 
-- >  /example$ ghci
-- >  Prelude Data.Tree System.Directory.Tree> putStrLn . drawTree . toTree =<< getDirectory "/example/foo/bar/test"
-- >  /example/foo/bar/test
-- >  |
-- >  +- A
-- >  |  |
-- >  |  +- x
-- >  |  |
-- >  |  `- y
-- >  |
-- >  +- B
-- >  |
-- >  +- a
-- >  |
-- >  `- b

newtype FSTree = FSTree { toTree :: Tree FilePath } deriving 
                (Typeable, Data, Eq, Read, Show)

type FSForest = [FSTree]

-- |Pseudo-constructor for 'FSTree'
mkFSTree :: FilePath -> FSForest -> FSTree
mkFSTree a = FSTree . Node a . mapToTree

mapFSTree :: Forest FilePath -> FSForest
mapFSTree = unsafeCoerce

mapToTree :: FSForest -> Forest FilePath
mapToTree = unsafeCoerce

-- |Overloaded lenses for 'Tree' and 'FSTree'
class TreeLens t a | t -> a where
  label    :: Lens t a
  children :: Lens t [t]

instance TreeLens (Tree a) a where
  label    = lens rootLabel (\a t -> t {rootLabel = a})
  children = lens subForest (\c t -> t {subForest = c}) 

instance TreeLens FSTree FilePath where
  label = lens (rootLabel . toTree) 
               (\a fs -> FSTree $ (toTree fs) {rootLabel = a})
  children = lens (mapFSTree . subForest . toTree)
                  (\c fs -> FSTree $ (toTree fs) {subForest = mapToTree c})

getDirectory :: FilePath -> IO FSTree
getDirectory = getDir_ unsafeInterleaveIO

getDirectory' :: FilePath -> IO FSTree
getDirectory' = getDir_ id

getDir_ :: (IO FSTree -> IO FSTree) -> FilePath -> IO FSTree
getDir_ f p = mkFSTree p <$> getChildren p
  where getChildren path = do
          cs <- P.filter (`notElem` [".",".."]) 
                <$> getDirectoryContents path
          forM cs $ \c ->
            let c' = path </> c
            in ifM (isDir c')
                   ( f . fmap (mkFSTree c) . getChildren $ c' )
                   ( return $ mkFSTree c [] )

-- |Checks if a path refers to a symbolic link
isSymLink :: FilePath -> IO Bool
isSymLink p = isSymbolicLink <$> getSymbolicLinkStatus p

-- |Checks if a path refers to a symbolically linked directory 
isSymDir :: FilePath -> IO Bool
isSymDir p = doesDirectoryExist p <&&> isSymLink p

-- |Checks if a path refers to a symbolically linked file
isSymFile :: FilePath -> IO Bool
isSymFile p = notM (doesDirectoryExist p) <&&> isSymLink p

-- |Checks if a path refers to a real directory (not a symbolic link)
isDir :: FilePath -> IO Bool
isDir p = doesDirectoryExist p <&&> notM (isSymLink p)

pop :: FSTree -> (FilePath, FSForest)
pop fs = (path, P.map prepend cs)
  where path = getL label fs
        cs = getL children fs
        prepend = modL label (path </>)

pop_ :: FSTree -> FSForest
pop_ = snd . pop

flatten :: FSTree -> [FilePath]
flatten = Tree.flatten . prependPaths 

flattenPostOrder :: FSTree -> [FilePath]
flattenPostOrder = toList . flatten' . prependPaths
  where flatten' (Node p cs) = DL.concat (P.map flatten' cs) `snoc` p  

map :: (FilePath -> b) -> FSTree -> Tree b
map f = fmap f . toTree

mapM :: Monad m => (FilePath -> m b) -> FSTree -> m (Tree b)
mapM f = T.mapM f . toTree

mapM_ :: Monad m => (FilePath -> m b) -> FSTree -> m () 
mapM_ f t = mapM f t >> return ()

filter :: (FilePath -> Bool) -> FSForest -> FSForest
filter p = fst . extract p

find :: (FilePath -> Bool) -> FSForest -> FSForest
find p = snd . extract p

extract :: (FilePath -> Bool) -> FSForest -> (FSForest, FSForest)
extract p = runIdentity . extractM (return . p)

filterM :: Monad m =>
           (FilePath -> m Bool) -> FSForest -> m FSForest
filterM p = liftM fst . extractM p

findM :: Monad m =>
         (FilePath -> m Bool) -> FSForest -> m FSForest
findM p = liftM snd . extractM p

extractM :: Monad m => 
            (FilePath -> m Bool) -> FSForest -> m (FSForest, FSForest)
extractM p = liftM (second toList) . extractM_ p

extractM_ :: Monad m => 
             (FilePath -> m Bool) -> FSForest -> m (FSForest, DList FSTree)
extractM_ p = foldrM extract' ([], DL.empty) . P.map prependPaths
  where 
    extract' t@(Node path cs) (ts, es)
      = ifM (p path)
        ( do
             (cs', es') <- foldrM extract' (ts, es) cs
             let t' = mkFSTree path cs'
             return (t' : ts, es' `append` es)
        )
        (
          return (ts, FSTree t `cons` es)
        )

truncateAt :: TreeLens t a => Word -> [t] -> [t]
truncateAt n = mapMaybe (truncate' 0)
  where 
    truncate' i t
      | i >= n = Nothing
      | otherwise = Just . modL children (mapMaybe (truncate' (i+1))) $ t


prependPaths :: FSTree -> Tree FilePath
prependPaths (FSTree root) = modL children (P.map (prepend' rootPath)) root
  where
    rootPath = rootLabel root
    prepend' parentPath = prependChildren . modL label (parentPath </>)
    prependChildren fs = modL children (P.map (prepend' (rootLabel fs))) fs

copyTo :: FilePath -> FSTree -> IO FSTree
copyTo = zipWithDestM_ $ \src dest ->ifM (isDir src)
                                     (createDirectoryIfMissing False dest) 
                                     (copyFile src dest)

copyTo_ :: FilePath -> FSTree -> IO ()
copyTo_ = (void .) . copyTo

moveTo :: FilePath -> FSTree ->  IO FSTree
moveTo dest fs = do
  whenM (isDir dest) $ remove =<< getDirectory dest
  zipWithDestM_
    (\s d -> ifM (isDir s) 
             (do tryRemoveDirectory s
                 createDirectory d)
             
             (renameFile s d)
    )
    dest fs
    <* removeEmptyDirectories fs

moveTo_ :: FilePath -> FSTree -> IO ()
moveTo_ = (void .) . moveTo

mergeInto :: FilePath -> FSTree -> IO FSTree
mergeInto dest fs = zipWithDestM_ 
                    (\s d -> ifM (isDir s) 
                                 (createDirectoryIfMissing False d) 
                                 (renameFile s d)
                    )
                    dest fs
                    <* removeEmptyDirectories fs
  
mergeInto_ :: FilePath -> FSTree -> IO ()
mergeInto_ = (void .) . mergeInto

remove :: FSTree -> IO ()
remove = void . tryRemoveWith throwIO

tryRemove :: FSTree -> IO [IOException]
tryRemove = tryRemoveWith return

tryRemoveWith :: (IOException -> IO a) -> FSTree -> IO [a]
tryRemoveWith handler = fmap (catMaybes . DL.toList) . remove' . prependPaths
  where remove' (Node p cs) =
          DL.snoc   <$> (fmap DL.concat . P.mapM remove' $ cs)
                    <*> ifM (doesDirectoryExist p)
                            (removeDirectory p >> return Nothing)
                            (removeFile p      >> return Nothing)
                            `catch` (fmap Just . handler)
            

removeEmptyDirectories :: FSTree -> IO ()
removeEmptyDirectories = P.mapM_ tryRemoveDirectory . flattenPostOrder

tryRemoveDirectory :: FilePath -> IO ()
tryRemoveDirectory path = removeDirectory path `catch` handler
  where handler :: IOException -> IO () 
        handler = const (return ())

zipWithDest :: (FilePath -> FilePath -> a)
               -> FilePath -> FSTree
               -> [a]
zipWithDest f dest fs = runIdentity $ zipWithDestM ((return .) . f) dest fs 

zipWithDestM :: Monad m => (FilePath -> FilePath -> m a)
                -> FilePath -> FSTree
                -> m [a]
zipWithDestM f dest fs = liftM fst $ zipWithDestM__ f dest fs

zipWithDestM_ :: Monad m => 
                 (FilePath -> FilePath -> m a)
                -> FilePath -> FSTree
                -> m FSTree
zipWithDestM_ f dest fs = liftM snd $ zipWithDestM__ f dest fs

zipWithDestM__ :: Monad m =>
                  (FilePath -> FilePath -> m a)
                  -> FilePath -> FSTree
                  -> m ([a], FSTree)
zipWithDestM__ f rootDest  fs =
  liftM2 (,) (sequence $ (zipWith f `on` flatten) fs destFs)
             (return destFs)
  where
    destFs = setL label rootDest fs