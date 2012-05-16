{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, DeriveDataTypeable, 
             FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies,
             TypeSynonymInstances, RankNTypes
  #-}
module System.File.Tree
       ( -- *Directory tree structure
         FSTree(..), mkFSTree, FSForest
         -- *Generic rose trees 
         -- |Re-exported from "Data.Tree"
       , Tree(..), Forest
         -- * Overloaded tree lenses
       , TreeLens(..)
         -- *Retrieve directory trees from the filesystem
       , getDirectory, getDirectory'
         -- *IO operations on directory trees
         -- **copy
       , copyTo, copyTo_
         -- **move
       , moveTo, moveTo_
       , mergeInto, mergeInto_
         -- **remove
       , remove, tryRemove, tryRemoveWith              
         -- * Operations on directory trees
         -- **basic operations
       , pop, pop_, flatten, flattenPostOrder, levels
         -- ** map over subtrees
       , map, mapM, mapM_
         -- **find subtrees
       , find, findM
         -- **filter subtrees
       , filter, filterM
         -- ***useful predicates
       , isFile, isDir, isSymLink, isSymDir, isSymFile, isRealFile, isRealDir
         -- **extract subtrees
       , extract, extractM
         -- **truncate tree to a maximum level
       , truncateAt
         -- **zip with destination tree
       , zipWithDest, zipWithDestM, zipWithDestM_
       )where

import System.IO.Unsafe (unsafeInterleaveIO)
import Unsafe.Coerce (unsafeCoerce)

import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist, 
                         copyFile, renameFile, removeFile, createDirectory,
                         createDirectoryIfMissing, removeDirectory,
                         removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink)

import Data.Tree (Tree(..), Forest)
import qualified Data.Tree as Tree (flatten, levels)
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
import Control.DeepSeq (NFData(..), deepseq)
import Control.Conditional (ifM, (<&&>), (<||>), notM, condM, otherwiseM)


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

instance NFData FSTree where
  rnf t = getL label t `deepseq` rnf (getL children t)

type FSForest = [FSTree]

-- |A pseudo-constructor for 'FSTree'.
mkFSTree :: FilePath -> FSForest -> FSTree
mkFSTree a = FSTree . Node a . mapToTree

-- |Efficiently maps 'FSTree' over a list. This is more efficient than map FSTree 
mapFSTree :: Forest FilePath -> FSForest
mapFSTree = unsafeCoerce

-- |Efficiently maps toTree over a list. This is more effficient than map toTree
mapToTree :: FSForest -> Forest FilePath
mapToTree = unsafeCoerce

-- |Overloaded lenses for 'Tree' and 'FSTree'
class TreeLens t a | t -> a where
  -- |Lens for the value at a tree node
  label    :: Lens t a
  -- |Lens for a list of children nodes
  children :: Lens t [t]

instance TreeLens (Tree a) a where
  label    = lens rootLabel (\a t -> t {rootLabel = a})
  children = lens subForest (\c t -> t {subForest = c}) 

instance TreeLens FSTree FilePath where
  label = lens (rootLabel . toTree) 
               (\a fs -> FSTree $ (toTree fs) {rootLabel = a})
  children = lens (mapFSTree . subForest . toTree)
                  (\c fs -> FSTree $ (toTree fs) {subForest = mapToTree c})

-- |Lazily retrieves a representation of a directory and its contents recursively.
--
-- Relative paths are not converted to absolute. Thus, a FSTree formed from a 
-- relative path will contain a \"relative tree\", and the usual caveats of
-- current directories and relative paths apply to the tree as a whole.
getDirectory :: FilePath -> IO FSTree
getDirectory = getDir_ unsafeInterleaveIO

-- |A strict variant of 'getDirectory'. 
--
-- Though race conditionals are still a possibility, this function will avoid some 
-- race conditions that could be caused from the use of lazy IO. For large 
-- directories, this function can easily cause memory leaks.
getDirectory' :: FilePath -> IO FSTree
getDirectory' = getDir_ id

getDir_ :: (forall a. IO a -> IO a) -> FilePath -> IO FSTree
getDir_ f p = mkFSTree p <$> getChildren p
  where getChildren path = do
          cs <- P.filter (`notElem` [".",".."]) 
                <$> f (getDirectoryContents path)
          forM cs $ \c ->
            let c' = path </> c
            in ifM (isRealDir c')
                   ( f . fmap (mkFSTree c) . getChildren $ c' )
                   ( return $ mkFSTree c [] )


-- |Checks if a path refers to a file.
isFile :: FilePath -> IO Bool
isFile = doesFileExist

-- |Checks if a path refer to a directory.
isDir :: FilePath -> IO Bool
isDir = doesDirectoryExist

-- |Checks if a path refers to a symbolic link. 
-- NOTE: always returns False on Windows
isSymLink :: FilePath -> IO Bool
#if CABAL_OS_WINDOWS
isSymLink p = return False
#else
isSymLink p = isSymbolicLink <$> getSymbolicLinkStatus p
#endif

-- |Checks if a path refers to a symbolically linked directory 
isSymDir :: FilePath -> IO Bool
isSymDir p = isDir p <&&> isSymLink p

-- |Checks if a path refers to a symbolically linked file
isSymFile :: FilePath -> IO Bool
isSymFile p = isFile p <&&> isSymLink p

-- |Checks if a path refers to a real directory (not a symbolic link)
isRealDir :: FilePath -> IO Bool
isRealDir p = isDir p <&&> notM (isSymLink p)

-- |Checks if a path refers to a real file (not a symbolic link)
isRealFile :: FilePath -> IO Bool
isRealFile p = isFile p <&&> notM (isSymLink p)

-- |Remove the root node of a filesystem tree, while preserving the paths of
-- its children. In other words, this function does not alter where any paths point 
-- to.
pop :: FSTree -> (FilePath, FSForest)
pop fs = (path, P.map prepend cs)
  where path = getL label fs
        cs = getL children fs
        prepend = modL label (path </>)

-- | > pop_ = snd . pop
pop_ :: FSTree -> FSForest
pop_ = snd . pop

-- |Flattens a filesystem tree into a list of its contents. This is a pre-order 
-- traversal of the tree.
flatten :: FSTree -> [FilePath]
flatten = Tree.flatten . prependPaths 

-- |A post-order traversal of the filesystem tree.
flattenPostOrder :: FSTree -> [FilePath]
flattenPostOrder = toList . flatten' . prependPaths
  where flatten' (Node p cs) = DL.concat (P.map flatten' cs) `snoc` p  

-- |List of file paths at each level of the tree.
levels :: FSTree -> [[FilePath]]
levels = Tree.levels . prependPaths

-- |Applies a function over the filepaths of a directory tree. 
--
-- Because we can't guarantee that the internal 'FSTree' representation is preserved 
-- in any way, the result is a regular 'Tree'.
map :: (FilePath -> b) -> FSTree -> Tree b
map f = fmap f . toTree

-- |Applies a monadic action to every filepath in a filesystem tree.
mapM :: Monad m => (FilePath -> m b) -> FSTree -> m (Tree b)
mapM f = T.mapM f . toTree

-- |'mapM' with the result discarded.
mapM_ :: Monad m => (FilePath -> m b) -> FSTree -> m () 
mapM_ f t = mapM f t >> return ()

-- |Applies a predicate to each path name in a filesystem forest, and removes
-- all unsuccessful paths from the result. If a directory fails the predicate test, 
-- then it will only be removed if all of its children also fail the test
filter :: (FilePath -> Bool) -> FSForest -> FSForest
filter p = runIdentity . filterM (return . p)

-- |Find all sub-forests within a forest that match the given predicate.
find :: (FilePath -> Bool) -> FSForest -> FSForest
find p = snd . extract p

-- |The first element of the result represents the forest after removing all 
-- subtrees that match the given predicate, and the second element is a list of 
-- trees that matched. This could be useful if you want to handle certain 
-- directories specially from others within a sub-filesystem.
extract :: (FilePath -> Bool) -> FSForest -> (FSForest, FSForest)
extract p = runIdentity . extractM (return . p)

-- |Monadic 'filter'.
filterM :: Monad m =>
           (FilePath -> m Bool) -> FSForest -> m FSForest
filterM p = foldrM (filter' . prependPaths) []
  where filter' (Node path cs) ts =
          ifM (p path)
          (liftM ((:ts) . mkFSTree path) $ foldrM filter' [] cs)
          (do
              cs' <- foldrM filter' [] $ cs
              return $ case cs' of
                [] -> ts
                _  -> mkFSTree path cs' : ts
          )

-- |Monadic 'find'.
findM :: Monad m =>
         (FilePath -> m Bool) -> FSForest -> m FSForest
findM p = liftM snd . extractM p

-- |Monadic 'extract'.
extractM :: Monad m => 
            (FilePath -> m Bool) -> FSForest -> m (FSForest, FSForest)
extractM p = liftM (second toList) . extractM_ p

extractM_ :: Monad m => 
             (FilePath -> m Bool) -> FSForest -> m (FSForest, DList FSTree)
extractM_ p = foldrM extract' ([], DL.empty) . P.map prependPaths
  where 
    extract' t@(Node path cs) (ts, es)
      = ifM (p path)
        (
             return (ts, FSTree t `cons` es)
        )
        (do
            (cs', es') <- foldrM extract' ([], DL.empty) cs
            let t' = mkFSTree path cs'
            return (t' : ts, es' `append` es)
        )

-- |Truncate a tree to a given maximum level, where root is level 0.  
truncateAt :: TreeLens t a => Word -> t -> t
truncateAt n = modL children (mapMaybe (truncate' 1))
  where 
    truncate' i t
      | i > n = Nothing
      | otherwise = Just . modL children (mapMaybe (truncate' (i+1))) $ t


-- |Converts a 'FSTree' to a 'Tree' where each node in the 'Tree' contains the
-- full path name of the filesystem node it represents.
prependPaths :: FSTree -> Tree FilePath
prependPaths (FSTree root) = modL children (P.map (prepend' rootPath)) root
  where
    rootPath = rootLabel root
    prepend' parentPath = prependChildren . modL label (parentPath </>)
    prependChildren fs = modL children (P.map (prepend' (rootLabel fs))) fs

-- |Copy a filesystem tree to a new location, creating directories as necessary.
-- The resulting 'FSTree' represents all of the copied directories/files in their 
-- new home.
--
-- Note that an single exception will halt the entire operation.
copyTo :: FilePath -> FSTree -> IO FSTree
copyTo = zipWithDestM_ $ \src dest ->ifM (isRealDir src)
                                     (createDirectoryIfMissing False dest) 
                                     (copyFile src dest)

copyTo_ :: FilePath -> FSTree -> IO ()
copyTo_ = (void .) . copyTo

-- |Move a filesystem tree to a new location, deleting any file/directory that
-- was present at the given destination path.
--
-- Directories listed in the source filesystem tree are removed if the move
-- operation empties their contents completely. The resulting 'FSTree' represents 
-- all the moved directories/files in their new home.
--
-- Note that an single exception will halt the entire operation.
moveTo :: FilePath -> FSTree ->  IO FSTree
moveTo dest fs = do
  condM [(isSymLink dest <||> isFile dest, removeFile dest)
        ,(isDir  dest,                     removeDirectoryRecursive dest)
        ,(otherwiseM,                      return ())
        ]
  zipWithDestM_
    (\s d -> ifM (isRealDir s) 
             (do tryRemoveDirectory s
                 createDirectory d)
             (renameFile s d)
    )
    dest fs
    <* removeEmptyDirectories fs

moveTo_ :: FilePath -> FSTree -> IO ()
moveTo_ = (void .) . moveTo

-- |This is similar to 'moveTo', except that whatever was present at the destination
-- path isn't deleted before the move operation commences.
--
-- Note that an single exception will halt the entire operation.
mergeInto :: FilePath -> FSTree -> IO FSTree
mergeInto dest fs = zipWithDestM_ 
                    (\s d -> ifM (isRealDir s) 
                                 (createDirectoryIfMissing False d) 
                                 (renameFile s d)
                    )
                    dest fs
                    <* removeEmptyDirectories fs
  
mergeInto_ :: FilePath -> FSTree -> IO ()
mergeInto_ = (void .) . mergeInto

-- |Remove a given filesystem tree. Directories are only removed
-- if the remove operation empties its contents.
--
-- Note that an single exception will halt the entire operation.
remove :: FSTree -> IO ()
remove = void . tryRemoveWith throwIO

-- |A variant of 'remove'. 'IOExceptions' do not stop the removal
-- process, and all 'IOExceptions' are accumulated into a list as the result of
-- the operation.
tryRemove :: FSTree -> IO [IOException]
tryRemove = tryRemoveWith return

-- |A variant of 'remove'. Allows you to specify your own exception handler to handle
-- exceptions for each removal operation.
tryRemoveWith :: (IOException -> IO a) -> FSTree -> IO [a]
tryRemoveWith handler = fmap (catMaybes . DL.toList) . remove' . prependPaths
  where remove' (Node p cs) =
          DL.snoc   <$> (fmap DL.concat . P.mapM remove' $ cs)
                    <*> ifM (doesDirectoryExist p)
                            (tryRemoveDirectory p >> return Nothing)
                            (removeFile p         >> return Nothing)
                            `catch` (fmap Just . handler)
            



-- |Helper function for removals.
removeEmptyDirectories :: FSTree -> IO ()
removeEmptyDirectories = P.mapM_ tryRemoveDirectory . flattenPostOrder

-- |Helper function for removals.
tryRemoveDirectory :: FilePath -> IO ()
tryRemoveDirectory path = removeDirectory path `catch` handler
  where handler :: IOException -> IO () 
        handler = const (return ())

-- |A generalization of the various move, copy, and remove operations. This
-- operation pairs each node of a 'FSTree' with a second path formed by rerooting
-- the filesystem tree to the given destination path.
zipWithDest :: (FilePath -> FilePath -> a)
               -> FilePath -> FSTree
               -> [a]
zipWithDest f dest fs = runIdentity $ zipWithDestM ((return .) . f) dest fs 

-- |Monadic 'zipWithDest'
zipWithDestM :: Monad m => (FilePath -> FilePath -> m a)
                -> FilePath -> FSTree
                -> m [a]
zipWithDestM f dest fs = liftM fst $ zipWithDestM__ f dest fs

-- |A variant of 'zipWithDestM' where the result is discarded and instead the 
-- rerooted filesystem tree is returned.
zipWithDestM_ :: Monad m => 
                 (FilePath -> FilePath -> m a)
                -> FilePath -> FSTree
                -> m FSTree
zipWithDestM_ f dest fs = liftM snd $ zipWithDestM__ f dest fs

-- |Internal implementation of the zipWithDest* operations.
zipWithDestM__ :: Monad m =>
                  (FilePath -> FilePath -> m a)
                  -> FilePath -> FSTree
                  -> m ([a], FSTree)
zipWithDestM__ f rootDest  fs =
  liftM2 (,) (sequence $ (zipWith f `on` flatten) fs destFs)
             (return destFs)
  where
    destFs = setL label rootDest fs