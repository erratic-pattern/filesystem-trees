{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, 
             FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, 
             TypeSynonymInstances
  #-}
module System.Directory.Tree
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
         -- **find subtrees
       , find, findM
         -- **filter subtrees
       , filter, filterM
         -- ***Useful predicates
       , isSymLink, isDir
         -- **extract subtrees
       , extract, extractM
         -- **truncate tree to a maximum level
       , truncateAt
         -- **Copy, move, and remove directory trees
       , copyTo, copyTo_,  moveTo, moveTo_, mergeInto, mergeInto_,  remove
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

import Control.Exception (catch, IOException)
import Control.Monad (forM, liftM, void)
import Control.Monad.Identity (runIdentity)
import Control.Applicative ((<$>), (<*))
import Control.Arrow (second)
import Data.Foldable (foldrM)
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Data.Lens.Common (Lens, lens, getL, setL, modL)
import Control.Cond (ifM, (<&&>), notM, whenM)

import Data.Word (Word)
import Data.Typeable (Typeable)
import Data.Data (Data)

import Prelude hiding (filter, catch)
import qualified Prelude as P (filter)

newtype FSTree = FSTree { toTree :: Tree FilePath } deriving 
                (Typeable, Data, Eq, Read, Show)

type FSForest = [FSTree]

mkFSTree :: FilePath -> FSForest -> FSTree
mkFSTree a = FSTree . Node a . mapToTree

mapFSTree :: Forest FilePath -> FSForest
mapFSTree = unsafeCoerce

mapToTree :: FSForest -> Forest FilePath
mapToTree = unsafeCoerce

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

-- |Checks if a path refers to a real directory (not a symbolic link)
isDir :: FilePath -> IO Bool
isDir p = doesDirectoryExist p <&&> notM (isSymLink p)

pop :: FSTree -> (FilePath, FSForest)
pop fs = (path, map prepend cs)
  where path = getL label fs
        cs = getL children fs
        prepend = modL label (path </>)

pop_ :: FSTree -> FSForest
pop_ = snd . pop

flatten :: FSTree -> [FilePath]
flatten = Tree.flatten . prependPaths 

flattenPostOrder :: FSTree -> [FilePath]
flattenPostOrder = toList . flatten' . prependPaths
  where flatten' (Node p cs) = DL.concat (map flatten' cs) `snoc` p  

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
extractM_ p = foldrM extract' ([], DL.empty) . map prependPaths
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
prependPaths (FSTree root) = modL children (map (prepend' rootPath)) root
  where
    rootPath = rootLabel root
    prepend' parentPath = prependChildren . modL label (parentPath </>)
    prependChildren fs = modL children (map (prepend' (rootLabel fs))) fs

copyTo :: FilePath -> FSTree -> IO FSTree
copyTo = zipWithDestM (const $ createDirectoryIfMissing False) copyFile

copyTo_ :: FilePath -> FSTree -> IO ()
copyTo_ = (void .) . copyTo

moveTo :: FilePath -> FSTree ->  IO FSTree
moveTo dest fs = do
  whenM (isDir dest) $ remove =<< getDirectory dest
  zipWithDestM 
    (\s d -> do tryRemoveDirectory s
                createDirectory d)
    renameFile 
    dest fs
    <* removeEmptyDirectories fs

moveTo_ :: FilePath -> FSTree -> IO ()
moveTo_ = (void .) . moveTo

mergeInto :: FilePath -> FSTree -> IO FSTree
mergeInto dest fs = zipWithDestM 
                    (\_ d -> createDirectoryIfMissing False d) 
                    renameFile
                    dest fs
                    <* removeEmptyDirectories fs
  
mergeInto_ :: FilePath -> FSTree -> IO ()
mergeInto_ = (void .) . mergeInto

remove :: FSTree -> IO ()
remove = remove' . prependPaths
  where remove' (Node p cs) = do
          mapM_ remove' cs
          ifM (doesDirectoryExist p)
              (removeDirectory p)
              (removeFile p)

removeEmptyDirectories :: FSTree -> IO ()
removeEmptyDirectories = mapM_ tryRemoveDirectory . flattenPostOrder

tryRemoveDirectory :: FilePath -> IO ()
tryRemoveDirectory path = removeDirectory path `catch` handler
  where handler :: IOException -> IO () 
        handler = const (return ())

zipWithDestM :: (FilePath -> FilePath -> IO ())
                -> (FilePath -> FilePath -> IO ())
                -> FilePath -> FSTree
                -> IO FSTree
zipWithDestM dirF fileF rootDest  fs = do 
  sequence_ $ (zipWith f `on` flatten) fs destFs
  return destFs
  where
    destFs = setL label rootDest fs
    f src dest = ifM (isDir src)
                     (dirF src dest)
                     (fileF src dest)