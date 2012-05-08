{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, 
             FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, 
             TypeSynonymInstances
  #-}
module System.Directory.Tree
       ( -- *Directory tree structure
         FSTree(..), FSForest, mkFSTree, unFSTree
         -- *Generic rose trees 
         -- |Re-exported from "Data.Tree"
       , Tree(..), Forest
         -- * Overloaded tree lenses
       , TreeLens(..)
         -- *Retrieve directory trees from the filesystem
       , getDir, getDir'
       , getDirectory, getDirectory'
       , Options(..), defaultOptions
         -- * Operations on directory trees
         -- **basic operations
       , pop, pop_, flatten
         -- **find subtrees
       , find, findM
         -- **filter subtrees
       , filter, filterM
         -- **extract subtrees
       , extract, extractM
         -- **truncate tree to a maximum level
       , truncateAt
       )where

import System.IO.Unsafe (unsafeInterleaveIO)
import Unsafe.Coerce (unsafeCoerce)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink)
import Data.Tree (Tree(..), Forest)
import qualified Data.Tree as Tree (flatten)
import Data.DList as DL (DList(..), cons, append, toList, empty)
import Data.Lens.Common (Lens, lens, getL, modL)

import Data.Foldable (foldrM)
import Control.Monad (forM, liftM)
import Control.Monad.Identity (runIdentity)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Maybe (mapMaybe)
import Control.Cond (ifM, (<||>), (<&&>), notM)

import Data.Default (Default(..))
import Data.Word (Word)
import Data.Typeable (Typeable)
import Data.Data (Data)

import Prelude hiding (filter)
import qualified Prelude as P (filter)

newtype FSTree = FSTree { toTree :: Tree FilePath } deriving 
                (Typeable, Data, Eq, Read, Show)

type FSForest = [FSTree]

mkFSTree :: FilePath -> FSForest -> FSTree
mkFSTree a = FSTree . Node a . mapToTree

unFSTree :: FSTree -> (FilePath, FSForest) 
unFSTree (FSTree (Node p cs)) = (p, mapFSTree cs) 

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
getDirectory = getDir defaultOptions

getDirectory' :: FilePath -> IO FSTree
getDirectory' = getDir' defaultOptions

getDir :: Options -> FilePath -> IO FSTree
getDir = getDir_ unsafeInterleaveIO

getDir' :: Options -> FilePath -> IO FSTree
getDir' = getDir_ id

data Options = Options { followSymLinks :: Bool } deriving (Eq, Show)

instance Default Options where
  def = defaultOptions

defaultOptions :: Options
defaultOptions = Options { followSymLinks = False }

getDir_ :: (IO FSTree -> IO FSTree) 
           -> Options
           -> FilePath
           -> IO FSTree
getDir_ f Options {..} p = mkFSTree p <$> getChildren p
  where getChildren path = do
          cs <- P.filter (`notElem` [".",".."]) 
                <$> getDirectoryContents path
          forM cs $ \c ->
            let c' = path </> c
            in ifM (doesDirectoryExist c' <&&> (return followSymLinks
                                                <||> notM (isSymLink c')))
                   ( f . fmap (mkFSTree c) . getChildren $ c' )
                   ( return $ mkFSTree c [] )

pop :: FSTree -> (FilePath, FSForest)
pop fs = (path, map prepend cs)
  where path = getL label fs
        cs = getL children fs
        prepend = modL label (path </>)

pop_ :: FSTree -> FSForest
pop_ = snd . pop

flatten :: FSTree -> [FilePath]
flatten = Tree.flatten . prependPaths 

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
prependPaths (FSTree t) = modL children (map (prepend' root)) t
  where
    root = rootLabel t
    prepend' parentPath = modL label (parentPath </>) . prependChildren
    prependChildren fs = modL children (map (prepend' (rootLabel fs))) fs

isSymLink :: FilePath -> IO Bool
isSymLink p = isSymbolicLink <$> getSymbolicLinkStatus p
