{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module System.Directory.Tree
       ( -- *Tree structure
         -- |Re-exported from "Data.Tree"
         Tree(..), Forest
         -- *Retrieve directory trees from filesystem
       , getDir, getDir'
       , getDirectory, getDirectory'
       , Options(..), defaultOptions
         -- * Operations on directory trees
         -- **basic operations
       , pop, pop_, flatten
         -- **find subtrees
       , findPaths, findPathsM
         -- **filter subtrees
       , filterPaths, filterPathsM
         -- **extract subtrees
       , extractPaths, extractPathsM
         -- **truncate tree to a maximum level
       , truncateAt
       )where

import System.IO.Unsafe (unsafeInterleaveIO)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink)
import Data.Tree (Tree(..), Forest)
import qualified Data.Tree as Tree (flatten)
import Data.DList as DL (DList(..), cons, append, toList, empty)

import Data.Foldable (foldrM)
import Control.Monad (forM, liftM)
import Control.Monad.Identity (runIdentity)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Maybe (mapMaybe)
import Control.Cond (ifM, (<||>), (<&&>))

import Data.Default (Default(..))
import Data.Word (Word)

data Options = Options { followSymLinks :: Bool } deriving (Eq, Show)

instance Default Options where
  def = Options { followSymLinks = False }

defaultOptions :: Options
defaultOptions = def

getDirectory :: FilePath -> IO (Tree FilePath)
getDirectory = getDir def

getDirectory' :: FilePath -> IO (Tree FilePath)
getDirectory' = getDir' def

getDir :: Options -> FilePath -> IO (Tree FilePath)
getDir = getDir_ unsafeInterleaveIO

getDir' :: Options -> FilePath -> IO (Tree FilePath)
getDir' = getDir_ id

getDir_ :: (IO (Tree FilePath) -> IO (Tree FilePath)) 
           -> Options
           -> FilePath
           -> IO (Tree FilePath)
getDir_ f o@Options {..} path = Node path <$> getChildren
  where getChildren = do
          children <- map (path </>) . filter (`notElem` [".",".."]) 
                      <$> getDirectoryContents path
          forM children $ \c ->
            ifM (doesDirectoryExist c <&&> (return followSymLinks
                                            <||> (not <$> isSymLink c)))
              ( f . getDir_ f o $ c )
              ( return $ Node c [] )


pop :: Tree FilePath -> (FilePath, Forest FilePath)
pop (Node path children) = (path, map prepend children)
  where prepend (Node p c) = Node (path </> p) c

pop_ :: Tree FilePath -> Forest FilePath
pop_ = snd . pop

flatten :: Tree FilePath -> [FilePath]
flatten = Tree.flatten . prependPaths 

filterPaths :: (FilePath -> Bool) -> Forest FilePath -> Forest FilePath
filterPaths p = fst . extractPaths p


findPaths :: (FilePath -> Bool) -> Forest FilePath -> Forest FilePath
findPaths p = snd . extractPaths p


extractPaths :: (FilePath -> Bool) -> Forest FilePath 
                -> (Forest FilePath, Forest FilePath)
extractPaths p = runIdentity . extractPathsM (return . p)

filterPathsM :: Monad m =>
                (FilePath -> m Bool) -> Forest FilePath
                -> m (Forest FilePath)
filterPathsM p = liftM fst . extractPathsM p

findPathsM :: Monad m =>
              (FilePath -> m Bool) -> Forest FilePath 
              -> m (Forest FilePath)
findPathsM p = liftM snd . extractPathsM p

extractPathsM :: Monad m => 
                 (FilePath -> m Bool) -> Forest FilePath  
                 -> m (Forest FilePath, Forest FilePath)
extractPathsM p = liftM (second toList) . extractPathsM_ p

extractPathsM_ :: Monad m => 
                  (FilePath -> m Bool) -> Forest FilePath 
                  -> m (Forest FilePath, DList (Tree FilePath))
extractPathsM_ p = foldrM extract ([], DL.empty) . map prependPaths
  where 
    extract t@(Node path children) (ts, es)
      = ifM (p path)
        ( do
             (children', es') <- foldrM extract (ts, es) children
             let t' = Node path children'
             return (t' : ts, es' `append` es)
        )
        (
          return (ts, t `cons` es)
        )

truncateAt :: Word -> Forest FilePath -> Forest FilePath
truncateAt n = mapMaybe (truncate' 0)
  where 
    truncate' i (Node p children)
      | i >= n = Nothing
      | otherwise = Just . Node p . mapMaybe (truncate' (i+1)) $ children


prependPaths :: Tree FilePath -> Tree FilePath
prependPaths (Node root childs) = Node root (map (prepend' root) childs)
  where 
    prepend' parent (Node p c) = Node p' $ map (prepend' p') c
      where p' = parent </> p

isSymLink :: FilePath -> IO Bool
isSymLink p = isSymbolicLink <$> getSymbolicLinkStatus p