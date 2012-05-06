{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module System.Directory.Tree 
       (Options(..), defaultOptions
       ,getDir, getDir'
       ,getDirectory, getDirectory'
       ,filterPaths, extractPaths
       ,filterPathsM, extractPathsM
       )where

import System.IO.Unsafe (unsafeInterleaveIO)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink)
import Data.Tree (Tree(..), Forest)
import Data.DList as DL (DList(..), cons, append, toList, empty)

import Data.Foldable (foldrM)
import Control.Monad (forM, liftM)
import Control.Monad.Identity (runIdentity)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Cond (ifM, (<||>), (<&&>))
import Data.Default (Default(..))


data Options = Options { followSymLinks :: Bool } deriving (Eq, Show)

instance Default Options where
  def = Options { followSymLinks = True }

defaultOptions :: Options
defaultOptions = def

getDirectory :: FilePath -> IO (Tree FilePath)
getDirectory = getDir def

getDirectory' :: FilePath -> IO (Tree FilePath)
getDirectory' = getDir' def

getDir :: Options -> FilePath -> IO (Tree FilePath)
getDir = _getDir unsafeInterleaveIO

getDir' :: Options -> FilePath -> IO (Tree FilePath)
getDir' = _getDir id

_getDir :: (IO (Tree FilePath) -> IO (Tree FilePath)) 
           -> Options
           -> FilePath
           -> IO (Tree FilePath)
_getDir f o@Options {..} path = Node path <$> getChildren
  where getChildren = do
          children <- map (path </>) . filter (`notElem` [".",".."]) 
                      <$> getDirectoryContents path
          forM children $ \c ->
            ifM (doesDirectoryExist c <&&> (return followSymLinks
                                            <||> (not <$> isSymLink c)))
              ( f . _getDir f o $ c )
              ( return $ Node c [] )

filterPaths :: (FilePath -> Bool) -> Forest FilePath -> Forest FilePath
filterPaths p = fst . extractPaths p


extractPaths :: (FilePath -> Bool) 
                -> Forest FilePath -> (Forest FilePath, Forest FilePath)
extractPaths p = runIdentity . extractPathsM (return . p)

filterPathsM :: Monad m =>
                (FilePath -> m Bool)
                -> Forest FilePath
                -> m (Forest FilePath)
filterPathsM p = liftM fst . extractPathsM p

extractPathsM :: Monad m => 
                 (FilePath -> m Bool) 
                 -> Forest FilePath 
                 -> m (Forest FilePath, Forest FilePath)
extractPathsM p = liftM (second toList) . extractPathsM_ p

extractPathsM_ :: Monad m => 
                  (FilePath -> m Bool) 
                  -> Forest FilePath 
                  -> m (Forest FilePath, DList (Tree FilePath))
extractPathsM_ p = foldrM extract ([], DL.empty)
  where 
    extract t@(Node path children) (ts, es)
      = ifM (p path)
        ( do
             (children', es') <- extractPathsM_ p children
             let t' = Node path children'
             return (t' : ts, es' `append` es)
        )
        (
          return (ts, t `cons` es)
        )

isSymLink :: FilePath -> IO Bool
isSymLink p = isSymbolicLink <$> getSymbolicLinkStatus p