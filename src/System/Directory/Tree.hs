{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module System.Directory.Tree 
       (Options(..), defaultOptions
       ,getDir, getDir'
       ,getDirectory, getDirectory'
       ,filterPaths, extractPaths
       )where

import System.IO.Unsafe (unsafeInterleaveIO)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink)
import Data.Tree (Tree(..), Forest)
import Data.DList as DL (DList(..), cons, append, toList, empty)
import Data.String (IsString, fromString)
import Data.Default

import Control.Monad
import Control.Applicative
import Control.Arrow (second)
import Control.Cond


data Options = Options { followSymLinks :: Bool } deriving (Eq, Show)

instance Default Options where
  def = Options { followSymLinks = True }

defaultOptions :: Options
defaultOptions = def

getDirectory :: FilePath -> IO (Tree FilePath)
getDirectory = getDir defaultOptions

getDirectory' :: FilePath -> IO (Tree FilePath)
getDirectory' = getDir' defaultOptions

filterPaths :: (FilePath -> Bool) -> Forest FilePath -> Forest FilePath
filterPaths p = fst . extractPaths p


extractPaths :: (FilePath -> Bool) 
                -> Forest FilePath -> (Forest FilePath, Forest FilePath)
extractPaths p = second toList . extractPaths_ p

extractPaths_ :: (FilePath -> Bool) 
                -> Forest FilePath -> (Forest FilePath, DList (Tree FilePath))
extractPaths_ p = foldr extract ([], DL.empty)
  where
    extract t@(Node path children) (ts, es)
      | p path = let (children', es') = extractPaths_ p children 
                 in (Node path children' : ts, es' `append` es)
      | otherwise = (ts, t `cons` es)

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
              ( f . _getDir f o . fromString $ c )
              ( return $ Node c [] )
                       
isSymLink :: FilePath -> IO Bool
isSymLink p = isSymbolicLink <$> getSymbolicLinkStatus p