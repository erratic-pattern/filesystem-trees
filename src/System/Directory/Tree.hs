{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Directory.Tree 
       (Directory, getDir 
       )where

import System.IO.Unsafe (unsafeInterleaveIO)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Data.Tree (Tree(..))
import Data.String (IsString, fromString)

import Control.Monad
import Control.Applicative

newtype Directory = Directory FilePath deriving (Eq, Ord, Show, Read, IsString)

getFilePath :: Directory -> FilePath
getFilePath (Directory p) = p

getDir :: Directory -> IO (Tree FilePath)
getDir (Directory path) = Node path <$> getChildren
  where getChildren = do
          children <- map (path </>) . filter (`notElem` [".",".."]) 
                      <$> getDirectoryContents path
          forM children $ \c -> do
            p <- doesDirectoryExist c
            if p 
              then unsafeInterleaveIO . getDir . fromString $ c
              else return $ Node c []