module System.Directory.Tree where

import System.Directory
import System.FilePath
import Data.Tree

import Control.Monad
import Control.Applicative

getDirectory :: FilePath -> IO (Tree FilePath)
getDirectory path = Node path <$> getChildren
  where getChildren = do
          children <- map (path </>) . filter (`notElem` [".",".."]) 
                      <$> getDirectoryContents path
          forM children $ \c -> do
            b <- doesDirectoryExist $ c
            if b 
              then getDirectory c
              else return $ Node c []