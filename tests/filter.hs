import System.File.Tree as FS
import Control.Applicative
import System.Directory
import Data.List

main = do  
  d <- getDirectory =<< getHomeDirectory
  print . FS.filter (".mozilla/" `isInfixOf`) . (:[]) $ d

