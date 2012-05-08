import System.Directory
import System.Directory.Tree
import Data.Tree
import Data.String
import Data.Foldable



main = do 
  home <- getHomeDirectory 
  print =<< getDirectory (fromString home)