module FileSystem where

import Relude
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)

recursivelyDeleteDirectory :: (MonadIO m) => FilePath -> m ()
recursivelyDeleteDirectory path = do
  doesExist <- liftIO $ doesDirectoryExist path
  when doesExist $ liftIO $ removeDirectoryRecursive path
