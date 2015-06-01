module Handler.Resource.FileManagement where

import Import
import System.Directory
import qualified Control.Exception as EXC
import qualified System.IO.Error as SIO

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    let destination = resourceDirectory </> unpack (fileName file)
    liftIO $ fileMove file destination
    return destination

deleteFile :: FilePath -> Handler ()
deleteFile fileName = liftIO $ removeFile fileName `EXC.catch` handleExists
  where handleExists e
          | SIO.isDoesNotExistError e = return ()
          | otherwise = EXC.throwIO e

