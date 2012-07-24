module Util where

import Prelude hiding (FilePath, catch)

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad (when)
import Data.Unique.Id
import Filesystem.Path.CurrentOS
import System.Directory
import System.IO.Error (isPermissionError)
import System.IO.FSNotify
import System.IO.FSNotify.Types
import System.Random

data ChanActionEnv =
    ChanEnv
  | ActionEnv
data DirTreeEnv =
    DirEnv
  | TreeEnv
data TestContext = TestContext ChanActionEnv DirTreeEnv ActionPredicate

type TestAction = FilePath -> IO ()

testName :: IO String
testName = do
  randomIO >>= initIdSupply >>= return . ("sandbox-" ++) . show . hashedId . idFromSupply

withTempDir :: (String -> IO a) -> IO a
withTempDir fn = do
  path <- testName
  bracket (createDirectory path >> return path) attemptDirectoryRemoval fn
  where
    attemptDirectoryRemoval :: String -> IO ()
    attemptDirectoryRemoval path = catch
        (removeDirectoryRecursive path)
        (\e -> when
               (not $ isPermissionError e)
               (throw e))

performAction :: TestAction -> FilePath -> IO ()
performAction action path = action path

act :: TestAction -> FilePath -> IO ()
act action path = do
  performAction action path

inEnv :: ChanActionEnv -> DirTreeEnv -> ActionPredicate -> TestAction -> IO EventChannel
inEnv caEnv dtEnv reportPred action = withTempDir $ \pathString ->
  withManager $ \manager -> do
    chan <- newChan
    let path = decodeString pathString
    watchInEnv caEnv dtEnv manager path reportPred chan
    act action path
    return chan

actionAsChan :: (WatchManager -> FilePath -> ActionPredicate -> Action       -> IO ()) ->
                 WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO ()
actionAsChan actionFunction wm fp ap ec = actionFunction wm fp ap (writeChan ec)


watchInEnv :: ChanActionEnv -> DirTreeEnv -> WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO ()
watchInEnv ChanEnv   DirEnv  = watchDirChan
watchInEnv ChanEnv   TreeEnv = watchTreeChan
watchInEnv ActionEnv DirEnv  = actionAsChan watchDirAction
watchInEnv ActionEnv TreeEnv = actionAsChan watchTreeAction
