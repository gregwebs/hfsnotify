{-# LANGUAGE OverloadedStrings #-}
module Basic (spec) where

import Prelude hiding (FilePath, writeFile)

import Control.Concurrent.Chan
import Data.ByteString (empty)
import Filesystem
import Filesystem.Path hiding (empty)
import Filesystem.Path.CurrentOS hiding (empty)
import System.IO.FSNotify.Types
import Util
import System.Exit

data EventCounter = EventCounter {
    addedCount   :: Int
  , removedCount :: Int
  }

newCounter :: EventCounter
newCounter = EventCounter 0 0

testFileName :: FilePath
testFileName = decodeString "test.txt"

testFile :: FilePath -> FilePath
testFile path = (path </> testFileName)

write :: FilePath -> IO ()
write path = writeFile (testFile path) empty

delete :: FilePath -> IO ()
delete path = removeFile (testFile path)

action :: FilePath -> IO ()
action path = do
  write path
  delete path

expectation :: String
expectation = "1 Added, 1 Removed event in stream"

data TestReport = TestReport FilePath [Event] deriving (Show)
data TestResult = TestResult Bool String TestReport deriving (Show)
type EventProcessor = TestReport -> IO TestResult

verify :: EventProcessor
verify report@(TestReport _ events) =
  if addedCount counter == 1 && removedCount counter == 1 then
    return (TestResult True ("Found " ++ expectation) report)
    else
    return (TestResult False ("Expected " ++ expectation) report)
  where
    counter = countEvents report

countEvents :: TestReport -> EventCounter
countEvents = countEvents' newCounter

countEvents' :: EventCounter -> TestReport -> EventCounter
countEvents' (EventCounter added removed) (TestReport path ((Added eventPath):events))
  | eventPath == testFileName = countEvents' (EventCounter (added + 1)  removed) (TestReport path events)
  | otherwise                 = countEvents' (EventCounter  added       removed) (TestReport path events)
countEvents' (EventCounter added removed) (TestReport path ((Removed eventPath):events))
  | eventPath == testFileName = countEvents' (EventCounter  added (removed + 1)) (TestReport path events)
  | otherwise                 = countEvents' (EventCounter  added  removed     ) (TestReport path events)
countEvents' (EventCounter added removed) (TestReport path (_:events)) =
                                countEvents' (EventCounter  added  removed     ) (TestReport path events)
countEvents' counter _ = counter

spec :: IO ()
spec = do
  chan <- inEnv ActionEnv DirEnv alwaysAct action
  reportOnAction "FIXME" chan verify >>= explainResult >>= exitStatus

reportOnAction :: FilePath -> EventChannel -> EventProcessor -> IO TestResult
reportOnAction = reportOnAction' []

reportOnAction' :: [Event] -> FilePath -> EventChannel -> EventProcessor -> IO TestResult
reportOnAction' events path chan processor = do
  result@(TestResult status _ _) <- processor (TestReport path events)
  if status then return result else do
    event <- readChan chan
    reportOnAction' (event:events) path chan processor

exitStatus :: Bool -> IO ()
exitStatus True  = testSuccess
exitStatus False = testFailure


explainResult :: TestResult -> IO Bool
explainResult (TestResult status explanation report) = do
  putStrLn ""
  if status then
     putStrLn " :: TEST SUCCESS"
     else
     putStrLn " !! TEST FAILURE"
  putStrLn ""
  putStrLn explanation
  putStrLn $ "Test report: " ++ show report
  return status

testFailure :: IO ()
testFailure = exitFailure

testSuccess :: IO ()
testSuccess = exitWith ExitSuccess

