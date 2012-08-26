{- BSD kqueue file modification notification interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Re-Licensed under BSD3
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module System.IO.FSNotify.Kqueue (
    Kqueue,
    initKqueue,
    stopKqueue,
    -- waitChange,
    Change(..),
    changedFile,
    isAdd,
    isDelete,
    -- runHooks,
) where

-- import Common
-- import Utility.Types.DirWatcher

import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)
import System.Posix.Types
import System.Posix.IO
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Marshal
-- import qualified Data.Map as M
-- import qualified Data.Set as S
-- import qualified System.Posix.Files as Files
import Control.Concurrent
import System.IO.FSNotify.Listener

data Change
    = Deleted FilePath
    | Added FilePath
    deriving (Show)

isAdd :: Change -> Bool
isAdd (Added _) = True
isAdd (Deleted _) = False

isDelete :: Change -> Bool
isDelete = not . isAdd

changedFile :: Change -> FilePath
changedFile (Added f) = f
changedFile (Deleted f) = f

-- TODO: close fds
data Kqueue = Kqueue { kqueueFd :: Fd } -- , kqueueOpenFds :: [FD]
    -- , kqueueMap :: DirMap
    -- , _kqueuePruner :: Pruner

type Pruner = FilePath -> Bool



foreign import ccall unsafe "libkqueue.h init_kqueue" c_init_kqueue
    :: IO Fd
foreign import ccall unsafe "libkqueue.h addfds_kqueue" c_addfds_kqueue
    :: Fd -> CInt -> Ptr Fd -> IO ()
foreign import ccall unsafe "libkqueue.h waitchange_kqueue" c_waitchange_kqueue
    :: Fd -> IO Fd

instance FileListener Kqueue where
  initSession = fmap Just initKqueue
  killSession = stopKqueue

  listen kqueue path actPred chan = do
    fd <- openFd (encodeString path) ReadOnly Nothing defaultFileFlags
    withArrayLen [fd] $ \fdcnt c_fds ->
        c_addfds_kqueue (kqueueFd kqueue) (fromIntegral fdcnt) c_fds
    return ()

{- is this the default def?
  rlisten kqueue path actPred chan = do
    path' <- canonicalizePath path
    paths <- findDirs True path'
    mapM_ (\fp -> listen kqueue fp actPred chan) paths
    -}


initKqueue :: IO Kqueue
initKqueue = c_init_kqueue >>= return . Kqueue

{- Stops a Kqueue. Note: Does not directly close the Fds in the dirmap,
 - so it can be reused.  -}
-- TODO: close all open Fds
stopKqueue :: Kqueue -> IO ()
stopKqueue = closeFd . kqueueFd

{- The kqueue interface does not tell what type of change took place in
 - the directory; it could be an added file, a deleted file, a renamed
 - file, a new subdirectory, or a deleted subdirectory, or a moved
 - subdirectory. 
 -
 - So to determine this, the contents of the directory are compared
 - with its last cached contents. The Kqueue is updated to watch new
 - directories as necessary.
 -}
    {-
handleChange :: Kqueue -> Fd -> DirInfo -> IO (Kqueue, [Change])
handleChange kq@(Kqueue _) fd olddirinfo =
    return (kq, [])
    go =<< catchMaybeIO (getDirInfo $ dirName olddirinfo)
    where
        go (Just newdirinfo) = do
            let changes = olddirinfo // newdirinfo
            let (added, deleted) = partition isAdd changes

            -- Scan newly added directories to add to the map.
            -- (Newly added files will fail getDirInfo.)
            newdirinfos <- catMaybes <$>
                mapM (catchMaybeIO . getDirInfo . changedFile) added
            newmap <- addSubDirs dirmap pruner $ map dirName newdirinfos

            -- Remove deleted directories from the map.
            newmap' <- foldM removeSubDir newmap (map changedFile deleted)

            -- Update the cached dirinfo just looked up.
            let newmap'' = M.insertWith' const fd newdirinfo newmap'

            -- When new directories were added, need to update
            -- the kqueue to watch them.
            let kq' = kq { kqueueMap = newmap'' }
            unless (null newdirinfos) $
                updateKqueue kq'

            return (kq', changes)
        go Nothing = do
            -- The directory has been moved or deleted, so
            -- remove it from our map.
            newmap <- removeSubDir dirmap (dirName olddirinfo)
            return (kq { kqueueMap = newmap }, [])
            -}

{- Processes changes on the Kqueue, calling the hooks as appropriate.
 - Never returns. -}
{-
runHooks :: Kqueue -> WatchHooks -> IO ()
runHooks kq hooks = return ()
    -- First, synthetic add events for the whole directory tree contents,
    -- to catch any files created beforehand.
    recursiveadd (kqueueMap kq) (Added $ kqueueTop kq)
    loop kq
    where
        loop q = do
            (q', changes) <- waitChange q
            forM_ changes $ dispatch (kqueueMap q')
            loop q'
        -- Kqueue returns changes for both whole directories
        -- being added and deleted, and individual files being
        -- added and deleted.
        dispatch dirmap change
            | isAdd change = withstatus change $ dispatchadd dirmap
            | otherwise = callhook delDirHook Nothing change
        dispatchadd dirmap change s
            | Files.isSymbolicLink s =
                callhook addSymlinkHook (Just s) change
            | Files.isDirectory s = recursiveadd dirmap change
            | Files.isRegularFile s =
                callhook addHook (Just s) change
            | otherwise = noop
        recursiveadd dirmap change = do
            let contents = findDirContents dirmap $ changedFile change
            forM_ contents $ \f ->
                withstatus (Added f) $ dispatchadd dirmap
        callhook h s change = case h hooks of
            Nothing -> noop
            Just a -> a (changedFile change) s
        withstatus change a = maybe noop (a change) =<<
            (catchMaybeIO (getSymbolicLinkStatus (changedFile change)))
                -}
