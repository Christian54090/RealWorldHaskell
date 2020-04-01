module NiceFork where

import Control.Concurrent
import Control.Exception (Exception, try)
import qualified Data.Map as M

newtype ThreadManager =
  Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
  deriving Eq

data ThreadStatus = Running
                  | Finished        -- terminated normally
                  | Threw Exception -- killed by uncaught exception
                  deriving (Eq, Show)

-- | create a new thread manager
newManager :: IO ThreadManager
newManager = Mgr <$> newMVar M.empty

-- | create a new managed thread
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
  modifyMVar mgr $ \m -> do
    state <- newEmptyMVar
    tid <- forkIO $ do
      result <- try body
      putMVar state (either Threw (const Finished) result)
    return (M.insert tid state m, tid)

-- | immediately return the status of a managed thread
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m,Nothing)
      Just st -> tryTakeMVar st >>= \mst -> case mst of
                   Nothing -> return (m, Just Running)
                   Just sth -> return (M.delete tid m, Just sth)

-- | block until a specific managed thread terminates
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
  maybeDone <- modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing,_) -> (m,Nothing)
      (done,m')   -> (m', done)
  case maybeDone of
    Nothing -> return Nothing
    Just st -> Just <$> takeMVar st

-- | block until all managed threads terminate
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
  where elems m = return (M.empty, M.elems m)