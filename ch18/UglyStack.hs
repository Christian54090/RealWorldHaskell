import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig { cfgMaxDepth :: Int } deriving (Show)

data AppState = AppState { stDeepestReached :: Int } deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in runStateT (runReaderT k config) state

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
              then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                  put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  return $ (path, length contents) : concat rest

-- HIDING OUR WORK --

newtype MyApp a = MyA {
    runA :: ReaderT AppConfig (StateT AppState IO) a
  } deriving (Monad, MonadIO, MonadReader, AppConfig, MonadState, AppState)

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in runStateT (runReaderT (runA k) config) state

-- if we export the MyApp type constructor and the runMyApp execution function
-- from a module, client code will not be able to tell that the internals
-- of our monad is a stack of monad transformers

-- large deriving clause requires the GeneralizedNewtypeDeriving pragma
