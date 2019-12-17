{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Server
    ( webAppEntry
    ) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.JS
import qualified Servant.JS               as SJS
import           System.FilePath
import           Debug.Trace
import           Data.IORef (newIORef, readIORef, writeIORef)



newtype State = State { value :: Int } deriving (Generic, Show, Num)

newtype User = User { name :: Text } deriving (Generic,  Show)

instance ToJSON State
instance ToJSON User
instance FromJSON User

newState :: IO (TVar State)
newState = newTVarIO 0

newUser :: IO (TVar User)
newUser = newTVarIO User { name = "username" }

updateState :: MonadIO m => TVar State -> m State
updateState state = liftIO . atomically $ do
  oldState <- readTVar state
  let newState = traceShow oldState (oldState + 1)
  writeTVar state newState
  return newState
  
--updateUser :: MonadIO m => TVar User -> m User
--updateUser user = liftIO . atomically $ do
--  oldUser <- readTVar user
--  let newUser = traceShow oldUser oldUser
--  writeTVar user newUser
--  return newUser

updateUser :: MonadIO m => TVar User -> User -> m User
updateUser user u = liftIO . atomically $ do
  oldUser <- readTVar user
  let newUser = traceShow u u
  writeTVar user newUser
  return newUser

currentState :: MonadIO m => TVar State -> m State
currentState state = liftIO $ readTVarIO state

currentUser :: MonadIO m => TVar User -> m User
currentUser user = liftIO $ readTVarIO user

type StateApi = "state" :> Post '[JSON] State
           :<|> "state" :> Get '[JSON] State
          
type UserApi = "user" :> ReqBody '[JSON] User :> Post '[JSON] User
          :<|> "user" :> Get '[JSON] User

type ServerApi = StateApi :<|> UserApi :<|> Raw

userApi :: Proxy UserApi
userApi = Proxy

stateApi :: Proxy StateApi
stateApi = Proxy

serverApi :: Proxy ServerApi
serverApi = Proxy

staticPath :: FilePath
staticPath = "static"

stateServer :: TVar State -> Server StateApi
stateServer state = updateState state :<|> currentState state

--userServer :: TVar User -> Server UserApi
--userServer user = update user :<|> current user
--  where update user u = do
--            writeTVar user u
--            traceShow u (return u)
--        current user = return readTVarIO user

userServer :: TVar User -> Server UserApi
userServer user = updateUser user :<|> currentUser user

server :: TVar State -> TVar User -> Server ServerApi
server state user = stateServer state :<|> userServer user :<|> serveDirectoryFileServer staticPath

runServer :: TVar State -> TVar User -> Int -> IO ()
runServer state user port = run port (serve serverApi $ server state user)

webAppEntry :: IO ()
webAppEntry = do
  writeJSForAPI stateApi jquery (staticPath </> "stateApi.js")
  writeJSForAPI userApi jquery (staticPath </> "userApi.js")

  state <- newState
  user <- newUser

  runServer state user 8080
