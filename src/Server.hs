--{-# LANGUAGE DataKinds                  #-}
--{-# LANGUAGE DeriveGeneric              #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE OverloadedStrings          #-}
--{-# LANGUAGE TypeOperators              #-}
--
--module Server
--    ( webAppEntry
--    ) where
--
--import           Control.Concurrent.STM
--import           Control.Monad.IO.Class
--import           Data.Aeson
--import           Data.Proxy
--import           Data.Text
--import           GHC.Generics
--import           Network.Wai.Handler.Warp (run)
--import           Servant
--import           Servant.JS
--import qualified Servant.JS               as SJS
--import           System.FilePath
--import           Debug.Trace
--import           Data.IORef (newIORef, readIORef, writeIORef)
--
--newtype UserPul = UserPul {users :: [User]} deriving (Show, Generic)
--
--instance ToJSON UserPul
--instance FromJSON UserPul
--
--newtype State = State { value :: Int } deriving (Generic, Show, Num)
--
--newtype User = User { name :: Text } deriving (Generic,  Show)
--
--instance ToJSON State
--instance ToJSON User
--instance FromJSON User
--
--newState :: IO (TVar State)
--newState = newTVarIO 0
--
--newUser :: IO (TVar User)
--newUser = newTVarIO User { name = "username" }
--
--newUserPul :: IO (TVar UserPul)
--newUserPul = newTVarIO UserPul { users = []}
--
--updateState :: MonadIO m => TVar State -> m State
--updateState state = liftIO . atomically $ do
--  oldState <- readTVar state
--  let newState = traceShow oldState (oldState + 1)
--  writeTVar state newState
--  return newState
--
----updateUser :: MonadIO m => TVar User -> m User
----updateUser user = liftIO . atomically $ do
----  oldUser <- readTVar user
----  let newUser = traceShow oldUser oldUser
----  writeTVar user newUser
----  return newUser
--
--updateUser :: MonadIO m => TVar UserPul -> User -> m User
--updateUser userPul u = liftIO . atomically $ do
--  oldUser <- readTVar userPul
--  let newUser = traceShow u u
--  writeTVar userPul userPul
--  return newUser
--
--currentState :: MonadIO m => TVar State -> m State
--currentState state = liftIO $ readTVarIO state
--
--currentUser :: MonadIO m => TVar User -> m User
--currentUser user = liftIO $ readTVarIO user
--
--type StateApi = "state" :> Post '[JSON] State
--           :<|> "state" :> Get '[JSON] State
--
--type UserApi = "user" :> ReqBody '[JSON] User :> Post '[JSON] User
--          :<|> "user" :> Get '[JSON] User
--
--type ServerApi = StateApi :<|> UserApi :<|> Raw
--
--userApi :: Proxy UserApi
--userApi = Proxy
--
--stateApi :: Proxy StateApi
--stateApi = Proxy
--
--serverApi :: Proxy ServerApi
--serverApi = Proxy
--
--staticPath :: FilePath
--staticPath = "static"
--
--stateServer :: TVar State -> Server StateApi
--stateServer state = updateState state :<|> currentState state
--
----userServer :: TVar User -> Server UserApi
----userServer user = update user :<|> current user
----  where update user u = do
----            writeTVar user u
----            traceShow u (return u)
----        current user = return readTVarIO user
--
--userServer :: TVar UserPul -> Server UserApi
--userServer userPul = updateUser userPul :<|> currentUser userPul
--
--server :: TVar State -> TVar UserPul -> Server ServerApi
--server state userPul = stateServer state :<|> userServer userPul :<|> serveDirectoryFileServer staticPath
--
--runServer :: TVar State -> TVar UserPul -> Int -> IO ()
--runServer state newUserPul port = run port (serve serverApi $ server state UserPul)
--
--webAppEntry :: IO ()
--webAppEntry = do
--  writeJSForAPI stateApi jquery (staticPath </> "stateApi.js")
--  writeJSForAPI userApi jquery (staticPath </> "userApi.js")
--
--  state <- newState
--  userList <- newUserPul
--
--  runServer state newUserPul 8080
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
import           Data.IORef               (newIORef, readIORef, writeIORef)
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.Text
import           Data.UUID
import           Data.UUID.V1
import           Debug.Trace
import           GHC.Generics
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.JS
import qualified Servant.JS               as SJS
import           System.FilePath

newtype UserPul =
  UserPul
    { users :: [User]
    }
  deriving (Show, Generic)

data Players =
  Players
    { firstPlayerName  :: Text
    , secondPlayerName :: Text
    }
  deriving (Generic, Show, Eq)

instance ToJSON UserPul

instance FromJSON UserPul

instance ToJSON GamePul

instance ToJSON Players
instance FromJSON Players

newUserPul :: IO (TVar UserPul)
newUserPul = newTVarIO UserPul {users = []}

data Game =
  Game
    { pk      :: Text
    , board   :: Text
    , firstPlayer :: User
    , secondPlayer :: User
    }
  deriving (Generic, Show, Eq)

instance ToJSON Game

instance FromJSON Game

newtype GamePul =
  GamePul
    { games :: [Game]
    }
  deriving (Show, Generic)

newGamePul :: IO (TVar GamePul)
newGamePul = newTVarIO GamePul {games = []}

newtype State =
  State
    { value :: Int
    }
  deriving (Generic, Show, Num)

newtype User =
  User
    { name :: Text
    }
  deriving (Generic, Show, Eq)

instance ToJSON State

instance ToJSON User

instance FromJSON User

newState :: IO (TVar State)
newState = newTVarIO 0

newUser :: IO (TVar User)
newUser = newTVarIO User {name = "username"}

updateState :: MonadIO m => TVar State -> m State
updateState state =
  liftIO . atomically $ do
    oldState <- readTVar state
    let newState = traceShow oldState (oldState + 1)
    writeTVar state newState
    return newState

--updateUser :: MonadIO m => TVar UserPul -> TVar GamePul -> User -> m UserPul
--updateUser userPul gamePul u =
--  liftIO . atomically $ do
--    oldUserPul <- readTVar userPul
--    if not (Data.List.null (users oldUserPul))
--      then do
--        let u1 = traceShow ("then", oldUserPul) Data.List.head (users oldUserPul)
--        let newUserPul = traceShow ("then", oldUserPul) UserPul {users = []}
--        writeTVar userPul newUserPul
--        oldGamePul <- traceShow ("then", oldUserPul) readTVar gamePul
--        let oldGame = traceShow ("then", oldUserPul) Data.List.head (Data.List.filter (\e -> pk e == "") (games oldGamePul))
--        let gms = traceShow ("then", oldUserPul) Data.List.delete oldGame (games oldGamePul)
--        let g = traceShow ("then", oldUserPul) connectGame u oldGame
--        writeTVar gamePul GamePul {games = g : gms}
--        return g
--      else do
--        let newUserPul = traceShow ("else", oldUserPul) (UserPul {users = u : users oldUserPul})
--        writeTVar userPul newUserPul
--        t <- readTVar userPul
--        oldGamePul <- readTVar gamePul
--        let g = initGame u
--        writeTVar gamePul GamePul {games = g : games oldGamePul}
--        return g
updateUser :: MonadIO m => TVar UserPul -> TVar GamePul -> Text -> m UserPul
updateUser userPul gamePul username =
  liftIO . atomically $ do
    oldUserPul <- readTVar userPul
    let user = User {name = username}
    let newUserPul = UserPul {users = user : users oldUserPul}
    writeTVar userPul newUserPul
    return newUserPul

--connectGame :: User -> Game -> Game
--connectGame u2 g = traceShow ("connectGame", u2, g) Game {pk = p, board = b, players = plrs}
--  where
--    p = Data.Text.concat [name (firstPlayer g), name u2]
--    u1 = firstPlayer g
--    b = "board"

initGameTwise :: User -> User -> Game
initGameTwise u1 u2 = Game {pk = p, board = b, firstPlayer = u1, secondPlayer = u2}
  where
    p = Data.Text.concat [name u1, name u2]
    b = "board"

play :: MonadIO m => TVar UserPul -> TVar GamePul -> Players -> m Game
play userPul gamePul players = traceShow ("play")
  liftIO . atomically $ do
    oldUserPul <- readTVar userPul
    --let u1 = Data.List.head (Data.List.filter (\e -> name e == firstPlayerName players) (users oldUserPul))
    let u1 = traceShow ("play", u1) fromJust (Data.List.find (\e -> name e == firstPlayerName players) (users oldUserPul))
    let u2 = traceShow ("play", u2) fromJust (Data.List.find (\e -> name e == secondPlayerName players) (users oldUserPul))
    oldGamePul <- readTVar gamePul
    let g = initGameTwise u1 u2
    writeTVar gamePul GamePul {games = g : games oldGamePul}
    let newUsers = traceShow ("play", u1, u2, g) Data.List.delete u2 (Data.List.delete u1 (users oldUserPul))
    let newUserPul = UserPul {users = newUsers}
    writeTVar userPul newUserPul
    return g

--initGame :: User -> Game
--initGame u = traceShow ("initGame", u) Game {pk = "", firstPlayer = u}

currentState :: MonadIO m => TVar State -> m State
currentState state = liftIO $ readTVarIO state

currentUser :: MonadIO m => TVar UserPul -> TVar GamePul -> m UserPul
currentUser userPul gamePul = liftIO $ readTVarIO userPul

type StateApi = "state" :> Post '[ JSON] State :<|> "state" :> Get '[ JSON] State

type UserApi
   = "user" :> ReqBody '[ JSON] Text :> Post '[ JSON] UserPul :<|> "user" :> Get '[ JSON] UserPul :<|> "play" :> ReqBody '[ JSON] Players :> Post '[ JSON] Game

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

userServer :: TVar UserPul -> TVar GamePul -> Server UserApi
userServer userPul gamePul = updateUser userPul gamePul :<|> currentUser userPul gamePul :<|> play userPul gamePul

server :: TVar State -> TVar UserPul -> TVar GamePul -> Server ServerApi
server state userPul gamePul =
  stateServer state :<|> userServer userPul gamePul :<|> serveDirectoryFileServer staticPath

runServer :: TVar State -> TVar UserPul -> TVar GamePul -> Int -> IO ()
runServer state userPul gamePul port = run port (serve serverApi $ server state userPul gamePul)

webAppEntry :: IO ()
webAppEntry = do
  writeJSForAPI stateApi jquery (staticPath </> "stateApi.js")
  writeJSForAPI userApi jquery (staticPath </> "userApi.js")
  state <- newState
  userPul <- newUserPul
  gamePul <- newGamePul
  runServer state userPul gamePul 8080
