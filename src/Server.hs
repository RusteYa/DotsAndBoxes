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
import           Data.String.ToString
import           Data.Text
import           Debug.Trace
import           GHC.Generics
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.JS
import           System.FilePath
import           Game.Logic

newtype UserPul =
  UserPul
    { users :: [User]
    }
  deriving (Show, Generic)

data Players =
  Players
    { firstPlayerName  :: Text
    , secondPlayerName :: Text
    , size :: Int
    }
  deriving (Generic, Show, Eq)

data Move =
  Move
    { x      :: String
    , y      :: String
    , direct :: String
    , gamepk :: Text
    }
  deriving (Generic, Show, Eq)

instance ToJSON Move

instance FromJSON Move

instance ToJSON UserPul

instance FromJSON UserPul

instance ToJSON GamePul

instance ToJSON Players

instance FromJSON Players

newUserPul :: IO (TVar UserPul)
newUserPul = newTVarIO UserPul {users = []}

data Game =
  Game
    { pk           :: Text
    , board        :: Board
    , boardS        :: String
    , availables   :: [Edge]
    , firstPlayer  :: User
    , secondPlayer :: User
    , playerTurn :: Int
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

newGame :: Game
newGame = Game {pk = "#", board = buildBoard 1 1, availables = buildAvailables 1 1, firstPlayer = User {name = ""}, secondPlayer = User {name = ""}, playerTurn=1, boardS=""}

updateState :: MonadIO m => TVar State -> m State
updateState state =
  liftIO . atomically $ do
    oldState <- readTVar state
    let newState = traceShow oldState (oldState + 1)
    writeTVar state newState
    return newState

updateUser :: MonadIO m => TVar UserPul -> TVar GamePul -> Text -> m UserPul
updateUser userPul gamePul username =
  liftIO . atomically $ do
    oldUserPul <- readTVar userPul
    let user = User {name = username}
    let newUserPul = UserPul {users = user : users oldUserPul}
    writeTVar userPul newUserPul
    return newUserPul

initGameTwise :: Int -> User -> User -> Game
initGameTwise n u1 u2 = Game {pk = p, board = b, availables = a, firstPlayer = u1, secondPlayer = u2, playerTurn = 1, boardS=""}
  where
    p = name u2
    b = buildBoard n n
    a = buildAvailables n n

play :: MonadIO m => TVar UserPul -> TVar GamePul -> Players -> m Game
play userPul gamePul players =
  traceShow ("play") liftIO . atomically $ do
    let n = size players
    oldUserPul <- readTVar userPul
    let u1 =
          traceShow
            ("play", firstPlayerName players)
            fromJust
            (Data.List.find (\e -> name e == firstPlayerName players) (users oldUserPul))
    let u2 =
          traceShow
            ("play", firstPlayerName players)
            fromJust
            (Data.List.find (\e -> name e == secondPlayerName players) (users oldUserPul))
    oldGamePul <- readTVar gamePul
    let g = initGameTwise n u1 u2
    writeTVar gamePul GamePul {games = g : games oldGamePul}
    let newUsers = traceShow ("play", u1, u2, g) Data.List.delete u2 (Data.List.delete u1 (users oldUserPul))
    let newUserPul = UserPul {users = newUsers}
    writeTVar userPul newUserPul
    return g

game :: MonadIO m => TVar GamePul -> Text -> m Game
game gamePul username =
  traceShow "game" liftIO . atomically $ do
    oldGamePul <- readTVar gamePul
    let g = Data.List.find (\e -> pk e == username) (games oldGamePul)
    maybe (return newGame) return g

move :: MonadIO m => TVar GamePul -> Move -> m Game
move gamePul mv =
  traceShow "move" liftIO . atomically $ do
    oldGamePul <- readTVar gamePul
    let oldGame = fromJust (Data.List.find (\e -> pk e == gamepk mv) (games oldGamePul))
    let edge = normalizeEdge [x mv, y mv] (direct mv)
    let newBoard = updateBoard (board oldGame) edge (playerTurn oldGame)
    let newAvailableEdges = deleteAvailable edge (availables oldGame)
    let newPlayerTurn = if snd newBoard then playerTurn oldGame else changePlayer (playerTurn oldGame)
    let newGames = Data.List.delete oldGame (games oldGamePul)
    let newGame = oldGame {board = fst newBoard, availables = newAvailableEdges, playerTurn = newPlayerTurn, boardS = boardToString (fst newBoard)}
    let newGamePul = GamePul {games = newGame : newGames}
    writeTVar gamePul newGamePul
    return newGame

currentState :: MonadIO m => TVar State -> m State
currentState state = liftIO $ readTVarIO state

currentUser :: MonadIO m => TVar UserPul -> TVar GamePul -> m UserPul
currentUser userPul gamePul = liftIO $ readTVarIO userPul

currentBoard :: MonadIO m => TVar GamePul -> m Board
currentBoard gamePul = liftIO . atomically $ do
  oldGamePul <- readTVar gamePul
  let game = fromJust (Data.List.find (\e -> pk e == "rustem") (games oldGamePul))
  return (board game)

type StateApi = "state" :> Post '[ JSON] State :<|> "state" :> Get '[ JSON] State

type UserApi
   = "user" :> ReqBody '[ JSON] Text :> Post '[ JSON] UserPul
   :<|> "user" :> Get '[ JSON] UserPul
   :<|> "play" :> ReqBody '[ JSON] Players :> Post '[ JSON] Game
   :<|> "game" :> ReqBody '[ JSON] Text :> Post '[ JSON] Game
   :<|> "move" :> ReqBody '[ JSON] Move :> Post '[ JSON] Game
   :<|> "board" :> Get '[ JSON] Board

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
userServer userPul gamePul =
  updateUser userPul gamePul :<|> currentUser userPul gamePul :<|> play userPul gamePul :<|> game gamePul :<|>
  move gamePul :<|> currentBoard gamePul

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
