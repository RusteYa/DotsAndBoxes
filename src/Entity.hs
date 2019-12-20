{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entity where

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
import           Entity
import           Game.Logic
import           GHC.Generics
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.JS
import           System.FilePath

-- список пользвоателей
newtype UserPul =
  UserPul
    { users :: [User]
    }
  deriving (Show, Generic)

-- данные клиента для создания игры
data Players =
  Players
    { firstPlayerName  :: Text
    , secondPlayerName :: Text
    , size             :: Int
    }
  deriving (Generic, Show, Eq)

-- ход игры
data Move =
  Move
    { x      :: String
    , y      :: String
    , direct :: String
    , gamepk :: Text
    }
  deriving (Generic, Show, Eq)

-- полные данные об конкретной игре
data Game =
  Game
    { pk           :: Text
    , board        :: Board
    , boardS       :: String
    , availables   :: [Edge]
    , firstPlayer  :: User
    , secondPlayer :: User
    , playerTurn   :: Int
    }
  deriving (Generic, Show, Eq)

-- список игр
newtype GamePul =
  GamePul
    { games :: [Game]
    }
  deriving (Show, Generic)

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
