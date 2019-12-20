{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Game.Logic
import           GHC.Generics
import           LogicTest
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Servant.QuickCheck
import           Servant.Server
import           Server
import           System.IO
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.Tasty

instance Arbitrary Char where
  arbitrary = choose ('\32', '\128')

instance Arbitrary Players where
  arbitrary = do
    let p1 = "p1"
    let p2 = "p2"
    let s = 3
    return Players {firstPlayerName = p1, firstPlayerName = p2, size = s}

instance Arbitrary Move where
  arbitrary = do
    let x1 = "0"
    let y1 = "0"
    let dir = "v"
    let gmpk = "pk"
    return Move {x = x1, y1 = y, direct = dir, gamepk = gmpk}

instance Arbitrary Game where
  arbitrary = do
    let pk1 = "pk"
    let board1 = buildBoard 3 3
    let boardS1 = ""
    let availables1 = buildAvailables 3 3
    let firstPlayer1 = User { name="fp"}
    let secondPlayer1 = User { name="sp"}
    let playerTurn1 = 1
    return Game { pk=pk1, board=board1, boardS=boardsS1, availables=availables1, firstPlayer=firstPlayer1, secondPlayer=secondPlayer1, playerTurn=playerTurn1}

main :: IO ()
main = do
  hspec test500
  defaultMain logicTests

test500 :: Spec
test500 =
  describe "serverApi" $
  it "not500" $
  withServantServer serverApi (return server) $ \burl -> serverSatisfies serverApi burl args (not500 <%> mempty)

testJSON :: Spec
testJSON =
  describe "serverApi" $
  it "onlyJSON" $
  withServantServer serverApi (return server) $ \burl ->
    serverSatisfies serverApi burl args (onlyJsonObjects <%> mempty)

testTimeOut :: Spec
testTimeOut =
  describe "serverApi" $
  it "timeout" $
  withServantServer serverApi (return server) $ \burl ->
    serverSatisfies serverApi burl args (notLongerThan 10000000000 <%> mempty)
