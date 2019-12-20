{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
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

main :: IO ()
main = do
  hspec test1
  defaultMain logicTests

test1 :: Spec
test1 =
  describe "serverApi" $
  it "not500" $
  withServantServer serverApi (return server) $ \burl -> serverSatisfies serverApi burl args (not500 <%> mempty)
