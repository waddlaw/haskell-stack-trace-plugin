{-# LANGUAGE OverloadedStrings #-}
module StackTrace.PluginSpec (spec) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Test.Hspec
import System.Process.Typed

spec :: Spec
spec = do
  output <- runIO exe
  expected <- runIO $ BL.readFile "test/resource/ghc.output"
  it "integration test" $ output `shouldBe` expected


exe :: IO ByteString
exe = do
  runProcess_ $ shell "cabal install exe:example --installdir=./dist"
  err <- readProcessStderr_ $ shell "./dist/example || true"
  runProcess_ $ shell "rm -rf ./dist"
  return err
