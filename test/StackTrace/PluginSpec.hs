{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module StackTrace.PluginSpec (spec) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Process.Typed
import Test.Hspec
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

-- | Extracts the function names of all functions that appear in a stack trace
extractStackTraceFunctions :: String -> [String]
extractStackTraceFunctions input = map extractFunctionName matches
 where
  regex = [r|[ ]*([A-Za-z0-9]+), called at example/Main.hs:[0-9]+:[0-9]+ in [^:]+:Main|] :: String
  matches = (input =~ regex) :: [[String]]
  extractFunctionName :: [String] -> String
  extractFunctionName (_ : name : _) = name
  extractFunctionName _ = error "Regex match failed"

spec :: Spec
spec = do
  output <- runIO exe
  let outputStr = BS.unpack output
  let extractedFunctions = extractStackTraceFunctions outputStr

  -- each of these exceptions should appear in the stack trace
  let expectedFunctions = ["error", "fError", "f10", "f9", "f8", "f7", "f6", "f5", "f4", "f3", "f2", "f1", "main"] :: [String]

  it "integration test" $ extractedFunctions `shouldBe` expectedFunctions

exe :: IO ByteString
exe = do
  runProcess_ $ shell "cabal install exe:example --installdir=./dist"
  err <- readProcessStderr_ $ shell "./dist/example || true"
  runProcess_ $ shell "rm -rf ./dist"
  return err
