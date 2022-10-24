module Main where

import Data.Maybe (fromJust)
import GHC.Stack

main :: IO ()
main = print f1

f1 :: Int
f1 = f2

f2 :: Int
f2 = f3

-- HsQualTy
f3 :: HasCallStack => Int
f3 = f4 0

-- HsQualTy
f4 :: Show a => a -> Int
f4 n = f5 (show n) 0

-- HsFunTy
f5 :: String -> Int -> Int
f5 _ _ = head f6

-- HsListTy
f6 :: [Int]
f6 = [fst f7]

-- HsTupleTy
f7 :: (Int, Int)
f7 = (fromJust f8, fromJust f8)

-- HsAppTy
f8 :: Maybe Int
f8 = Just f9

f9 :: Int
f9 = f10
  where
    f10 :: Int
    f10 = fError

-- HsTyVar
fError :: Int
fError = error "fError"
