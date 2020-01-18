# haskell-stack-trace-plugin

![](https://github.com/waddlaw/haskell-stack-trace-plugin/workflows/cabal/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/haskell-stack-trace-plugin.svg)](https://hackage.haskell.org/package/haskell-stack-trace-plugin)

This plugin allow implicitly add `HasCallStack` class to every top-level function for all module. Hence, we can  to get completely continuous call stack.

1. (implicitly) Import [GHC.Stack](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Stack.html) for all modules.
2. Add [HasCallStack](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Stack.html#t:HasCallStack) constraint for all top-level functions.

Requirement: (8.6 <= on GHC)

## Synopsis

```haskell
module Main where

import Data.Maybe (fromJust)

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
f4 _ = f5 0 0

-- HsFunTy
f5 :: Int -> Int -> Int
f5 _ _ = head f6

-- HsListTy
f6 :: [Int]
f6 = [fst f7]

-- HsTupleTy
f7 :: (Int, Int)
f7 = (fromJust f8, fromJust f8)

-- HsAppTy
f8 :: Maybe Int
f8 = Just fError

-- HsTyVar
fError :: Int
fError = error "fError"
```

This example get error:

```shell
$ cabal new-build
example/Main.hs:15:7: error:
    Not in scope: type constructor or class ‘HasCallStack’
   |
15 | f3 :: HasCallStack => Int
   |       ^^^^^^^^^^^^
```

Yes, add `import GHC.Stack` to above example.

Fix and rebuild!

```shell
$ cabal new-run
example: fError
CallStack (from HasCallStack):
  error, called at example/Main.hs:41:10 in main:Main
```

Hmm, it is not useful. But, you will to be happy when enable this plugin.

```cabal
  ghc-options:
    -fplugin=StackTrace.Plugin
```

```shell
$ cabal new-run
...

example: fError
CallStack (from HasCallStack):
  error, called at example/Main.hs:40:10 in main:Main
  fError, called at example/Main.hs:36:11 in main:Main
  f8, called at example/Main.hs:32:16 in main:Main
  f7, called at example/Main.hs:28:11 in main:Main
  f6, called at example/Main.hs:24:15 in main:Main
  f5, called at example/Main.hs:20:8 in main:Main
  f4, called at example/Main.hs:16:6 in main:Main
  f3, called at example/Main.hs:12:6 in main:Main
  f2, called at example/Main.hs:9:6 in main:Main
  f1, called at example/Main.hs:6:14 in main:Main
  main, called at example/Main.hs:6:1 in main:Main
```

Great!!!
