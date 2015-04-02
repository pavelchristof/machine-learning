{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Data.ML.Repl
Description :  Simple REPL.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

A simple REPL to test your models.
-}
module Data.ML.Repl (
    ReplT,
    runReplT,
    driverRepl
    ) where

import Data.ML.Driver
import Data.ML.Internal.Repl
import Data.Typeable
import Language.Haskell.Interpreter

driverRepl :: Typeable model => Typeable scalar => String -> ReplT (Driver model scalar) ()
driverRepl mainFile = do
    set [ languageExtensions :=
              [ DataKinds
              , RankNTypes
              , TypeFamilies
              , GADTs
              , FlexibleContexts
              , FlexibleInstances
              , ScopedTypeVariables
              , TypeOperators
              , OverloadedStrings
              , OverloadedLists
              ]
        , installedModulesInScope := True
        ]
    loadModules [mainFile]
    setTopLevelModules ["Main"]
    repl
