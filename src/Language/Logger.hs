{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}

module Language.Logger where

import           Eff.TH

data Logger a where
  Log :: String -> Logger ()

makeFreer ''Logger
