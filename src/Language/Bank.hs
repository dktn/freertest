{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}

module Language.Bank where

import           Eff.TH

import           Domain

data Bank a where
  GetBalance :: Bank Int
  PutBalance :: Int -> Bank ()

makeFreer ''Bank
