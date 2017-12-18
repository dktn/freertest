{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}


module Main where

import           Data.Monoid           ((<>))
import           Eff                   (Eff, Member, runM)
import           Eff.SafeIO            (runSafeIO)

import           Domain
import           Interpreters.Logger   (runLogger)
import           Interpreters.Bank     (runBank)
import           Language.Logger       (Logger)
import qualified Language.Logger       as Logger
import           Language.Bank         (Bank)
import qualified Language.Bank         as Bank

main :: IO ()
main = do
  balance <- runSafeIO $ runBank $ runLogger bankScenario
  putStrLn balance

bankScenario :: (Member Bank r, Member Logger r) => Eff r String
bankScenario = do
  balance <- Bank.getBalance
  Logger.log $ "Current balance " <> show balance
  Logger.log "Changing balance to $20"
  Bank.putBalance 20
  balance <- Bank.getBalance
  Logger.log $ "Current balance " <> show balance
  return $ "$" <> show balance
