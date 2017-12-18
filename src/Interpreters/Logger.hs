{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Interpreters.Logger
  ( runLogger
  ) where

import           Data.Monoid       ((<>))
import           Control.Exception (SomeException)
import           Eff               (Eff, Member, handleRelay)
import           Eff.Exc           (Exc)
import           Eff.SafeIO        (SIO, safeIO)

import           Language.Logger   (Logger (..))

runLogger :: (Member SIO r, Member (Exc SomeException) r) => Eff (Logger ': r) a -> Eff r a
runLogger = handleRelay pure (\k q -> interpret k >>= q)

interpret :: (Member SIO r, Member (Exc SomeException) r)  => Logger x -> Eff r x
interpret (Log message) = safeIO . putStrLn $ "Log: " <> message
