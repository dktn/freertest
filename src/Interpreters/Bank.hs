{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE PartialTypeSignatures         #-}

module Interpreters.Bank
  ( runBank
  ) where

import           Data.Monoid       ((<>))
import           Control.Exception (ArithException (..), SomeException, throwIO)
import           Eff
import           Eff.Exc           (Exc)
import           Eff.Exc.Pure      (catchError)
import           Eff.Region        (Ancestor, Region, RegionEff, Resource,
                                    ResourceCtor, SafeForRegion, acquire,
                                    catchSafeIOExcs, handleRegionRelay,
                                    unsafeWithResource)
import           Eff.SafeIO        (SIO, safeIO)

import           Language.Bank

newtype TransactionId = TransactionId Int deriving (Show, Eq)

-- datatype that represents a resource, could be newtype on file handle
data Transaction = Transaction TransactionId deriving (Eq, Show)

instance SafeForRegion Transaction '[SIO, Exc SomeException]

-- datatype that carries data to resource constructor (see allocateTransaction )
data TransactionType = Default | Special deriving (Show)

-- it says that TransactionType is resouce counstructor type for Transaction resource
type instance ResourceCtor Transaction = TransactionType

-- here we bind transaction region with alloc/release functions
transactionRegion :: forall effs a
                   . ( SafeForRegion Transaction effs
                     , Member SIO effs
                     , Member (Exc SomeException) effs
                     )
                  => Region Transaction effs a
                  -> Eff effs a
transactionRegion = handleRegionRelay allocateTransaction releaseTransaction catchSafeIOExcs
  where
    -- here we can allocate our resource that is handled by region
    -- note that we may allocate more than one resources in single region - they will be all released
    allocateTransaction tpe = do
      safeIO $ putStrLn $ "Starting transaction: " <> show tpe
      return (Transaction (TransactionId 1))
    -- here we release allocated resource, this will always run even if region is terminated by exception
    releaseTransaction (Transaction nr) = safeIO $ putStrLn $ "Ending transaction: " <> show nr

-- this wraps acquire with types set for Transaction resource
startTransaction :: forall r s
                  . ( s ~ Ancestor 0 r
                    , Member (RegionEff Transaction s) r
                    )
                 => TransactionType
                 -> Eff r (Resource Transaction s)
startTransaction = acquire @Transaction

-- wrapper for catchError
inTransaction :: forall effs b
               . ( SafeForRegion Transaction effs
                 , Member SIO effs
                 , Member (Exc SomeException) effs
                 )
              => Region Transaction effs () -> Eff effs ()
inTransaction region = catchError (transactionRegion region) handleTransactionAbort

withTransaction :: forall effs s b
                 . ( s ~ Ancestor 0 effs
                   , Member (RegionEff Transaction s) effs
                   )
                => TransactionType
                -> (Resource Transaction s -> Eff effs b)
                -> Eff effs b
withTransaction transactionType region = startTransaction transactionType >>= region

-- Our error handler for catchError, we return () but
handleTransactionAbort :: ( Member SIO effs
                          , Member (Exc SomeException) effs
                          )
                       => SomeException
                       -> Eff effs ()
handleTransactionAbort _err = safeIO $ putStrLn "Transaction failed!"

-- main of our interpreter - possibly this could be generalized and put to library
runBank :: ( SafeForRegion Transaction r
           , Member SIO r
           , Member (Exc SomeException) r
           )
        => Eff (Bank ': r) a
        -> Eff r a
runBank = handleRelay pure (\k q -> interpret k >>= q)

-- real body of the interpreter, we require SafeIO effect environment (this implies Exc SomeException too)
interpret :: ( SafeForRegion Transaction r
             , Member SIO r
             , Member (Exc SomeException) r
             )
          => Bank x
          -> Eff r x
interpret (PutBalance newBalance) = do
  safeIO $ putStrLn $ "Put balance: " <> show newBalance
  inTransaction $ withTransaction Default $ \transaction -> do
    let transactionName = unsafeWithResource transaction show
    safeIO $ putStrLn $ "Transaction: " <> transactionName
    _ <- safeIO $ throwIO Overflow
    safeIO $ putStrLn "This should not be executed!"
  return ()
interpret GetBalance = do
  safeIO (putStrLn "Get balance")
  return 77
