{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Injector
  ( Injector (..),
    InjectorConfig (..),
    Dependency (..),
    InjectionError,
    getInstance,
    newInjector,
    provideNothing,
    addProvider,
    emptyConfig,
    provideValue,
    inject,
  )
where

import qualified Control.Concurrent.MVar
import qualified Control.Exception
import qualified Data.Dynamic
import qualified Data.Map as Map
import qualified Data.Proxy
import qualified Data.Typeable
import qualified Type.Reflection

data InjectorConfig m = InjectorConfig
  { providers :: Map.Map Type.Reflection.SomeTypeRep (Injector m -> m (Either InjectionError Data.Dynamic.Dynamic))
  }

emptyConfig :: InjectorConfig IO
emptyConfig =
  Data.Injector.InjectorConfig
    { providers = Map.empty
    }

addProvider :: forall a. (Data.Typeable.Typeable a) => (Injector IO -> IO (Either InjectionError a)) -> InjectorConfig IO -> InjectorConfig IO
addProvider provider config =
  let typeKey = Type.Reflection.someTypeRep (Data.Proxy.Proxy @a)
      dynamicProvider = (fmap $ fmap Data.Dynamic.toDyn) . provider
      updatedProviders = Map.insert typeKey dynamicProvider $ providers config
   in seq updatedProviders $
        InjectorConfig
          { providers = updatedProviders
          }

data Injector m = Injector
  { injectorConfig :: InjectorConfig m,
    instanceCache :: Control.Concurrent.MVar.MVar (Map.Map Type.Reflection.SomeTypeRep Data.Dynamic.Dynamic)
  }

class (Data.Typeable.Typeable a) => Dependency a where
  defaultProvider :: Injector IO -> IO (Either InjectionError a)

data InjectionError
  = InjectionProviderTypeCastError Type.Reflection.SomeTypeRep
  | InjectionInstanceTypeCastError Type.Reflection.SomeTypeRep
  | InjectionDefaultNotProvided Type.Reflection.SomeTypeRep
  deriving (Show)

instance Control.Exception.Exception InjectionError

provideNothing :: forall a. (Data.Typeable.Typeable a) => Injector IO -> IO (Either InjectionError a)
provideNothing _ = return $ Left $ InjectionDefaultNotProvided $ Type.Reflection.someTypeRep (Data.Proxy.Proxy @a)

inject :: (Injector IO -> IO (Either InjectionError a)) -> (a -> IO (Either InjectionError b)) -> Injector IO -> IO (Either InjectionError b)
inject dependencyProvider builder injector = do
    deps <- dependencyProvider injector
    case deps of
        Right resolvedDeps -> builder resolvedDeps
        Left err -> return $ Left err

provideValue :: a -> Injector IO -> IO (Either InjectionError a)
provideValue value _injector = pure $ Right value

newInjector :: InjectorConfig IO -> IO (Injector IO)
newInjector config = do
  cacheMvar <- Control.Concurrent.MVar.newMVar $ Map.empty
  return $
    Injector
      { injectorConfig = config,
        instanceCache = cacheMvar
      }

getInstance :: forall a. (Dependency a) => Injector IO -> IO (Either InjectionError a)
getInstance injector = do
  currentInstanceCache <- Control.Concurrent.MVar.readMVar $ instanceCache injector
  let typeKey = Type.Reflection.someTypeRep (Data.Proxy.Proxy @a)
  let instanceLookup = Map.lookup typeKey currentInstanceCache
  let providerLookup = Map.lookup typeKey (providers $ injectorConfig injector)
  instanceValue <- case (instanceLookup, providerLookup) of
    (Just val, _) -> case Data.Dynamic.fromDynamic val of
      Just typedVal -> return $ Right typedVal
      _ -> return $ Left $ InjectionInstanceTypeCastError typeKey
    (_, Just providerVal) -> do
      val <- providerVal injector
      case fmap Data.Dynamic.fromDynamic val of
        Right (Just typedVal) -> return $ Right typedVal
        Right (Nothing) -> return $ Left $ InjectionProviderTypeCastError typeKey
        Left err -> return $ Left err
    _ -> (defaultProvider injector :: IO (Either InjectionError a))
  case (instanceLookup, instanceValue) of
    (Nothing, Right resolvedInstanceValue) -> do
      let cache = instanceCache injector
      Control.Concurrent.MVar.modifyMVar_ cache $ \currentCache -> do
        let updatedMap = Map.insert typeKey (Data.Dynamic.toDyn resolvedInstanceValue) currentCache
        seq updatedMap $ return updatedMap
      return ()
    _ -> return ()
  return instanceValue
