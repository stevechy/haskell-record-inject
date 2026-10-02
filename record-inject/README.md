# record-inject

# Usage

To use the injector create an `InjectorConfig` and add providers for desired types.

Injectable dependencies must provide a default implementation or explictly declare no default implementation by implementing the `Data.Injector.Dependency` typeclass.

```
instance Data.Injector.Dependency SqlDbPool where
    defaultProvider = Data.Injector.provideNothing
```

`provideNothing` is useful for when the dependency is easier to set up outside the injector.

```
import qualified Data.Injector
import Data.Function ((&))

main :: IO ()
main = do
  let injectorConfig =
      Data.Injector.emptyConfig
        & (Data.Injector.addProvider $ Data.Injector.provideValue Db.Pool.SqlDbPool {sqlDbPool = pool})
        & id
  injector <- Data.Injector.newInjector injectorConfig
  pool <- (Data.Injector.getInstance injector) :: IO (Either Data.Injector.InjectionError Db.Pool.SqlDbPool)
  return ()
```

To autogenerate code to inject multiple dependencies use `Data.Injector.TH` to generate the boilerplate.


```
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeApplications #-}

module Service.User (
    UserService(..)
)
where

data Deps = Deps {
  sqlPool :: Db.Pool.SqlDbPool
}

Data.Injector.TH.mkDependencyProvider 'Deps


data UserService = UserService {
    printHello :: IO ()
}

instance Data.Injector.Dependency UserService where
    defaultProvider = Data.Injector.inject depsProvider (fmap Right . newUserService)

newUserService :: Deps -> IO UserService
newUserService Deps{..} = do
    return $ UserService {
        printHello = do
            putStrLn "Hello World"
    }

```
