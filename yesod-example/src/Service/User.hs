{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeApplications #-}

module Service.User (
    UserService(..),
    otherUserServiceProvider,
)
where

import qualified Data.Injector
import qualified Data.Injector.TH
import qualified Db.Pool
import Database.Persist.Class
import Model

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
            results <- Db.Pool.runSqlDbPool sqlPool $ (selectList @User []  [])
            putStrLn $ "Hello World Users: " ++ (show $ length results)
    }



otherUserServiceProvider :: (Data.Injector.Injector IO) -> IO UserService
otherUserServiceProvider _injector = pure UserService {
    printHello = putStrLn "Hello Other World"
}
