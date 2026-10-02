{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeApplications #-}

module Db.Pool
(
    SqlDbPool(..),
    runSqlDbPool
)
where

import qualified Database.Persist.Sql
import qualified Data.Pool
import qualified Data.Injector
import Control.Monad.Trans.Reader (ReaderT)
import qualified Yesod

data SqlDbPool = SqlDbPool {
    sqlDbPool :: Data.Pool.Pool Database.Persist.Sql.SqlBackend
}

instance Data.Injector.Dependency SqlDbPool where
    defaultProvider = Data.Injector.provideNothing

runSqlDbPool :: (Yesod.MonadUnliftIO m) => SqlDbPool -> ReaderT Database.Persist.Sql.SqlBackend m a -> m a
runSqlDbPool poolWrapper r = Database.Persist.Sql.runSqlPool r (poolWrapper.sqlDbPool)
