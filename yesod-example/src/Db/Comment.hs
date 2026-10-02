{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeApplications #-}

module Db.Comment (
    CommentRepository(..)
)
where

import qualified Data.Injector
import qualified Data.Injector.TH
import qualified Db.Pool
import qualified Database.Persist.Sql
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Class
import Model

data Deps = Deps {
    sqlPool :: Db.Pool.SqlDbPool
}

Data.Injector.TH.mkDependencyProvider 'Deps

data CommentRepository = CommentRepository {
    getComments :: ReaderT Database.Persist.Sql.SqlBackend IO [Database.Persist.Sql.Entity Comment]
}

instance Data.Injector.Dependency CommentRepository where
    defaultProvider = Data.Injector.inject depsProvider (fmap Right . newCommentRepository)

newCommentRepository :: Deps -> IO CommentRepository
newCommentRepository Deps{..} = do
    return $ CommentRepository {
        getComments = do
            results <- (selectList @Comment []  [])
            return results
    }
