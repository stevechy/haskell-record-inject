{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeApplications #-}

module Service.Comment (
    CommentService(..)
)
where

import qualified Data.Injector
import qualified Data.Injector.TH
import qualified Db.Pool
import qualified Db.Comment
import Model

data Deps = Deps {
    sqlPool :: Db.Pool.SqlDbPool,
    commentRepository :: Db.Comment.CommentRepository
}

Data.Injector.TH.mkDependencyProvider 'Deps

data CommentService = CommentService {
    printComments :: IO ()
}

instance Data.Injector.Dependency CommentService where
    defaultProvider = Data.Injector.inject depsProvider (fmap Right . newCommentService)

newCommentService :: Deps -> IO CommentService
newCommentService Deps{..} = do
    return $ CommentService {
        printComments = do
            comments <- Db.Pool.runSqlDbPool sqlPool commentRepository.getComments
            putStrLn $ "Number of comments: " ++ (show $ length comments)
    }
