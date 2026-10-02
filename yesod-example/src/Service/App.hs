{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# Language RecordWildCards #-}

module Service.App (
    Services(..),
    module Import
)
where

import qualified Data.Injector
import qualified Data.Injector.TH
import Service.User as Import
import Service.Comment as Import

data Services = Services {
    userService :: UserService,
    commentService :: CommentService
}

Data.Injector.TH.mkDependencyProvider 'Services

instance Data.Injector.Dependency Services where
    defaultProvider = servicesProvider
