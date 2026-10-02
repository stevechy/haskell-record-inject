{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Injector
import Data.Injector.TH

data LeafDependency = LeafDependency {}

instance Data.Injector.Dependency LeafDependency where
  defaultProvider _ = fmap Right $ do
    return $ LeafDependency {}

data RecordTest = RecordTest
  { aLeaf :: LeafDependency
  }

mkDependencyProvider 'RecordTest

main :: IO ()
main = putStrLn "Test suite not yet implemented"
