{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time
import Data.Typeable (Typeable)
import Database.Persist.Quasi


share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

deriving instance Show (Unique ICounter)
deriving instance Show (Unique IWebCounter)
