module Types where

import ClassyPrelude.Yesod   hiding (getCurrentTime)
import Data.Fixed            (Nano)
import PKCloud.Accounts.Import

data AccountTree master = 
      FolderNode {
        folderNode :: Entity (FolderAccount master)
      , folderNodeBalance :: Nano
      , folderNodeIsDebit :: Bool
      , folderNodeChildren :: [AccountTree master]
      }
    | AccountLeaf {
        accountLeaf :: Entity (Account master)
      , accountLeafBalance :: Nano
      , accountLeafIsDebit :: Bool
      }

