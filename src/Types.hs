module Types where

import ClassyPrelude.Yesod   hiding (getCurrentTime)
import Data.Fixed            (Nano)
import Model

data AccountTree = 
      FolderNode {
        folderNode :: Entity FolderAccount
      , folderNodeBalance :: Nano
      , folderNodeIsDebit :: Bool
      , folderNodeChildren :: [AccountTree]
      }
    | AccountLeaf {
        accountLeaf :: Entity Account
      , accountLeafBalance :: Nano
      , accountLeafIsDebit :: Bool
      }

