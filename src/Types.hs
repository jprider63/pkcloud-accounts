module Types where

import ClassyPrelude.Yesod   hiding (getCurrentTime)
import Data.Fixed            (Nano)
import PKCloud.Accounts.Import
import PKCloud.Accounts.Routes

type Handler master = SubHandlerFor (PKCloudAccountsApp master) master
type Widget master = WidgetFor (PKCloudAccountsApp master)
--type Widget master = WidgetFor (PKCloudAccountsApp master)
--type Handler master = HandlerFor (PKCloudAccountsApp master)

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

