module PKCloud.Accounts.Core (
      module Export
    , PKCloudAccounts(..)
    ) where

import PKCloud (PKCloudApp(..))

import PKCloud.Accounts.Import as Export
import PKCloud.Accounts.Routes as Export

instance PKCloudAccounts master => PKCloudApp (PKCloudAccountsApp master) where
  pkcloudAppName PKCloudAccountsApp = "Accounts"
  pkcloudAppIdentifier PKCloudAccountsApp = "accounts"
  pkcloudAppRoot = HomeR

