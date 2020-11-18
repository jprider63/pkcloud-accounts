{-# LANGUAGE UndecidableInstances #-}

-- | This module typically is imported by end user sites.

module PKCloud.Accounts (
      PKCloudAccounts(..)
    , module Export
    ) where

import PKCloud.Import

import PKCloud.Accounts.Core as Export

import PKCloud.Accounts.Handler.Account
import PKCloud.Accounts.Handler.Account.Create
import PKCloud.Accounts.Handler.Account.Delete
import PKCloud.Accounts.Handler.Account.Edit
import PKCloud.Accounts.Handler.Accounts
import PKCloud.Accounts.Handler.Book
import PKCloud.Accounts.Handler.Book.Settings
import PKCloud.Accounts.Handler.Book.Settings.FrequentTransaction
import PKCloud.Accounts.Handler.Book.Settings.FrequentTransaction.Create
import PKCloud.Accounts.Handler.Book.Settings.FrequentTransaction.Delete
import PKCloud.Accounts.Handler.Book.Settings.FrequentTransaction.Edit
import PKCloud.Accounts.Handler.Books
import PKCloud.Accounts.Handler.Folder
import PKCloud.Accounts.Handler.Folder.Create
import PKCloud.Accounts.Handler.Folder.Delete
import PKCloud.Accounts.Handler.Folder.Edit
import PKCloud.Accounts.Handler.Home
import PKCloud.Accounts.Handler.Transaction
import PKCloud.Accounts.Handler.Transaction.Create
import PKCloud.Accounts.Handler.Transaction.Delete
import PKCloud.Accounts.Handler.Transaction.Edit
import PKCloud.Accounts.Handler.Transactions

instance (ToMasterRoute (PKCloudAccountsApp master) master, RedirectUrl master (Route (PKCloudAccountsApp master)), PKCloudAccounts master) => YesodSubDispatch (PKCloudAccountsApp master) master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesPKCloudAccountsApp)


