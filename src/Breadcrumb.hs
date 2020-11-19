module Breadcrumb (
    Breadcrumb(..)
  , CRUD(..)
  , breadcrumbs
  ) where

import           Import.NoFoundation (($), Entity(..), Text, Route)
import qualified PKCloud.Accounts.Import as I
import           PKCloud.Accounts.Routes

data Breadcrumb master = 
    Accounts
  | Account (CRUD (I.Account master))
  | Book
  | FrequentTransaction (CRUD (I.FrequentTransaction master))
  | Transactions
  | Transaction (CRUD (I.Transaction master))
  | Folder (CRUD (I.FolderAccount master))
  | Settings

data CRUD a = 
    View (Entity a)
  | Create
  | Edit (Entity a)
  | Delete (Entity a)


breadcrumbs :: I.PKCloudAccounts master => Entity (I.Book master) -> Breadcrumb master -> [(Text, Route (PKCloudAccountsApp master))]
breadcrumbs (Entity bookId book) breadcrumb = (I.pkBookName book, BookR bookId): case breadcrumb of
  Book -> []
  Accounts -> accountsBreadcrumbs bookId []
  Account Create -> accountsBreadcrumbs bookId [("New Account", AccountCreateR bookId)]
  Account (View e) -> accountBreadcrumbs bookId e []
  Account (Edit e) -> accountBreadcrumbs bookId e [("Edit Account", AccountEditR bookId (entityKey e))]
  Account (Delete e) -> accountBreadcrumbs bookId e [("Delete Account", AccountDeleteR bookId (entityKey e))]
  Transactions -> transactionsBreadcrumbs bookId []
  Transaction Create -> transactionsBreadcrumbs bookId [("New Transaction", TransactionCreateR bookId)]
  Transaction (View e) -> transactionBreadcrumbs bookId e []
  Transaction (Edit e) -> transactionBreadcrumbs bookId e [("Edit Transaction", TransactionEditR bookId (entityKey e))]
  Transaction (Delete e) -> transactionBreadcrumbs bookId e [("Delete Transaction", TransactionDeleteR bookId (entityKey e))]
  Folder Create -> foldersBreadcrumbs bookId [("New Folder", FolderCreateR bookId)]
  Folder (View e) -> folderBreadcrumbs bookId e []
  Folder (Edit e) -> folderBreadcrumbs bookId e [("Edit Folder", FolderEditR bookId (entityKey e))]
  Folder (Delete e) -> folderBreadcrumbs bookId e [("Delete Folder", FolderDeleteR bookId (entityKey e))]
  Settings -> settingsBreadcrumbs bookId []
  FrequentTransaction Create -> frequentTransactionsBreadcrumbs bookId [("New Frequent Transaction", BookSettingsFrequentCreateR bookId)]
  FrequentTransaction (View e) -> frequentTransactionBreadcrumbs bookId e []
  FrequentTransaction (Edit e) -> frequentTransactionBreadcrumbs bookId e [("Edit Frequent Transaction", BookSettingsFrequentEditR bookId (entityKey e))]
  FrequentTransaction (Delete e) -> frequentTransactionBreadcrumbs bookId e [("Delete Frequent Transaction", BookSettingsFrequentDeleteR bookId (entityKey e))]

settingsBreadcrumbs :: I.BookId master -> [(Text, Route (PKCloudAccountsApp master))] -> [(Text, Route (PKCloudAccountsApp master))]
settingsBreadcrumbs bookId xs = ("Settings", BookSettingsR bookId):xs

frequentTransactionsBreadcrumbs :: I.BookId master -> [(Text, Route (PKCloudAccountsApp master))] -> [(Text, Route (PKCloudAccountsApp master))]
frequentTransactionsBreadcrumbs bookId xs = settingsBreadcrumbs bookId $ ("Frequent Transactions", BookSettingsR bookId):xs

frequentTransactionBreadcrumbs :: I.PKCloudAccounts master => I.BookId master -> Entity (I.FrequentTransaction master) -> [(Text, Route (PKCloudAccountsApp master))] -> [(Text, Route (PKCloudAccountsApp master))]
frequentTransactionBreadcrumbs bookId (Entity ftId ft) xs = frequentTransactionsBreadcrumbs bookId $ (I.pkFrequentTransactionDescription ft, BookSettingsFrequentR bookId ftId):xs

accountsBreadcrumbs :: I.BookId master -> [(Text, Route (PKCloudAccountsApp master))] -> [(Text, Route (PKCloudAccountsApp master))]
accountsBreadcrumbs bookId xs = ("Accounts", AccountsR bookId):xs

accountBreadcrumbs :: I.PKCloudAccounts master => I.BookId master -> Entity (I.Account master) -> [(Text, Route (PKCloudAccountsApp master))] -> [(Text, Route (PKCloudAccountsApp master))]
accountBreadcrumbs bookId (Entity aId a) xs = accountsBreadcrumbs bookId $ (I.pkAccountName a, AccountR bookId aId):xs

foldersBreadcrumbs :: I.BookId master -> [(Text, Route (PKCloudAccountsApp master))] -> [(Text, Route (PKCloudAccountsApp master))]
foldersBreadcrumbs bookId xs = ("Account folders", AccountsR bookId):xs

folderBreadcrumbs :: I.PKCloudAccounts master => I.BookId master -> Entity (I.FolderAccount master) -> [(Text, Route (PKCloudAccountsApp master))] -> [(Text, Route (PKCloudAccountsApp master))]
folderBreadcrumbs bookId (Entity fId f) xs = foldersBreadcrumbs bookId $ (I.pkFolderAccountName f, FolderR bookId fId):xs

transactionsBreadcrumbs :: I.BookId master -> [(Text, Route (PKCloudAccountsApp master))] -> [(Text, Route (PKCloudAccountsApp master))]
transactionsBreadcrumbs bookId xs = ("Transactions", TransactionsR bookId):xs

transactionBreadcrumbs :: I.PKCloudAccounts master => I.BookId master -> Entity (I.Transaction master) -> [(Text, Route (PKCloudAccountsApp master))] -> [(Text, Route (PKCloudAccountsApp master))]
transactionBreadcrumbs bookId (Entity tId t) xs = transactionsBreadcrumbs bookId $ (I.pkTransactionDescription t, TransactionR bookId tId):xs

