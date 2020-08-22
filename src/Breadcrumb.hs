module Breadcrumb (
    Breadcrumb(..)
  , CRUD(..)
  , breadcrumbs
  ) where

import           Import.NoFoundation (Entity(..), Text, ($))
import qualified Import.NoFoundation as I
import           Foundation

data Breadcrumb = 
    Accounts
  | Account (CRUD I.Account)
  | Book
  | FrequentTransaction (CRUD I.FrequentTransaction)
  | Transactions
  | Transaction (CRUD I.Transaction)
  | Folder (CRUD I.FolderAccount)
  | Settings

data CRUD a = 
    View (Entity a)
  | Create
  | Edit (Entity a)
  | Delete (Entity a)


breadcrumbs :: Entity I.Book -> Breadcrumb -> [(Text, Route App)]
breadcrumbs (Entity bookId book) breadcrumb = (I.bookName book, BookR bookId): case breadcrumb of
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

settingsBreadcrumbs :: I.BookId -> [(Text, Route App)] -> [(Text, Route App)]
settingsBreadcrumbs bookId xs = ("Settings", BookSettingsR bookId):xs

frequentTransactionsBreadcrumbs :: I.BookId -> [(Text, Route App)] -> [(Text, Route App)]
frequentTransactionsBreadcrumbs bookId xs = settingsBreadcrumbs bookId $ ("Frequent Transactions", BookSettingsR bookId):xs

frequentTransactionBreadcrumbs :: I.BookId -> Entity I.FrequentTransaction -> [(Text, Route App)] -> [(Text, Route App)]
frequentTransactionBreadcrumbs bookId (Entity ftId ft) xs = frequentTransactionsBreadcrumbs bookId $ (I.frequentTransactionDescription ft, BookSettingsFrequentR bookId ftId):xs

accountsBreadcrumbs :: I.BookId -> [(Text, Route App)] -> [(Text, Route App)]
accountsBreadcrumbs bookId xs = ("Accounts", AccountsR bookId):xs

accountBreadcrumbs :: I.BookId -> Entity I.Account -> [(Text, Route App)] -> [(Text, Route App)]
accountBreadcrumbs bookId (Entity aId a) xs = accountsBreadcrumbs bookId $ (I.accountName a, AccountR bookId aId):xs

foldersBreadcrumbs :: I.BookId -> [(Text, Route App)] -> [(Text, Route App)]
foldersBreadcrumbs bookId xs = ("Account folders", AccountsR bookId):xs

folderBreadcrumbs :: I.BookId -> Entity I.FolderAccount -> [(Text, Route App)] -> [(Text, Route App)]
folderBreadcrumbs bookId (Entity fId f) xs = foldersBreadcrumbs bookId $ (I.folderAccountName f, FolderR bookId fId):xs

transactionsBreadcrumbs :: I.BookId -> [(Text, Route App)] -> [(Text, Route App)]
transactionsBreadcrumbs bookId xs = ("Transactions", TransactionsR bookId):xs

transactionBreadcrumbs :: I.BookId -> Entity I.Transaction -> [(Text, Route App)] -> [(Text, Route App)]
transactionBreadcrumbs bookId (Entity tId t) xs = transactionsBreadcrumbs bookId $ (I.transactionDescription t, TransactionR bookId tId):xs

