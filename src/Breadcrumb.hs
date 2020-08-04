module Breadcrumb where

import           Import.NoFoundation
import           Foundation

-- type Breadcrumbs = [(Text, Maybe Route)]
data Breadcrumb = 
    Book -- Book
  | Transaction (BreadcrumbCRUD Transaction)
  -- | BreadcrumbFolder BreadcrumbFolder
  -- | BreadcrumbAccount (Entity Account)
  -- | BreadcrumbSettings

data BreadcrumbCRUD a = 
    View (Entity a)
  | Create
  | Edit (Entity a)
  | Delete (Entity a)


-- data BreadcrumbTransaction
-- data BreadcrumbFolder
-- data BreadcrumbAccount = BreadcrumbAccount

breadcrumbs :: Entity Book -> Breadcrumb -> [(Text, Route App)]
breadcrumbs (Entity bookId book) breadcrumb = (bookName book, BookR bookId): case breadcrumb of
  BreadcrumbBook -> []
  BreadcrumbTransaction (View (Entity tId t)) -> [("Transactions", TransactionsR bookId), (transactionDescription t, TransactionR bookId tId)]
