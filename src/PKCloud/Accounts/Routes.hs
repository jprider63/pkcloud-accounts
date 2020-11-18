module PKCloud.Accounts.Routes where

import PKCloud.Import

import PKCloud.Accounts.Import

data PKCloudAccountsApp master = PKCloudAccountsApp

mkYesodSubData "(PKCloudAccounts master) => PKCloudAccountsApp master" [parseRoutes|
/ HomeR GET

/book/#{BookId master} BookR GET
/book/#{BookId master}/settings BookSettingsR GET
/book/#{BookId master}/settings/frequent_transaction/#{FrequentTransactionId master} BookSettingsFrequentR GET
/book/#{BookId master}/settings/frequent_transaction/#{FrequentTransactionId master}/delete BookSettingsFrequentDeleteR GET POST
/book/#{BookId master}/settings/frequent_transaction/#{FrequentTransactionId master}/edit BookSettingsFrequentEditR GET POST
/book/#{BookId master}/settings/frequent_transactions/new BookSettingsFrequentCreateR GET POST
/books BooksR GET
/books/new BookCreateR POST

/book/#{BookId master}/accounts AccountsR GET
/book/#{BookId master}/account/#{AccountId master} AccountR GET
/book/#{BookId master}/account/#{AccountId master}/delete AccountDeleteR GET POST
/book/#{BookId master}/account/#{AccountId master}/edit AccountEditR GET POST
/book/#{BookId master}/accounts/new AccountCreateR GET POST
/book/#{BookId master}/folder/#{FolderAccountId master} FolderR GET
/book/#{BookId master}/folder/#{FolderAccountId master}/delete FolderDeleteR GET POST
/book/#{BookId master}/folder/#{FolderAccountId master}/edit FolderEditR GET POST
/book/#{BookId master}/folders/new FolderCreateR GET POST
/book/#{BookId master}/transactions TransactionsR GET
/book/#{BookId master}/transaction/#{TransactionId master} TransactionR GET
/book/#{BookId master}/transaction/#{TransactionId master}/delete TransactionDeleteR GET POST
/book/#{BookId master}/transaction/#{TransactionId master}/edit TransactionEditR GET POST
/book/#{BookId master}/transactions/new TransactionCreateR GET POST
|]
