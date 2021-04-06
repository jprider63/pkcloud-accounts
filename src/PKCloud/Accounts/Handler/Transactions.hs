module PKCloud.Accounts.Handler.Transactions where

import qualified Database.Esqueleto as E
import qualified Data.List as List

import qualified Account
import qualified Book
import qualified Breadcrumb
import           Import
import           Types.Classes

getTransactionsR :: forall master . (PKCloudAccounts master, GeneralizedTransactionAccount (TransactionAccount master)) => BookId master -> Handler master Html
getTransactionsR = Book.layout Breadcrumb.Transactions $ \(Entity bookId book) accountTree -> do
    pkcloudSetTitle $ toHtml ("Transactions" :: Text)

    -- TODO: Filters by date, ...

    [whamlet|
            <h2>
                Transactions
            <div>
                ^{transactionsW accountTree bookId}
    |]

  where
    transactionsW :: [AccountTree master] -> BookId master -> WidgetFor master ()
    transactionsW accountTree bookId = do
      ts' <- handlerToWidget $ liftHandler $ runDB $ E.select $ E.fromSubSelect transactionQuery $ \(t, ta, s) -> do
        E.where_ (ta E.^. pkTransactionAccountAccountField `E.in_` E.valList (Account.toAccountIds accountTree))
        return (t, ta, E.fromAlias s)

      -- Mark if we should display the description.
      -- TODO: Is there a faster way? XXX
      let ts = List.groupBy (\((Just (Entity a _)),_,_) ((Just (Entity b _)),_,_) -> a == b) $ map justFirst3 ts'

      [whamlet|
          <table .table .table-condensed>
              <tr>
                  <th>
                      Description
                  <th>
                      Date
                  <th>
                      Account
                  <th>
                      Debit
                  <th>
                      Credit
                  <th>
                      Balance
              ^{mconcat (map (Account.displayTransactionRow accountTree bookId . reverse) ts)}
      |]

