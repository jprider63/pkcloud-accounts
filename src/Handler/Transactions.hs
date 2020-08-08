module Handler.Transactions where

import qualified Database.Esqueleto as E

import qualified Account
import qualified Book
import qualified Breadcrumb
import           Import

getTransactionsR :: BookId -> Handler Html
getTransactionsR = Book.layout Breadcrumb.Transactions $ \(Entity bookId book) accountTree -> do
    setTitle $ toHtml ("Transactions" :: Text)

    -- TODO: Filters by date, ...

    [whamlet|
            <h2>
                Transactions
            <div>
                ^{transactionsW accountTree bookId}
    |]

  where
    transactionsW :: [AccountTree] -> BookId -> Widget
    transactionsW accountTree bookId = do
      ts' <- handlerToWidget $ runDB $ E.select $ E.fromSubSelect transactionQuery $ \(t, ta, s) -> do
        E.where_ (ta E.^. TransactionAccountAccount `E.in_` E.valList (Account.toAccountIds accountTree))
        return (t, ta, E.fromAlias s)

      -- Mark if we should display the description.
      -- TODO: Is there a faster way? XXX
      let ts = groupBy (\((Just (Entity a _)),_,_) ((Just (Entity b _)),_,_) -> a == b) $ map justFirst3 ts'

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
              ^{concatMap (Account.displayTransactionRow accountTree bookId . reverse) ts}
      |]

