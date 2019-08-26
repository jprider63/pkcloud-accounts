module Handler.Book.Settings.FrequentTransaction where

import qualified Database.Esqueleto as E

import qualified Account
import qualified Book
import Import

getBookSettingsFrequentR :: BookId -> FrequentTransactionId -> Handler Html
getBookSettingsFrequentR bookId ftId = flip Book.layout bookId $ \(Entity bookId book) accountTree -> do
    setTitle $ toHtml ("Frequent Transaction" :: Text)
        
    -- JP: Make a Transaction.layout function?
    ts <- handlerToWidget $ runDB $ E.select $ E.from $ \(t `E.InnerJoin` ta) -> do
        E.on (t E.^. FrequentTransactionId E.==. ta E.^. FrequentTransactionAccountTransaction)
        E.where_ (t E.^. FrequentTransactionId E.==. E.val ftId)
        -- E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
        E.orderBy [E.asc (ta E.^. FrequentTransactionAccountId)]
        -- E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
        return (t, ta, E.val Nothing)
    
    -- Check that the accounts are in the book.
    -- JP: We could just check one if we have the invariant that all accounts in the transaction belong to the same book.
    Account.requireAllInBook accountTree $ map (\(_, (Entity _ ta), _) -> frequentTransactionAccountAccount ta) $ take 1 ts

    [whamlet|
        <a class="btn btn-primary pull-right" href="FrequentTransactionEditR bookId transactionId">
            Edit
        <a class="btn btn-danger pull-right" style="margin-right: 5px" href="FrequentTransactionDeleteR bookId transactionId">
            Delete
        <h2>
            Frequent Transaction
        <table .table .table-condensed>
            <tr>
                <th>
                    Description
                <th>
                    Account
                <th>
                    Debit
                <th>
                    Credit
            ^{Account.displayTransactionRow accountTree bookId ts}
    |]
