module Handler.Book.Settings.FrequentTransaction where

import qualified Database.Esqueleto as E

import qualified Account
import qualified Book
import Import

getBookSettingsFrequentR :: BookId -> FrequentTransactionId -> Handler Html
getBookSettingsFrequentR bookId ftId = flip Book.layout bookId $ \(Entity bookId book) accountTree -> do
    setTitle $ toHtml ("Frequent Transaction" :: Text)
        
    -- ts <- handlerToWidget $ runDB $ E.select $ E.from $ \(t `E.InnerJoin` ta) -> do
    --     E.on (t E.^. FrequentTransactionId E.==. ta E.^. FrequentTransactionAccountTransaction)
    --     E.where_ (t E.^. FrequentTransactionId E.==. E.val ftId)
    --     -- E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
    --     E.orderBy [E.asc (ta E.^. FrequentTransactionAccountId)]
    --     -- E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
    --     return (E.just t, ta, E.val Nothing)
    
    -- -- TODO: Make a Transaction.layout function? XXX
    -- ts <- handlerToWidget $ runDB $ E.select $ \ta -> do
    --     E.where_ (ta E.^. FrequentTransactionAccountAccount E.==. E.val ftId)
    --     -- E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
    --     E.orderBy [E.asc (ta E.^. FrequentTransactionAccountId)]
    --     -- E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
    --     return (E.val Nothing, ta, E.val Nothing)
    -- 
    -- -- let ts = zip3 (repeat $ E.Value Nothing) entries (repeat (E.Value (Nothing :: (Maybe Nano))))

    -- -- Check that the accounts are in the book.
    -- -- JP: We could just check one if we have the invariant that all accounts in the transaction belong to the same book.
    -- Account.requireAllInBook accountTree $ map (\(_, (Entity _ ta), _) -> frequentTransactionAccountAccount ta) $ take 1 ts

    -- [whamlet|
    --     <a class="btn btn-primary pull-right" href="@{BookSettingsFrequentEditR bookId ftId}">
    --         Edit
    --     <a class="btn btn-danger pull-right" style="margin-right: 5px" href="@{BookSettingsFrequentDeleteR bookId ftId}">
    --         Delete
    --     <h2>
    --         Frequent Transaction
    --     <table .table .table-condensed>
    --         <tr>
    --             <th>
    --                 Description
    --             <th>
    --                 Account
    --             <th>
    --                 Debit
    --             <th>
    --                 Credit
    --         ^{Account.displayTransactionRow accountTree bookId ts}
    -- |]
