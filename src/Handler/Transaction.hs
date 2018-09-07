module Handler.Transaction where

import qualified Account
import qualified Book
import qualified Database.Esqueleto as E
import Import

getTransactionR :: BookId -> TransactionId -> Handler Html
getTransactionR bookId transactionId = flip (Book.layout (const "Transaction")) bookId $ \(Entity bookId book) accountTree -> do
    -- TODO: Edit button XXX
        
    -- JP: Make a Transaction.layout function?
    ts <- handlerToWidget $ runDB $ E.select $ E.from $ \(t `E.InnerJoin` ta `E.InnerJoin` a) -> do
        E.on (a E.^. AccountId E.==. ta E.^. TransactionAccountAccount)
        E.on (t E.^. TransactionId E.==. ta E.^. TransactionAccountTransaction)
        E.where_ (t E.^. TransactionId E.==. E.val transactionId)
        -- E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
        E.orderBy [E.desc (ta E.^. TransactionAccountId)]
        -- E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
        return (t, ta, a E.^. AccountName)

    -- Check that the accounts are in the book.
    -- JP: We could just check one if we have the invariant that all accounts in the transaction belong to the same book.
    mapM_ (\(_, (Entity _ ta), _) ->
        unless (Account.isInBook accountTree $ transactionAccountAccount ta) $
            permissionDenied ""
      ) $ take 1 ts

    [whamlet|
        <h2>
            Transaction
        <table .table .table-condensed>
            <tr>
                <th>
                    Description
                <th>
                    Date
                <th>
                    Account
                <th>
                    Amount
            ^{concatMap (displayTransactionRow bookId) ts}
    |]

    where
        -- TODO: Don't display description and date multiple times.
        -- TODO: Separate debits and credits. Lookup accounts from account tree? XXX
        displayTransactionRow bookId ((Entity tId t), (Entity taId ta), E.Value accountName) = do
            -- let balance = maybe "" dollar balance'
            [whamlet|
                <tr>
                    <td>
                        #{transactionDescription t}
                    <td>
                        #{shortDateTime (transactionDate t)}
                    <td>
                        <a href="@{AccountR bookId $ transactionAccountAccount ta}">
                            #{accountName}
                    <td>
                        #{dollar (transactionAccountAmount ta)}
            |]
        
