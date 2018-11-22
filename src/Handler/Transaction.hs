module Handler.Transaction where

import qualified Account
import qualified Book
import qualified Database.Esqueleto as E
import Import

getTransactionR :: BookId -> TransactionId -> Handler Html
getTransactionR bookId transactionId = flip Book.layout bookId $ \(Entity bookId book) accountTree -> do
    setTitle $ toHtml ("Transaction" :: Text)
        
    -- JP: Make a Transaction.layout function?
    ts <- handlerToWidget $ runDB $ E.select $ E.from $ \(t `E.InnerJoin` ta) -> do
        E.on (t E.^. TransactionId E.==. ta E.^. TransactionAccountTransaction)
        E.where_ (t E.^. TransactionId E.==. E.val transactionId)
        -- E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
        E.orderBy [E.asc (ta E.^. TransactionAccountId)]
        -- E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
        return (t, ta, E.val Nothing)

    -- Check that the accounts are in the book.
    -- JP: We could just check one if we have the invariant that all accounts in the transaction belong to the same book.
    Account.requireAllInBook accountTree $ map (\(_, (Entity _ ta), _) -> transactionAccountAccount ta) $ take 1 ts

    [whamlet|
        <a class="btn btn-primary pull-right" href="@{TransactionEditR bookId transactionId}">
            Edit
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
                    Debit
                <th>
                    Credit
            ^{Account.displayTransactionRow accountTree bookId ts}
    |]

    where
        -- TODO: Don't display description and date multiple times.
        -- TODO: Separate debits and credits. Lookup accounts from account tree? XXX
        displayTransactionRow bookId accountTree ((Entity tId t), (Entity taId ta), (Entity aId a)) = do
            accountIsDebit <- Account.isDebit accountTree aId

            -- let balance = maybe "" dollar balance'
            [whamlet|
                <tr>
                    <td>
                        #{transactionDescription t}
                    <td>
                        #{shortDateTime (transactionDate t)}
                    <td>
                        <a href="@{AccountR bookId $ transactionAccountAccount ta}">
                            #{accountName a}
                    <td>
                        #{maybe "" dollar (Account.amountToDebit accountIsDebit $ transactionAccountAmount ta)}
                    <td>
                        #{maybe "" dollar (Account.amountToCredit accountIsDebit $ transactionAccountAmount ta)}
            |]
        
