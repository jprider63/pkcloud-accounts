module Handler.Transaction where

import qualified Database.Esqueleto as E

import qualified Account
import Import
import qualified Transaction

getTransactionR :: BookId -> TransactionId -> Handler Html
getTransactionR = Transaction.layout $ \(Entity bookId _) (Entity transactionId transaction) entries accountTree -> do
    setTitle $ toHtml ("Transaction" :: Text)

    let ts = zip3 (repeat Nothing) entries (repeat (E.Value (Nothing :: (Maybe Nano))))

    -- TODO: Fix this CSS XXX
    [whamlet|
        <a class="btn btn-primary pull-right" href="@{TransactionEditR bookId transactionId}">
            Edit
        <a class="btn btn-danger pull-right" style="margin-right: 5px" href="@{TransactionDeleteR bookId transactionId}">
            Delete
        <h2>
            Transaction
        <form>
            <div .form-group>
                <label>
                    Description
                <input .static>
                    #{transactionDescription transaction}
            <div .form-group>
                <label>
                    Date
                <input .static>
                    #{shortDateTime (transactionDate transaction)}
            <div .form-group>
                <label>
                    Transactions
                <table .table .table-condensed>
                    <tr>
                        <th>
                            Account
                        <th>
                            Debit
                        <th>
                            Credit
                    ^{Account.displayTransactionRow accountTree bookId ts}
    |]

        -- TODO: Separate debits and credits. Lookup accounts from account tree? XXX
        
