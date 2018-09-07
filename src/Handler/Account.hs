module Handler.Account where

import qualified Account
import qualified Database.Esqueleto as E
import Import

getAccountR :: BookId -> AccountId -> Handler Html
getAccountR = Account.layout accountName $ \(Entity bookId book) (Entity accountId account) accountTree -> do
        -- TODO: Fix this to get balance. XXX
        ts <- handlerToWidget $ runDB $ E.select $ E.from $ \(t `E.InnerJoin` ta) -> do
            E.on (t E.^. TransactionId E.==. ta E.^. TransactionAccountTransaction)
            E.where_ (ta E.^. TransactionAccountAccount E.==. E.val accountId)
            E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
            E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
            return (t, ta, (E.sum_ (ta E.^. TransactionAccountAmount)))

        -- TODO
        -- Edit account link.
        -- Move "New Transaction" to sidebar?
        [whamlet|
            <div>
                <a .btn .btn-primary href="@{TransactionCreateR bookId}?account=#{fromSqlKey accountId}" .pull-right>
                    New Transaction
            <div .clearfix>
            <h2>
                Transactions
            <table .table .table-condensed>
                <tr>
                    <th>
                        Description
                    <th>
                        Date
                    <th>
                        Amount
                    <th>
                        Balance
                ^{concatMap (displayTransaction bookId) ts}
        |]

    where
        displayTransaction bookId ((Entity tId t), (Entity taId ta), E.Value balance') = do
            let balance = maybe "" dollar balance'
            -- TODO: Separate debits and credits. XXX
            [whamlet|
                <tr>
                    <td>
                        <a href="@{TransactionR bookId tId}">
                            #{transactionDescription t}
                    <td>
                        #{shortDateTime (transactionDate t)}
                    <td>
                        #{dollar (transactionAccountAmount ta)}
                    <td>
                        #{balance}
            |]

