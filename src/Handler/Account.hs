module Handler.Account where

import qualified Account
import qualified Database.Esqueleto as E
import Import

getAccountR :: BookId -> AccountId -> Handler Html
getAccountR = Account.layout $ \(Entity bookId book) (Entity accountId account) accountTree -> do
        setTitle $ toHtml $ accountName account
        -- TODO: Fix this to get balance. XXX
        
        -- ts <- handlerToWidget $ runDB $ E.select $ E.from $ \(r@(_, ta, _) `E.As` ra) -> do
        --     E.subselect transactionQuery ra
        --     E.where_ (ta E.^. TransactionAccountAccount E.==. E.val accountId)
        --     return r

        ts <- handlerToWidget $ runDB $ E.select $ E.fromSubSelect transactionQuery $ \(t, ta, s) -> do
            E.where_ (ta E.^. TransactionAccountAccount E.==. E.val accountId)
            return (t, ta, E.fromAlias s)

        -- E.subselect_query transactionQuery $ \r@(_, ta, _) -> do
        --     E.where_ (ta E.^. TransactionAccountAccount E.==. E.val accountId)
        --     return r

        -- ts <- handlerToWidget $ runDB $ E.select $ E.from $ \(t `E.InnerJoin` ta) -> do
        --     E.on (t E.^. TransactionId E.==. ta E.^. TransactionAccountTransaction)
        --     E.where_ (ta E.^. TransactionAccountAccount E.==. E.val accountId)
        --     E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
        --     E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
        --     return (t, ta, E.over (E.sum_ (ta E.^. TransactionAccountAmount)) (Just $ ta E.^. TransactionAccountAccount) [E.asc (t E.^. TransactionDate), E.asc (t E.^. TransactionId)])

        accountIsDebit <- Account.isDebit accountTree accountId

        let accountType = if accountIsDebit then "Debit" else "Credit" :: Text

        -- TODO
        -- Edit account link.
        -- Move "New Transaction" to sidebar?
        -- Make account information more of a table/form layout?
        [whamlet|
            <h2>
                #{accountName account}
                <small>
                    #{accountType} Account
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
                        Debit
                    <th>
                        Credit
                    <th>
                        Balance
                ^{concatMap (displayTransaction bookId accountIsDebit) ts}
        |]

    where
        displayTransaction bookId accountIsDebit ((Entity tId t), (Entity taId ta), E.Value balance') = do
            let balance = maybe "" dollar balance'
            [whamlet|
                <tr>
                    <td>
                        <a href="@{TransactionR bookId tId}">
                            #{transactionDescription t}
                    <td>
                        #{shortDateTime (transactionDate t)}
                    <td>
                        #{maybe "" dollar (Account.amountToDebit accountIsDebit $ transactionAccountAmount ta)}
                    <td>
                        #{maybe "" dollar (Account.amountToCredit accountIsDebit $ transactionAccountAmount ta)}
                    <td>
                        #{balance}
            |]

-- transactionQuery :: E.SqlQuery (E.SqlExpr (Entity Transaction), E.SqlExpr (Entity TransactionAccount), E.SqlExpr (E.Value (Maybe Nano)))
-- transactionQuery = E.from $ \(t `E.InnerJoin` ta) -> do
--     E.on (t E.^. TransactionId E.==. ta E.^. TransactionAccountTransaction)
--     E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
--     E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
--     return (t, ta, E.over (E.sum_ (ta E.^. TransactionAccountAmount)) (Just $ ta E.^. TransactionAccountAccount) [E.asc (t E.^. TransactionDate), E.asc (t E.^. TransactionId)])
