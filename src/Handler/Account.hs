module Handler.Account where

import qualified Database.Esqueleto as E

import qualified Account
import qualified Breadcrumb
import           Import

getAccountR :: BookId -> AccountId -> Handler Html
getAccountR = Account.layout Breadcrumb.View $ \(Entity bookId book) (Entity accountId account) accountIsDebit accountTree -> do
    setTitle $ toHtml $ accountName account
    -- TODO: Fix this to get balance. XXX
    
    -- ts <- handlerToWidget $ runDB $ E.select $ E.from $ \(r@(_, ta, _) `E.As` ra) -> do
    --     E.subselect transactionQuery ra
    --     E.where_ (ta E.^. TransactionAccountAccount E.==. E.val accountId)
    --     return r

    ts' <- handlerToWidget $ runDB $ E.select $ E.fromSubSelect transactionQuery $ \(t, ta, s) -> do
        E.where_ (ta E.^. TransactionAccountAccount E.==. E.val accountId)
        return (t, ta, E.fromAlias s)
    let ts = map justFirst3 ts'

    -- E.subselect_query transactionQuery $ \r@(_, ta, _) -> do
    --     E.where_ (ta E.^. TransactionAccountAccount E.==. E.val accountId)
    --     return r

    -- ts <- handlerToWidget $ runDB $ E.select $ E.from $ \(t `E.InnerJoin` ta) -> do
    --     E.on (t E.^. TransactionId E.==. ta E.^. TransactionAccountTransaction)
    --     E.where_ (ta E.^. TransactionAccountAccount E.==. E.val accountId)
    --     E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
    --     E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
    --     return (t, ta, E.over (E.sum_ (ta E.^. TransactionAccountAmount)) (Just $ ta E.^. TransactionAccountAccount) [E.asc (t E.^. TransactionDate), E.asc (t E.^. TransactionId)])

    let accountType = if accountIsDebit then "Debit" else "Credit" :: Text

    -- TODO
    -- Edit account link.
    -- Move "New Transaction" to sidebar?
    -- Make account information more of a table/form layout?
    [whamlet|
        <a class="btn btn-primary pull-right" href="@{AccountEditR bookId accountId}">
            Edit
        <a class="btn btn-danger pull-right" style="margin-right: 5px" href="@{AccountDeleteR bookId accountId}">
            Delete
        <h2>
            #{accountName account}
            <small>
                #{accountType} Account
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
            ^{concatMap (Account.displayTransactionRow' accountTree bookId False . return) ts}
    |]

-- transactionQuery :: E.SqlQuery (E.SqlExpr (Entity Transaction), E.SqlExpr (Entity TransactionAccount), E.SqlExpr (E.Value (Maybe Nano)))
-- transactionQuery = E.from $ \(t `E.InnerJoin` ta) -> do
--     E.on (t E.^. TransactionId E.==. ta E.^. TransactionAccountTransaction)
--     E.orderBy [E.desc (t E.^. TransactionDate), E.desc (t E.^. TransactionId)]
--     E.groupBy (t E.^. TransactionId, ta E.^. TransactionAccountId)
--     return (t, ta, E.over (E.sum_ (ta E.^. TransactionAccountAmount)) (Just $ ta E.^. TransactionAccountAccount) [E.asc (t E.^. TransactionDate), E.asc (t E.^. TransactionId)])
