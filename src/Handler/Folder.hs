module Handler.Folder where

import qualified Database.Esqueleto as E

import qualified Account
import qualified Breadcrumb
import qualified Folder
import           Import

getFolderR :: BookId -> FolderAccountId -> Handler Html
getFolderR = Folder.layout Breadcrumb.View $ \(Entity bookId book) accountTree (FolderNode (Entity faId fa) balance isDebit children) -> do
    setTitle $ toHtml ("Folder" :: Text)

    -- Load recent transactions from this folder.
    ts' <- handlerToWidget $ runDB $ E.select $ E.fromSubSelect transactionQuery $ \(t, ta, s) -> do
        E.where_ (ta E.^. TransactionAccountAccount `E.in_` E.valList (Account.toAccountIds children))
        return (t, ta, E.fromAlias s)

    -- Mark if we should display the description.
    -- TODO: Is there a faster way? XXX
    let ts = groupBy (\((Just (Entity a _)),_,_) ((Just (Entity b _)),_,_) -> a == b) $ map justFirst3 ts'

    -- Display folder information and recent transactions.
    let accountType = if isDebit then "Debit" else "Credit" :: Text
    [whamlet|
        <a class="btn btn-primary pull-right" href="@{FolderEditR bookId faId}">
            Edit
        <a class="btn btn-danger pull-right" style="margin-right: 5px" href="@{FolderDeleteR bookId faId}">
            Delete

        <h2>
            #{folderAccountName fa}
            <small>
                #{accountType} Folder

        <h2>
            Transactions
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

