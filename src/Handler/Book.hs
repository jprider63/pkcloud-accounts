module Handler.Book where

import qualified Book
import Import

-- JP: I'd probably move this to Handler.Book.Create or something.
postBookCreateR :: Handler Value
postBookCreateR = do
    currentUserId <- requireAuthId

    -- Insert book.
    book <- (requireJsonBody :: Handler Book)
    now <- lift getCurrentTime
    let book' = book { bookCreatedBy = currentUserId
                     , bookDateCreated = now }
    insertedBook <- runDB $ do
        insertedBook@(Entity bId _) <- insertEntity book'

        -- Insert default accounts.
        assetsId <- insert $ FolderAccount "Assets" Nothing False
        insert_ $ BookFolderAccount bId assetsId

        liabilitiesId <- insert $ FolderAccount "Liabilities" Nothing True
        insert_ $ BookFolderAccount bId liabilitiesId

        equityId <- insert $ FolderAccount "Equity" Nothing True
        insert_ $ BookFolderAccount bId equityId

        return insertedBook

    returnJson insertedBook

--JP: Change BookId to a unique BookUrl?
getBookR :: BookId -> Handler Html
getBookR = Book.layout (const "Overview") $ \(Entity bookId book) -> do
    setTitle $ toHtml $ bookName book

    accountTree <- handlerToWidget $ Book.accountTrees bookId

    [whamlet|
        <div>
            <a .btn .btn-primary href="#" .pull-right>
                New Transaction
        <div .clearfix>
        <h2>
            Featured Accounts
        <div>
            <ul>
                ^{concatMap displayAccountTree accountTree}
        <h2>
            Recent Transactions
    |]

    where
        displayAccountTree (Book.FolderNode (Entity folderId folder) _ children) = [whamlet|
            <li>
                #{folderAccountName folder}
                <ul>
                    ^{concatMap displayAccountTree children}
        |]
        displayAccountTree (Book.AccountLeaf (Entity accountId account) _) = [whamlet|
            <li>
                #{accountName account}
        |]
