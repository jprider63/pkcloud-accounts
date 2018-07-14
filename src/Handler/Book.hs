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
getBookR = Book.layout (const "Overview") $ \(Entity bookId book) accountTree -> do
    setTitle $ toHtml $ bookName book

    [whamlet|
        <div>
            <a .btn .btn-primary href="#" .pull-right>
                New Transaction
        <div .clearfix>
        <h2>
            Featured Accounts
        <div>
            ^{filteredW accountTree}
        <h2>
            Recent Transactions
    |]

    where
        filteredW :: [Book.AccountTree] -> Widget
        filteredW tree =
            case filterFeatured tree of
                Nothing ->
                    [whamlet|
                        No featured accounts.
                    |]
                Just tree -> 
                    [whamlet|
                        <ul>
                            ^{concatMap displayFeatured tree}
                    |]

            
        -- Filter out unfeatured accounts.
        filterFeatured node@(Book.FolderNode _ _ children) = 
            case catMaybes $ map filterFeatured children of
                [] ->
                    Nothing
                children' -> 
                    Just (node {Book.folderNodeChildren = children'})

        filterFeatured leaf@(Book.AccountLeaf (Entity _ account) _) = 
            if accountFeatured account then
                Just leaf
            else
                Nothing

        displayFeatured (Book.FolderNode (Entity folderId folder) balance children) = [whamlet|
            <li>
                #{folderAccountName folder} - #{dollar balance}
                <ul>
                    ^{concatMap displayFeatured children}
        |]

        displayFeatured (Book.AccountLeaf (Entity accountId account) balance) = 
            [whamlet|
                <li>
                    #{accountName account} - #{dollar balance}
            |]

