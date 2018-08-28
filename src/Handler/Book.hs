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
        assetsId <- insert $ FolderAccount "Assets" Nothing
        insert_ $ BookFolderAccount bId assetsId True

        liabilitiesId <- insert $ FolderAccount "Liabilities" Nothing
        insert_ $ BookFolderAccount bId liabilitiesId False

        equityId <- insert $ FolderAccount "Equity" Nothing
        insert_ $ BookFolderAccount bId equityId False

        return insertedBook

    returnJson insertedBook

--JP: Change BookId to a unique BookUrl?
getBookR :: BookId -> Handler Html
getBookR = Book.layout (const "Overview") $ \(Entity bookId book) accountTree -> do
    setTitle $ toHtml $ bookName book

    [whamlet|
        <div>
            <a .btn .btn-primary href="@{TransactionCreateR bookId}" .pull-right>
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
        filteredW :: [AccountTree] -> Widget
        filteredW tree =
            case catMaybes $ map filterFeatured tree of
                [] ->
                    [whamlet|
                        No featured accounts.
                    |]
                tree -> 
                    [whamlet|
                        <ul>
                            ^{concatMap displayFeatured tree}
                    |]

            
        -- Filter out unfeatured accounts.
        filterFeatured :: AccountTree -> Maybe AccountTree
        filterFeatured node@(FolderNode _ _ _ children) = 
            case catMaybes $ map filterFeatured children of
                [] ->
                    Nothing
                children' -> 
                    Just (node {folderNodeChildren = children'})

        filterFeatured leaf@(AccountLeaf (Entity _ account) _ _) = 
            if accountFeatured account then
                Just leaf
            else
                Nothing

        displayFeatured (FolderNode (Entity folderId folder) balance isDebit children) = [whamlet|
            <li>
                #{folderAccountName folder} - #{dollar balance}
                <ul>
                    ^{concatMap displayFeatured children}
        |]

        displayFeatured (AccountLeaf (Entity accountId account) balance isDebit) = 
            [whamlet|
                <li>
                    #{accountName account} - #{dollar balance}
            |]

