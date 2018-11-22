module Handler.Book where

import qualified Database.Esqueleto as E

import qualified Account
import qualified Book
import Import

-- JP: I'd probably move this to Handler.Book.Create or something.
postBookCreateR :: Handler Value
postBookCreateR = do
    currentUserId <- requireAuthId

    -- Insert book.
    book <- (requireJsonBody :: Handler Book)
    now <- getCurrentTime
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
getBookR = Book.layout $ \(Entity bookId book) accountTree -> do
    setTitle $ toHtml $ bookName book

    Book.setLastOpened bookId

    [whamlet|
        <h2>
            Overview
        <h2>
            Featured Accounts
        <div>
            ^{featuredW bookId accountTree}
        <h2>
            Recent Transactions
        <div>
            ^{recentW accountTree bookId}
    |]

    where
        recentW :: [AccountTree] -> BookId -> Widget
        recentW accountTree bookId = do
            ts' <- handlerToWidget $ runDB $ E.select $ E.fromSubSelect transactionQuery $ \(t, ta, s) -> do
                E.where_ (ta E.^. TransactionAccountAccount `E.in_` E.valList (Account.toAccountIds accountTree))
                return (t, ta, E.fromAlias s)

            -- Mark if we should display the description.
            -- TODO: Is there a faster way? XXX
            let ts = groupBy (\((Entity a _),_,_) ((Entity b _),_,_) -> a == b) ts'

            [whamlet|
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

        featuredW :: BookId -> [AccountTree] -> Widget
        featuredW bookId tree =
            case catMaybes $ map filterFeatured tree of
                [] ->
                    [whamlet|
                        No featured accounts.
                    |]
                tree -> 
                    Book.displayAccountTrees bookId tree

            
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

