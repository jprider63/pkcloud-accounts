module PKCloud.Accounts.Handler.Book where

import qualified Database.Esqueleto as E

import qualified Account
import qualified Book
import qualified Breadcrumb
import           Import

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
getBookR = Book.layout Breadcrumb.Book $ \(Entity bookId book) accountTree -> do
    setTitle $ toHtml $ bookName book

    Book.setLastOpened bookId

    [whamlet|
        <div .pull-left>
            <h2>
                Featured Accounts
        <div .pull-right>
            <h2>
                <small>
                    <a href="@{AccountsR bookId}">
                        All accounts
        <div .clearfix>
        <div>
            ^{featuredW bookId accountTree}
        <div .pull-left>
            <h2>
                Recent Transactions
        <div .pull-right>
            <h2>
                <small>
                    <a href="@{TransactionsR bookId}">
                        More transactions
        <div .clearfix>
        <div>
            ^{recentW accountTree bookId}
    |]

    where
        recentW :: [AccountTree] -> BookId -> Widget
        recentW accountTree bookId = do
            ts' <- handlerToWidget $ runDB $ E.select $ E.fromSubSelect transactionQuery $ \(t, ta, s) -> do
                E.where_ (ta E.^. TransactionAccountAccount `E.in_` E.valList (Account.toAccountIds accountTree))
                return (t, ta, E.fromAlias s)

                -- JP: Past couple months?

            -- Mark if we should display the description.
            -- TODO: Is there a faster way? XXX
            let ts = groupBy (\((Just (Entity a _)),_,_) ((Just (Entity b _)),_,_) -> a == b) $ map justFirst3 ts'

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

