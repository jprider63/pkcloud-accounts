module Book where

-- import qualified Account
import qualified Database.Esqueleto as E
import Import

requireCanWriteBook book = do
    uId <- requireAuthId
    when (bookCreatedBy book /= uId) $ 
        permissionDenied ""

canViewBook :: UserId -> Book -> Bool
canViewBook uId book = bookCreatedBy book == uId

layout :: (Entity Book -> [AccountTree] -> Widget) -> BookId -> Handler Html
layout w bookId = do
    -- Check if user is authenticated.
    uId <- requireAuthId
    
    -- Lookup book.
    book <- runDB $ get404 bookId

    -- Check if user can view book.
    unless (canViewBook uId book) $ 
        permissionDenied ""

    accountTree <- accountTrees bookId

    defaultLayout $ [whamlet|
            <div .container>
                <div .row>
                    <h1>
                        Book #{bookName book}
                <div .row>
                    <div .col-xs-3>
                        ^{sidebarW accountTree}
                    <div .col-xs-9>
                        ^{w (Entity bookId book) accountTree}
        |]

    where
        sidebarW :: [AccountTree] -> Widget
        sidebarW trees = [whamlet|
            <div .sidebar>
                <h2>
                    <a href="@{BookR bookId}">
                        Overview
                <h2>
                    Accounts
                <a href="@{AccountCreateR bookId}" .btn .pull-right>
                    New Account
                <a href="@{FolderCreateR bookId}" .btn .pull-right>
                    New Folder
                ^{concatMap sidebarAccountTree trees}
        |]

        sidebarAccountTree (FolderNode (Entity folderId folder) balance isDebit children) =
            [whamlet|
                <li>
                    #{folderAccountName folder} - #{dollar balance}
                    <ul>
                        ^{concatMap sidebarAccountTree children}
            |]
        sidebarAccountTree (AccountLeaf (Entity accountId account) balance isDebit) =
            [whamlet|
                <li>
                    <a href="@{AccountR bookId accountId}">
                        #{accountName account}
                    - #{dollar balance}
            |]

accountTrees :: BookId -> Handler [AccountTree]
accountTrees bookId = runDB $ do
    -- Get book folders.
    bookFolders <- E.select $ E.from $ \(bfa `E.InnerJoin` fa) -> do
        E.on (bfa E.^. BookFolderAccountFolder E.==. fa E.^. FolderAccountId)
        E.where_ (bfa E.^. BookFolderAccountBook E.==. E.val bookId)
        E.orderBy [E.asc (bfa E.^. BookFolderAccountId)]
        return (fa, bfa E.^. BookFolderAccountIsDebit)

    -- Get each folder's tree.
    mapM (folderAccountTree . fmap E.unValue) bookFolders

folderAccountTree :: (Entity FolderAccount, Bool) -> ReaderT SqlBackend Handler AccountTree
folderAccountTree (folderE@(Entity fId _), isDebit) = do
    -- Get child folders.
    folders' <- selectList [FolderAccountParent ==. Just fId] []
    folders <- mapM (\f -> folderAccountTree (f, isDebit)) folders'

    -- Get child accounts.
    accounts' <- selectList [AccountParent ==. fId] []
    accounts <- mapM (accountLeaf isDebit) accounts'
    
    let balance = sum $ map accountLeafBalance accounts ++ map folderNodeBalance folders

    return $ FolderNode folderE balance isDebit $ folders ++ accounts

    where
        accountLeaf isDebit accountE@(Entity accountId _) = do
            -- Compute account balance.
            balance <- queryAccountBalance accountId

            return $ AccountLeaf accountE balance isDebit

        -- JP: Move to Import or Account?
        queryAccountBalance :: MonadHandler m => AccountId -> ReaderT SqlBackend m Nano
        queryAccountBalance aId = do
            res <- E.select $ E.from $ \a -> do
                E.where_ (a E.^. TransactionAccountAccount E.==. E.val aId)
                return $ E.sum_ (a E.^. TransactionAccountAmount)
            case res of
                [E.Value (Just x)] -> return x
                _ -> return 0
        
