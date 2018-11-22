module Book where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Esqueleto as E

-- import qualified Account
import Import

_lastOpenedBookKey :: Text
_lastOpenedBookKey = "_pkcloud_accounts_lastopened"

setLastOpened :: MonadHandler m => BookId -> m ()
setLastOpened bookId = 
    setSessionBS _lastOpenedBookKey $ BSL.toStrict $ Aeson.encode bookId

getLastOpened :: Handler (Maybe BookId)
getLastOpened = do
    -- Get current user.
    userId <- requireAuthId

    -- Lookup latest book.
    bookIdM <- (>>= Aeson.decodeStrict) <$> lookupSessionBS _lastOpenedBookKey
    case bookIdM of
        Nothing ->
            return Nothing
        Just bookId -> do
            book <- runDB $ get404 bookId

            -- Make sure user can read book.
            return $ if canViewBook userId book then
                Just bookId
            else
                Nothing

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
                    <div .col-xs-12>
                        <h1>
                            #{bookName book}
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
                ^{displayAccountTrees bookId trees}
                <a .btn .btn-primary .btn-block href="@{TransactionCreateR bookId}">
                    New Transaction
                <a href="@{AccountCreateR bookId}" .btn .btn-primary .btn-block>
                    New Account
                <a href="@{FolderCreateR bookId}" .btn .btn-primary .btn-block>
                    New Folder
                <a href="@{BooksR}" .btn .btn-default .btn-block>
                    Other Books
        |]
        -- Book Settings?

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
        
displayAccountTrees :: BookId -> [AccountTree] -> Widget
displayAccountTrees bookId trees = [whamlet|
        <ul .list-group .list-group-collapse>
            ^{concatMap sidebarAccountTree trees}
    |]

    where
        
        -- https://stackoverflow.com/a/33571268
        sidebarAccountTree (FolderNode (Entity folderId folder) balance isDebit children) =
            [whamlet|
                <a .list-group-item href="#TODO">
                    #{folderAccountName folder}
                    <span .badge .badge-balance>
                        #{dollar balance}
                <ul .list-group>
                    ^{concatMap sidebarAccountTree children}
            |]
        sidebarAccountTree (AccountLeaf (Entity accountId account) balance isDebit) =
            [whamlet|
                <a .list-group-item href="@{AccountR bookId accountId}">
                    #{accountName account}
                    <span .badge .badge-balance>
                        #{dollar balance}
            |]

