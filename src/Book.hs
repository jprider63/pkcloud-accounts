module Book where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Esqueleto as E

-- import qualified Account
import           Breadcrumb (Breadcrumb, breadcrumbs)
import           Import.NoFoundation
import           Types

_lastOpenedBookKey :: Text
_lastOpenedBookKey = "_pkcloud_accounts_lastopened"

setLastOpened :: (PKCloudAccounts master, MonadHandler m, m ~ HandlerFor master) => BookId master -> m ()
setLastOpened bookId = 
    setSessionBS _lastOpenedBookKey $ BSL.toStrict $ Aeson.encode bookId

getLastOpened :: HandlerFor master (Maybe (BookId master))
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

canViewBook :: AuthId master -> Book master -> Bool
canViewBook uId book = bookCreatedBy book == uId

layout :: Breadcrumb master -> (Entity (Book master) -> [AccountTree master] -> WidgetFor master ()) -> BookId master -> HandlerFor master Html
layout breadcrumb w bookId = do
    -- Check if user is authenticated.
    uId <- requireAuthId
    
    -- Lookup book.
    book <- runDB $ get404 bookId

    -- Check if user can view book.
    unless (canViewBook uId book) $ 
        permissionDenied ""

    accountTree <- accountTrees bookId

    let bookE = Entity bookId book
    defaultLayout $ [whamlet|
            <div .container>
                <div .row>
                    <div .col-sm-9>
                        <ol .breadcrumb>
                            ^{breadcrumbW (breadcrumbs bookE breadcrumb)}
                        ^{w bookE accountTree}
                    <div .col-sm-3>
                        ^{sidebarW accountTree}
        |]
                -- <div .row>
                --     <div .col-xs-12>
                --         <h1>
                --             #{bookName book}

    where

        breadcrumbW [] = mempty
        breadcrumbW [(title, _)] = [whamlet|
            <li .active>
              #{title}
          |]
        breadcrumbW ((title, link):bs) = [whamlet|
            <li>
              <a href="@{link}">
                #{title}
          |] <> breadcrumbW bs

        -- breadcrumbW book = [whamlet|
        --     <li><a href="#">Home</a></li>
        --     <li><a href="#">Library</a></li>
        --     <li class="active">Data</li>
        -- |]
        sidebarW :: [AccountTree master] -> WidgetFor master ()
        sidebarW trees = [whamlet|
            <div .sidebar>
                <a .btn .btn-primary .btn-block href="@{TransactionCreateR bookId}">
                    New Transaction
                <a href="@{AccountCreateR bookId}" .btn .btn-primary .btn-block>
                    New Account
                <a href="@{FolderCreateR bookId}" .btn .btn-primary .btn-block>
                    New Folder
                <a href="@{BookSettingsR bookId}" .btn .btn-default .btn-block>
                    Settings
                <a href="@{BooksR}" .btn .btn-default .btn-block>
                    Other Books
        |]
        -- Book Settings?
                -- <h2>
                --     <a href="@{BookR bookId}">
                --         Overview
                -- <h2>
                --     Accounts
                -- ^{displayAccountTrees bookId trees}

accountTrees :: BookId master -> HandlerFor master [AccountTree master]
accountTrees bookId = runDB $ do
    -- Get book folders.
    bookFolders <- E.select $ E.from $ \(bfa `E.InnerJoin` fa) -> do
        E.on (bfa E.^. BookFolderAccountFolder E.==. fa E.^. FolderAccountId)
        E.where_ (bfa E.^. BookFolderAccountBook E.==. E.val bookId)
        E.orderBy [E.asc (bfa E.^. BookFolderAccountId)]
        return (fa, bfa E.^. BookFolderAccountIsDebit)

    -- Get each folder's tree.
    mapM (folderAccountTree . fmap E.unValue) bookFolders

folderAccountTree :: (Entity (FolderAccount master), Bool) -> ReaderT SqlBackend (HandlerFor master) (AccountTree master)
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
        queryAccountBalance :: MonadHandler m => AccountId master -> ReaderT SqlBackend m Nano
        queryAccountBalance aId = do
            res <- E.select $ E.from $ \a -> do
                E.where_ (a E.^. TransactionAccountAccount E.==. E.val aId)
                return $ E.sum_ (a E.^. TransactionAccountAmount)
            case res of
                [E.Value (Just x)] -> return x
                _ -> return 0
        
displayAccountTrees :: PKCloudAccounts master => BookId master -> [AccountTree master] -> WidgetFor (PKCloudAccountsApp master) ()
displayAccountTrees bookId trees = [whamlet|
        <ul .list-group .list-group-collapse>
            ^{mconcat $ map sidebarAccountTree trees}
    |]

    where
        
        -- https://stackoverflow.com/a/33571268
        sidebarAccountTree (FolderNode (Entity folderId folder) balance isDebit children) =
            [whamlet|
                <a .list-group-item href="@{FolderR bookId folderId}">
                    #{pkFolderAccountName folder}
                    <span .badge .badge-balance>
                        #{dollar balance}
                <ul .list-group>
                    ^{mconcat $ map sidebarAccountTree children}
            |]
        sidebarAccountTree (AccountLeaf (Entity accountId account) balance isDebit) =
            [whamlet|
                <a .list-group-item href="@{AccountR bookId accountId}">
                    #{pkAccountName account}
                    <span .badge .badge-balance>
                        #{dollar balance}
            |]

