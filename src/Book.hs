module Book where

import qualified Database.Esqueleto as E
import Import

canViewBook :: UserId -> Book -> Bool
canViewBook uId book = bookCreatedBy book == uId

layout :: (Book -> Text) -> (Entity Book -> Widget) -> BookId -> Handler Html
layout titleF w bookId = do
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
                        #{bookName book}
                        <small>
                            #{titleF book}
                <div .row>
                    <div .col-xs-3>
                        ^{sidebarW accountTree}
                    <div .col-xs-9>
                        ^{w $ Entity bookId book}
        |]

    where
        sidebarW :: [AccountTree] -> Widget
        sidebarW trees = [whamlet|
            <div .sidebar>
                <h2>
                    <a href="#">
                        Overview
                <h2>
                    Accounts
                <a href="#" .btn .pull-right>
                    New Account
                ^{concatMap sidebarAccountTree trees}
        |]

        sidebarAccountTree (FolderNode (Entity folderId folder) balance children) =
            [whamlet|
                <li>
                    #{folderAccountName folder} - #{dollar balance}
                    <ul>
                        ^{concatMap sidebarAccountTree children}
            |]
        sidebarAccountTree (AccountLeaf (Entity accountId account) balance) =
            [whamlet|
                <li>
                    #{accountName account} - #{dollar balance}
            |]

data AccountTree = 
      FolderNode {
        folderNode :: Entity FolderAccount
      , folderNodeBalance :: Nano
      , folderNodeChildren :: [AccountTree]
      }
    | AccountLeaf {
        accountLeaf :: Entity Account
      , accountLeafBalance :: Nano
      }

accountTrees :: BookId -> Handler [AccountTree]
accountTrees bookId = runDB $ do
    -- Get book folders.
    bookFolders <- E.select $ E.from $ \(bfa `E.InnerJoin` fa) -> do
        E.on (bfa E.^. BookFolderAccountFolder E.==. fa E.^. FolderAccountId)
        E.where_ (bfa E.^. BookFolderAccountBook E.==. E.val bookId)
        E.orderBy [E.asc (bfa E.^. BookFolderAccountId)]
        return fa

    -- Get each folder's tree.
    mapM (folderAccountTree) bookFolders

folderAccountTree :: Entity FolderAccount -> ReaderT SqlBackend Handler AccountTree
folderAccountTree folderE@(Entity fId _) = do
    -- Get child folders.
    folders' <- selectList [FolderAccountParent ==. Just fId] []
    folders <- mapM folderAccountTree folders'

    -- Get child accounts.
    accounts' <- selectList [AccountParent ==. Just fId] []
    accounts <- mapM accountLeaf accounts'
    
    let balance = sum $ map accountLeafBalance accounts ++ map folderNodeBalance folders

    return $ FolderNode folderE balance $ folders ++ accounts

    where
        accountLeaf accountE@(Entity accountId _) = do
            -- TODO: Compute account balance.
            let balance = 1
            return $ AccountLeaf accountE balance

