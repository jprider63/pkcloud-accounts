module Book where

import qualified Database.Esqueleto as E
import Import

canViewBook :: UserId -> Book -> Bool
canViewBook uId book = bookCreatedBy book == uId

layout :: (Entity Book -> Widget) -> BookId -> Handler Html
layout w bookId = do
    -- Check if user is authenticated.
    uId <- requireAuthId
    
    -- Lookup book.
    book <- runDB $ get404 bookId

    -- Check if user can view book.
    unless (canViewBook uId book) $ 
        permissionDenied ""

    defaultLayout $ w $ Entity bookId book

data AccountTree = 
      FolderNode (Entity FolderAccount) [AccountTree]
    | AccountLeaf (Entity Account)

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
    let accounts = map AccountLeaf accounts'

    return $ FolderNode folderE $ folders ++ accounts

