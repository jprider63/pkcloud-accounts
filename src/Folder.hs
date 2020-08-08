module Folder where

import qualified Account
import qualified Book
import qualified Breadcrumb
import           Import

treesToFolders :: [AccountTree] -> [(Text, FolderAccountId)]
treesToFolders trees = concatMap (toFolders "") trees
    where
        toFolders :: Text -> AccountTree -> [(Text, FolderAccountId)]
        toFolders spacing (FolderNode (Entity folderId folder) _ _ children) = 
            let spacing' = spacingChar <> spacing in
            (spacing <> folderAccountName folder, folderId):(concatMap (toFolders spacing') children)
        toFolders _ (AccountLeaf _ _ _) = []

        spacingChar = "-"

treesToAccounts :: [AccountTree] -> [(Text, AccountId)]
treesToAccounts trees = concatMap (toAccounts "") trees
    where
        toAccounts :: Text -> AccountTree -> [(Text, AccountId)]
        toAccounts path (FolderNode (Entity _ folder) _ _ children) = 
            let path' = path <> folderAccountName folder <> spacingChar in
            concatMap (toAccounts path') children
        toAccounts path (AccountLeaf (Entity accountId account) _ _) = 
            [(path <> accountName account, accountId)]

        spacingChar = ":"

treesToShadows :: [AccountTree] -> [(AccountId, AccountId)]
treesToShadows trees = concatMap toShadows trees
  where
    toShadows (FolderNode _ _ _ children) = concatMap toShadows children
    toShadows (AccountLeaf (Entity accountId account) _ _) | Just shadowAccountId <- accountShadow account = [(accountId, shadowAccountId)]
    toShadows (AccountLeaf _ _ _) = []

layout :: (Entity FolderAccount -> Breadcrumb.CRUD FolderAccount) -> (Entity Book -> [AccountTree] -> AccountTree -> Widget) -> BookId -> FolderAccountId -> Handler Html
layout bc f bookId fId = do
  -- JP: This lookup is redundant...
  folder <- runDB $ get404 fId
  let folderE = Entity fId folder
  
  flip (Book.layout (Breadcrumb.Folder $ bc folderE)) bookId $ \bookE accountTree -> do
    -- Load folder (and check that folder is in book).
    folder <- Account.requireFolder accountTree fId

    f bookE accountTree folder
