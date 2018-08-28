module Folder where

import qualified Book
import Import

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
