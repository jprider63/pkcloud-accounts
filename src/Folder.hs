module Folder where

import qualified Book
import Import

treesToFolders :: [Book.AccountTree] -> [(Text, FolderAccountId)]
treesToFolders trees = concatMap (toFolders "") trees
    where
        toFolders :: Text -> Book.AccountTree -> [(Text, FolderAccountId)]
        toFolders spacing (Book.FolderNode (Entity folderId folder) _ _ children) = 
            let spacing' = spacingChar <> spacing in
            (spacing <> folderAccountName folder, folderId):(concatMap (toFolders spacing') children)
        toFolders _ (Book.AccountLeaf _ _ _) = []

        spacingChar = "-"

treesToAccounts :: [Book.AccountTree] -> [(Text, AccountId)]
treesToAccounts trees = concatMap (toAccounts "") trees
    where
        toAccounts :: Text -> Book.AccountTree -> [(Text, AccountId)]
        toAccounts path (Book.FolderNode (Entity _ folder) _ _ children) = 
            let path' = path <> folderAccountName folder <> spacingChar in
            concatMap (toAccounts path') children
        toAccounts path (Book.AccountLeaf (Entity accountId account) _ _) = 
            [(path <> accountName account, accountId)]

        spacingChar = ":"
