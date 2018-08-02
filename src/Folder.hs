module Folder where

import qualified Book
import Import

treesToFolders :: MonadHandler m => [Book.AccountTree] -> m (OptionList FolderAccountId)
treesToFolders trees = optionsPairs $ concatMap (toFolders "") trees
    where
        toFolders :: Text -> Book.AccountTree -> [(Text, FolderAccountId)]
        toFolders spacing (Book.FolderNode (Entity folderId folder) _ children) = 
            let spacing' = spacingChar <> spacing in
            (spacing <> folderAccountName folder, folderId):(concatMap (toFolders spacing') children)
        toFolders _ (Book.AccountLeaf _ _) = []

        spacingChar = "-"
