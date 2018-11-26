module Handler.Folder.Edit where

import qualified Book
import Import

generateHTML :: BookId -> FolderAccountId -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bookId folderId trees formM = do
    setTitle $ toHtml ("Edit Folder" :: Text)

getFolderEditR :: Key Book -> Key FolderAccount -> Handler Html
getFolderEditR bookId folderId = flip Book.layout bookId $ \(Entity bookId book) accountTree ->
    generateHTML bookId folderId accountTree Nothing

postFolderEditR :: Key Book -> Key FolderAccount -> Handler Html
postFolderEditR bookId folderId = flip Book.layout bookId $ \(Entity bookId book) accountTree ->
    
    undefined
