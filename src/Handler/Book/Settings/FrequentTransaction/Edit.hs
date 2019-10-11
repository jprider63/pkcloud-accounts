module Handler.Book.Settings.FrequentTransaction.Edit where

import qualified Book
import Import

generateHTML :: BookId -> FrequentTransactionId -> [AccountTree] -> Maybe (Widget, Enctype) -> Widget
generateHTML bId ftId trees formM = do
    -- setTitle $ toHtml ("Edit Frequent Transaction" :: Text)

    error "TODO"

getBookSettingsFrequentEditR :: BookId -> FrequentTransactionId -> Handler Html
getBookSettingsFrequentEditR bookId ftId = flip Book.layout bookId $ \(Entity bookId book) accountTree -> do
    generateHTML bookId ftId accountTree Nothing

postBookSettingsFrequentEditR :: BookId -> FrequentTransactionId -> Handler Html
postBookSettingsFrequentEditR = error "TODO"
