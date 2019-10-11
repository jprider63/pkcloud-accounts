module Handler.Book.Settings.FrequentTransaction.Delete where

import qualified Book
import qualified FrequentTransaction
import Import

getBookSettingsFrequentDeleteR :: BookId -> FrequentTransactionId -> Handler Html
getBookSettingsFrequentDeleteR = FrequentTransaction.layout $ \(Entity bookId _) (Entity ftId _) _ accountTree ->
    -- generateHtml bookId ftId accountTree Nothing
    error "TODO"

postBookSettingsFrequentDeleteR :: BookId -> FrequentTransactionId -> Handler Html
postBookSettingsFrequentDeleteR = FrequentTransaction.layout $ \(Entity bookId book) (Entity ftId _) _ accountTree -> do
    -- Check that user can write to book.
    handlerToWidget $ Book.requireCanWriteBook book


