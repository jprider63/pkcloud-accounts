module Handler.Book.Settings where

import qualified Book
import Import

getBookSettingsR :: BookId -> Handler Html
getBookSettingsR = Book.layout (error "TODO") $ \(Entity bookId book) accountTree -> do
    setTitle $ toHtml ("Settings" :: Text)
    [whamlet|
        <h2>
            Settings
    |]
    frequentTransactionsW bookId

    where
        frequentTransactionsW bookId = do
            freqTransactions <- handlerToWidget $ runDB $ selectList [] [Asc FrequentTransactionId]
        
            [whamlet|
                <div>
                    <div .btn-toolbar .pull-right>
                        <div .btn-group>
                            <a href="@{BookSettingsFrequentCreateR bookId}" type="button" .btn .btn-primary .glyphicon .glyphicon-plus style="margin-top: -4px;">
                    <h3>
                        Frequent Transactions
                    
            |]

            case freqTransactions of
                [] -> 
                    [whamlet|
                        <p>
                            No frequent transactions currently exist.
                    |]
                _ -> 
                    [whamlet|
                        <ul .list-group>
                            ^{concatMap (frequentTransactionW bookId) freqTransactions}
                    |]

        frequentTransactionW bookId (Entity ftId FrequentTransaction{..}) = [whamlet|
                <a href="@{BookSettingsFrequentR bookId ftId}" .list-group-item>
                    #{frequentTransactionDescription}
            |]
                
                
                
