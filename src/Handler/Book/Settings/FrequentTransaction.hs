module Handler.Book.Settings.FrequentTransaction where

import qualified Database.Esqueleto as E

import qualified Account
import Import
import qualified FrequentTransaction

getBookSettingsFrequentR :: BookId -> FrequentTransactionId -> Handler Html
getBookSettingsFrequentR = FrequentTransaction.layout $ \(Entity bookId book) (Entity ftId ft) fts accountTree -> do
    setTitle $ toHtml ("Frequent Transaction" :: Text)
        
    let ts = zip3 (repeat Nothing) fts (repeat (E.Value (Nothing :: (Maybe Nano))))

    [whamlet|
        <a class="btn btn-primary pull-right" href="@{BookSettingsFrequentEditR bookId ftId}">
            Edit
        <a class="btn btn-danger pull-right" style="margin-right: 5px" href="@{BookSettingsFrequentDeleteR bookId ftId}">
            Delete
        <h2>
            Frequent Transaction
        <form>
            <div .form-group>
                <label>
                    Description
                <div>
                    <p .form-control-static>
                        #{frequentTransactionDescription ft}
            <div .form-group>
                <label>
                    Transactions
                <table .table .table-condensed .table-transactions>
                    <tr>
                        <th>
                            Description
                        <th>
                            Account
                        <th>
                            Debit
                        <th>
                            Credit
                    ^{Account.displayTransactionRow accountTree bookId ts}
    |]
