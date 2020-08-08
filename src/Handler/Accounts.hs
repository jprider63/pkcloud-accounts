module Handler.Accounts where

import qualified Book
import qualified Breadcrumb
import           Import

getAccountsR :: BookId -> Handler Html
getAccountsR = Book.layout Breadcrumb.Accounts $ \(Entity bookId book) accountTree -> do
    setTitle $ toHtml ("Accounts" :: Text)

    [whamlet|
            <h2>
                Accounts
            <div>
                ^{Book.displayAccountTrees bookId accountTree}
    |]

