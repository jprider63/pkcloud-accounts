module Transaction where

import qualified Account
import qualified Book
import qualified Breadcrumb
import Import

layout :: (Entity Transaction -> Breadcrumb.CRUD Transaction) -> (Entity Book -> Entity Transaction -> [Entity TransactionAccount] -> [AccountTree] -> Widget) -> BookId -> TransactionId -> Handler Html
layout bc f bookId transactionId = do
    -- Lookup transaction.
    t <- runDB $ get404 transactionId

    let transactionE = Entity transactionId t

    flip (Book.layout (Breadcrumb.Transaction $ bc transactionE)) bookId $ \bookE accountTree -> do
        -- Check that transaction is in book.
        -- JP: We could just check one if we have the invariant that all accounts in the transaction belong to the same book.
        ts <- handlerToWidget $ runDB $ selectList [TransactionAccountTransaction ==. transactionId] [Asc TransactionAccountId]
        Account.requireAllInBook accountTree $ map (\(Entity _ ta) -> transactionAccountAccount ta) ts -- $ take 1 ts
    
        f bookE transactionE ts accountTree

