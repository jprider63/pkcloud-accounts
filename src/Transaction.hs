module Transaction where

import qualified Account
import qualified Book
import Import

layout :: (Entity Book -> Entity Transaction -> [AccountTree] -> Widget) -> BookId -> TransactionId -> Handler Html
layout f bookId transactionId = flip Book.layout bookId $ \bookE accountTree -> do
    -- Check that transaction is in book.
    -- JP: We could just check one if we have the invariant that all accounts in the transaction belong to the same book.
    ts <- handlerToWidget $ runDB $ selectList [TransactionAccountTransaction ==. transactionId] [Asc TransactionAccountId]
    Account.requireAllInBook accountTree $ map (\(Entity _ ta) -> transactionAccountAccount ta) ts -- $ take 1 ts
    
    -- Lookup transaction.
    t <- handlerToWidget $ runDB $ get404 transactionId

    f bookE (Entity transactionId t) accountTree

