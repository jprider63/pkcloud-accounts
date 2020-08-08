module FrequentTransaction where

import qualified Account
import qualified Book
import Import

layout :: (Entity Book -> Entity FrequentTransaction -> [Entity FrequentTransactionAccount] -> [AccountTree] -> Widget) -> BookId -> FrequentTransactionId -> Handler Html
layout f bookId ftId = flip (Book.layout (error "TODO")) bookId $ \bookE accountTree -> do
    
    -- Check that transaction is in book.
    -- JP: We could just check one if we have the invariant that all accounts in the transaction belong to the same book.
    fts <- handlerToWidget $ runDB $ selectList [FrequentTransactionAccountTransaction ==. ftId] [Asc FrequentTransactionAccountId]
    Account.requireAllInBook accountTree $ map (\(Entity _ ta) -> frequentTransactionAccountAccount ta) fts -- $ take 1 ts

    -- Lookup frequent transacition.

    ft <- handlerToWidget $ runDB $ get404 ftId

    f bookE (Entity ftId ft) fts accountTree

