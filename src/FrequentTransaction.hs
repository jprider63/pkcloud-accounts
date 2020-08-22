module FrequentTransaction where

import qualified Account
import qualified Book
import qualified Breadcrumb
import           Import

layout :: (Entity FrequentTransaction -> Breadcrumb.CRUD FrequentTransaction) -> (Entity Book -> Entity FrequentTransaction -> [Entity FrequentTransactionAccount] -> [AccountTree] -> Widget) -> BookId -> FrequentTransactionId -> Handler Html
layout bc f bookId ftId = do
  -- Lookup frequent transacition.
  ft <- runDB $ get404 ftId
  let ftE = Entity ftId ft

  flip (Book.layout (Breadcrumb.FrequentTransaction $ bc ftE)) bookId $ \bookE accountTree -> do
    
    -- Check that transaction is in book.
    -- JP: We could just check one if we have the invariant that all accounts in the transaction belong to the same book.
    fts <- handlerToWidget $ runDB $ selectList [FrequentTransactionAccountTransaction ==. ftId] [Asc FrequentTransactionAccountId]
    Account.requireAllInBook accountTree $ map (\(Entity _ ta) -> frequentTransactionAccountAccount ta) fts -- $ take 1 ts

    f bookE ftE fts accountTree

