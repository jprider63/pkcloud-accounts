module Transaction where

import qualified Account
import qualified Book
import qualified Breadcrumb
import           Import
import qualified Database.Persist as P

layout :: PKCloudAccounts master => (Entity (Transaction master) -> Breadcrumb.CRUD (Transaction master)) -> (Entity (Book master) -> Entity (Transaction master) -> [Entity (TransactionAccount master)] -> [AccountTree master] -> WidgetFor master ()) -> BookId master -> TransactionId master -> Handler master Html
layout bc f bookId transactionId = do
    -- Lookup transaction.
    t <- liftHandler $ runDB $ get404 transactionId

    let transactionE = Entity transactionId t

    flip (Book.layout (Breadcrumb.Transaction $ bc transactionE)) bookId $ \bookE accountTree -> do
        -- Check that transaction is in book.
        -- JP: We could just check one if we have the invariant that all accounts in the transaction belong to the same book.
        ts <- handlerToWidget $ liftHandler $ runDB $ selectList [pkTransactionAccountTransactionField P.==. transactionId] [P.Asc pkTransactionAccountIdField]
        Account.requireAllInBook accountTree $ map (\(Entity _ ta) -> pkTransactionAccountAccount ta) ts -- $ take 1 ts
    
        f bookE transactionE ts accountTree

