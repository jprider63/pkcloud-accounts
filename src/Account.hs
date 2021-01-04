module Account where

import qualified Book
import qualified Breadcrumb
import qualified Database.Esqueleto as E
import           Import.NoFoundation
import           Types
import           Types.Classes

-- TODO: Cache this, make a map, or run DB queries? Change type of account tree to be Map (Either FolderId AccountId) (Either Folder Debit, Bool)?? Then use recursive CTE query for folders/accounts?
-- Throws permission denied if account isn't in the book's account tree.
isDebit :: (PKCloudAccounts master, MonadHandler m) => [AccountTree master] -> AccountId master -> m Bool
isDebit ts aId = (\(_,_,x) -> x) <$> leaf ts aId

toAccountIds :: [AccountTree master] -> [AccountId master]
toAccountIds = concatMap helper
    where
        helper :: AccountTree master -> [AccountId master]
        helper (AccountLeaf (Entity aId _) _ _) = [aId]
        helper (FolderNode _ _ _ c) = toAccountIds c

leaf :: (PKCloudAccounts master, MonadHandler m) => [AccountTree master] -> AccountId  master-> m (Entity (Account master), Nano, Bool)
leaf ts aId = case getAccountNode ts aId of
    Nothing ->
        permissionDenied ""
    Just (AccountLeaf e b isDebit) ->
        return (e, b, isDebit)
    Just (FolderNode _ _ _ _) ->
        permissionDenied "Unreachable"


requireAllInBook :: (PKCloudAccounts master, MonadHandler m) => [AccountTree master] -> [AccountId master] -> m ()
requireAllInBook accountTree = mapM_ $ requireInBook accountTree

requireInBook :: (PKCloudAccounts master, MonadHandler m) => [AccountTree master] -> AccountId master -> m ()
requireInBook accountTree aId = 
    unless (isInBook accountTree aId) $
        permissionDenied ""

isInBook :: PKCloudAccounts master => [AccountTree master] -> AccountId master -> Bool
isInBook a = maybe False (const True) . getAccountNode a

amountToDebit :: Bool  -- | If account is debit.
    -> Nano -> Maybe Nano
amountToDebit True x | x >= 0 = Just x
amountToDebit False x | x < 0 = Just $ negate x
amountToDebit _ _ = Nothing


amountToCredit :: Bool -- | If account is debit.
    -> Nano -> Maybe Nano
amountToCredit True x | x < 0 = Just $ negate x
amountToCredit False x | x >= 0 = Just x
amountToCredit _ _ = Nothing

requireFolder :: (PKCloudAccounts master, MonadHandler m) => [AccountTree master] -> FolderAccountId master -> m (AccountTree master)
requireFolder a f = case getFolderNode a f of
    Just x -> return x
    Nothing -> permissionDenied ""

getFolderNode :: PKCloudAccounts master => [AccountTree master] -> FolderAccountId  master-> Maybe (AccountTree master)
getFolderNode [] _ = Nothing
getFolderNode (node@(FolderNode (Entity fId' _) _ _ _):ts) fId | fId == fId' = Just node
getFolderNode ((FolderNode _ _ _ children):ts) fId = getFolderNode (children ++ ts) fId
getFolderNode ((AccountLeaf _ _ _):ts) fId = getFolderNode ts fId

getAccountNode :: PKCloudAccounts master => [AccountTree master] -> AccountId master -> Maybe (AccountTree master)
getAccountNode [] aId = Nothing
getAccountNode ((leaf@(AccountLeaf (Entity aId' _) _ _)):ts) aId | aId == aId' = Just leaf
getAccountNode ((AccountLeaf _ _ _):ts) aId = getAccountNode ts aId
getAccountNode ((FolderNode _ _ _ children):ts) aId = getAccountNode (children ++ ts) aId

layout :: forall master . PKCloudAccounts master => (Entity (Account master) -> Breadcrumb.CRUD (Account master)) -> (Entity (Book master) -> Entity (Account master) -> Bool -> [AccountTree master] -> WidgetFor master ()) -> BookId master -> AccountId master -> Handler master Html
layout bc f bookId accountId = do
    account <- liftHandler $ runDB $ get404 accountId

    let accountE = Entity accountId account
    Book.layout (Breadcrumb.Account $ bc accountE) (w accountE) bookId

    where
        w accountE bookE@(Entity bookId book) accountTree = do
            -- Check if account is in book.
            unless (isInBook accountTree accountId) $ 
                permissionDenied ""
            
            -- Get account type.
            accountIsDebit <- Account.isDebit accountTree accountId

            -- CPS for widget.
            f bookE accountE accountIsDebit accountTree

displayTransactionRow :: (PKCloudAccounts master, GeneralizedTransactionAccount ta) => [AccountTree master] -> BookId master -> [((Maybe (Entity (Transaction master))), Entity ta, E.Value (Maybe Nano))] -> Widget master ()
displayTransactionRow a b x = displayTransactionRow' a b True x

displayTransactionRow' :: (PKCloudAccounts master, GeneralizedTransactionAccount ta) => [AccountTree master] -> BookId master -> Bool -> [(Maybe (Entity (Transaction master)), Entity ta, E.Value (Maybe Nano))] -> Widget master ()
displayTransactionRow' _ _ _ [] = mempty -- "No transactions"??
displayTransactionRow' accountTree bookId showAccountName (first:rest) = 
    let f = displayRow accountTree bookId in
    mconcat $ (f True first): map (f False) rest

    where
        displayRow accountTree bookId isFirst (transactionM, (Entity taId ta), (E.Value balanceM)) = do
            ((Entity aId a), _, accountIsDebit) <- Account.leaf accountTree $ gTransactionAccountAccount ta
            [whamlet|
                <tr .#{style}>
                    ^{desc}
                    ^{date}
                    ^{account aId a}
                    <td>
                        #{maybe "" dollar (Account.amountToDebit accountIsDebit $ gTransactionAccountAmount ta)}
                    <td>
                        #{maybe "" dollar (Account.amountToCredit accountIsDebit $ gTransactionAccountAmount ta)}
                    #{balanceH}
            |]

            where
                account aId a = if showAccountName then
                        [whamlet|
                            <td>
                                <a href="@{AccountR bookId aId}">
                                    #{pkAccountName a}
                        |]
                    else
                        mempty
                style = if isFirst then "transaction-first" else "transaction-rest" :: Text
                desc = case transactionM of
                    Nothing ->
                        mempty
                    Just (Entity tId t) ->
                        if isFirst then [whamlet|
                            <td>
                                <a href="@{TransactionR bookId tId}">
                                    #{pkTransactionDescription t}
                          |]
                        else [whamlet|
                            <td>
                          |]

                date = case (pkTransactionDate . entityVal) <$> transactionM of
                    Nothing -> 
                        mempty
                    Just d -> 
                        if isFirst then [whamlet|
                            <td>
                              #{shortDateTime d}
                          |] 
                        else
                          [whamlet|
                            <td>
                          |]
                        
                balanceH = maybe mempty (\d -> [shamlet|
                    <td>
                        #{dollar d}
                  |]) balanceM

-- Converts list of (abstract) transactions to entries for entries field.
transactionsToEntries :: (PKCloudAccounts master, GeneralizedTransactionAccount ta) => [AccountTree master] -> [Entity ta] -> Handler master [(Key (Account master), Either Nano Nano)]
transactionsToEntries trees = mapM $ \(Entity _ ta) -> do
    -- Get account type.
    let taa = gTransactionAccountAccount ta
    isDebit <- Account.isDebit trees taa

    return (taa, fromAmount isDebit $ gTransactionAccountAmount ta)

    where

        fromAmount True v | v >= 0 = Left v
        fromAmount True v = Right $ negate v
        fromAmount False v | v >= 0 = Right v
        fromAmount False v = Left $ negate v

