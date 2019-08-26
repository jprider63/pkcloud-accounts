module Account where

import qualified Book
import qualified Database.Esqueleto as E
import Import
import Types.Classes

-- TODO: Cache this, make a map, or run DB queries? Change type of account tree to be Map (Either FolderId AccountId) (Either Folder Debit, Bool)?? Then use recursive CTE query for folders/accounts?
-- Throws permission denied if account isn't in the book's account tree.
isDebit :: MonadHandler m => [AccountTree] -> AccountId -> m Bool
isDebit ts aId = (\(_,_,x) -> x) <$> leaf ts aId

toAccountIds :: [AccountTree] -> [AccountId]
toAccountIds = concatMap helper
    where
        helper :: AccountTree -> [AccountId]
        helper (AccountLeaf (Entity aId _) _ _) = [aId]
        helper (FolderNode _ _ _ c) = toAccountIds c

leaf :: MonadHandler m => [AccountTree] -> AccountId -> m (Entity Account, Nano, Bool)
leaf ts aId = case getAccountNode ts aId of
    Nothing ->
        permissionDenied ""
    Just (AccountLeaf e b isDebit) ->
        return (e, b, isDebit)
    Just (FolderNode _ _ _ _) ->
        permissionDenied "Unreachable"


requireAllInBook :: MonadHandler m => [AccountTree] -> [AccountId] -> m ()
requireAllInBook accountTree = mapM_ $ requireInBook accountTree

requireInBook :: MonadHandler m => [AccountTree] -> AccountId -> m ()
requireInBook accountTree aId = 
    unless (isInBook accountTree aId) $
        permissionDenied ""

isInBook :: [AccountTree] -> AccountId -> Bool
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

requireFolder :: MonadHandler m => [AccountTree] -> FolderAccountId -> m AccountTree
requireFolder a f = case getFolderNode a f of
    Just x -> return x
    Nothing -> permissionDenied ""

getFolderNode :: [AccountTree] -> FolderAccountId -> Maybe AccountTree
getFolderNode [] _ = Nothing
getFolderNode (node@(FolderNode (Entity fId' _) _ _ _):ts) fId | fId == fId' = Just node
getFolderNode ((FolderNode _ _ _ children):ts) fId = getFolderNode (children ++ ts) fId
getFolderNode ((AccountLeaf _ _ _):ts) fId = getFolderNode ts fId

getAccountNode :: [AccountTree] -> AccountId -> Maybe AccountTree
getAccountNode [] aId = Nothing
getAccountNode ((leaf@(AccountLeaf (Entity aId' _) _ _)):ts) aId | aId == aId' = Just leaf
getAccountNode ((AccountLeaf _ _ _):ts) aId = getAccountNode ts aId
getAccountNode ((FolderNode _ _ _ children):ts) aId = getAccountNode (children ++ ts) aId

layout :: (Entity Book -> Entity Account -> Bool -> [AccountTree] -> Widget) -> BookId -> AccountId -> Handler Html
layout f bookId accountId = do
    account <- runDB $ get404 accountId

    Book.layout (w account) bookId

    where
        w account bookE@(Entity bookId book) accountTree = do
            -- Check if account is in book.
            unless (isInBook accountTree accountId) $ 
                permissionDenied ""
            
            -- Get account type.
            accountIsDebit <- Account.isDebit accountTree accountId

            -- CPS for widget.
            f bookE (Entity accountId account) accountIsDebit accountTree

displayTransactionRow :: (GeneralizedTransaction t, GeneralizedTransactionAccount ta) => [AccountTree] -> BookId -> [(Entity t, Entity ta, E.Value (Maybe Nano))] -> Widget
displayTransactionRow a b x = displayTransactionRow' a b True x

displayTransactionRow' :: (GeneralizedTransaction t, GeneralizedTransactionAccount ta) => [AccountTree] -> BookId -> Bool -> [(Entity t, Entity ta, E.Value (Maybe Nano))] -> Widget
displayTransactionRow' _ _ _ [] = mempty -- "No transactions"??
displayTransactionRow' accountTree bookId showAccountName (first:rest) = 
    let f = displayRow accountTree bookId in
    mconcat $ (f True first): map (f False) rest

    where
        displayRow accountTree bookId displayDescription ((Entity tId t), (Entity taId ta), (E.Value balanceM)) = do
            ((Entity aId a), _, accountIsDebit) <- Account.leaf accountTree $ gTransactionAccountAccount ta
            let balanceH = maybe mempty (\d -> [shamlet|
                    <td>
                        #{dollar d}
                  |]) balanceM
            [whamlet|
                <tr .#{style}>
                    <td>
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
                                    #{accountName a}
                        |]
                    else
                        mempty
                style = if displayDescription then "transaction-first" else "transaction-rest" :: Text
                desc = if displayDescription then [whamlet|
                          <a href="@{gTransactionRoute bookId tId}">
                              #{gTransactionDescription t}
                        |]
                       else
                         mempty

                date = case gTransactionDate t of
                    Nothing -> 
                        mempty
                    Just d -> 
                        if displayDescription then [whamlet|
                            <td>
                              #{shortDateTime d}
                          |] 
                        else
                          [whamlet|
                            <td>
                          |]
                        
