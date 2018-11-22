module Account where

import qualified Book
import qualified Database.Esqueleto as E
import Import

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
requireAllInBook accountTree = mapM_ $ \aId ->
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

getAccountNode :: [AccountTree] -> AccountId -> Maybe AccountTree
getAccountNode [] aId = Nothing
getAccountNode ((leaf@(AccountLeaf (Entity aId' _) _ _)):ts) aId | aId == aId' = Just leaf
getAccountNode ((AccountLeaf _ _ _):ts) aId = getAccountNode ts aId
getAccountNode ((FolderNode _ _ _ children):ts) aId = getAccountNode (children ++ ts) aId

layout :: (Entity Book -> Entity Account -> [AccountTree] -> Widget) -> BookId -> AccountId -> Handler Html
layout f bookId accountId = do
    account <- runDB $ get404 accountId

    Book.layout (w account) bookId

    where
        w account bookE@(Entity bookId book) accountTree = do
            -- Check if account is in book.
            unless (isInBook accountTree accountId) $ 
                permissionDenied ""
            
            -- CPS for widget.
            f bookE (Entity accountId account) accountTree

