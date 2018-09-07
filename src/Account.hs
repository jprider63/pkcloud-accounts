module Account where

import qualified Database.Esqueleto as E
import Import

-- TODO: Cache this, make a map, or run DB queries?
-- Throws permission denied if account isn't in the book's account tree.
isDebit :: MonadHandler m => [AccountTree] -> AccountId -> m Bool
isDebit ts aId = case getAccountNode ts aId of
    Nothing ->
        notFound
    Just (AccountLeaf _ _ isDebit) ->
        return isDebit
    Just (FolderNode _ _ _ _) ->
        permissionDenied ""

getAccountNode :: [AccountTree] -> AccountId -> Maybe AccountTree
getAccountNode [] aId = Nothing
getAccountNode ((leaf@(AccountLeaf (Entity aId' _) _ _)):ts) aId | aId == aId' = Just leaf
getAccountNode ((AccountLeaf _ _ _):ts) aId = getAccountNode ts aId
getAccountNode ((FolderNode _ _ _ children):ts) aId = getAccountNode (children ++ ts) aId

queryBalance :: MonadHandler m => AccountId -> ReaderT SqlBackend m Nano
queryBalance aId = do
    res <- E.select $ E.from $ \a -> do
        E.where_ (a E.^. TransactionAccountAccount E.==. E.val aId)
        return $ E.sum_ (a E.^. TransactionAccountAmount)
    case res of
        [E.Value (Just x)] -> return x
        _ -> return 0
