module Account where

import Book (AccountTree(..))
import Import

-- TODO: Cache this, make a map, or run DB queries?
isDebit :: MonadHandler m => [AccountTree] -> AccountId -> m Bool
isDebit ts aId = case getAccountNode ts aId of
    Nothing ->
        notFound
    Just (AccountLeaf _ _ isDebit) ->
        return isDebit
    Just (FolderNode _ _ _ _) ->
        error "unreachable"

getAccountNode :: [AccountTree] -> AccountId -> Maybe AccountTree
getAccountNode [] aId = Nothing
getAccountNode ((leaf@(AccountLeaf (Entity aId' _) _ _)):ts) aId | aId == aId' = Just leaf
getAccountNode ((AccountLeaf _ _ _):ts) aId = getAccountNode ts aId
getAccountNode ((FolderNode _ _ _ children):ts) aId = getAccountNode (children ++ ts) aId
