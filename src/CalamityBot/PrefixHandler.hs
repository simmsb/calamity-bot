-- |
module CalamityBot.PrefixHandler
  ( useDatabasePrefix,
  )
where

import Calamity.Commands.ParsePrefix
import CalamityBot.Db
import Control.Lens
import qualified Data.Text.Lazy as L
import Polysemy
import Squeal.PostgreSQL

firstJusts :: [Maybe a] -> Maybe a
firstJusts (Just x : _) = Just x
firstJusts (_ : xs) = firstJusts xs
firstJusts [] = Nothing

useDatabasePrefix :: Member (DBEff DB) r => L.Text -> Sem (ParsePrefix ': r) a -> Sem r a
useDatabasePrefix def =
  interpret
    ( \case
        ParsePrefix m -> do
          prefixes <- case m ^. #guildID of
            Just gid -> (def :) <$> usingConn (execute (getPrefixes gid) >>= (fmap fromOnly <$>) . getRows)
            Nothing -> pure [def]
          pure $ firstJusts (map (\p -> (p,) <$> L.stripPrefix p (m ^. #content)) prefixes)
    )
