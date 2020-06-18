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
import Database.Beam (runSelectReturningList)

firstJusts :: [Maybe a] -> Maybe a
firstJusts (Just x : _) = Just x
firstJusts (_ : xs) = firstJusts xs
firstJusts [] = Nothing

useDatabasePrefix :: Member DBEff r => L.Text -> Sem (ParsePrefix ': r) a -> Sem r a
useDatabasePrefix def =
  interpret \case
    ParsePrefix m -> do
      prefixes <- case m ^. #guildID of
        Just gid -> (def :) <$> usingConn (runSelectReturningList $ getPrefixes' gid)
        Nothing -> pure [def]
      pure $ firstJusts (map (\p -> (p,) <$> L.stripPrefix p (m ^. #content)) prefixes)
