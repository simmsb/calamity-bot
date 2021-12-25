-- |
module CalamityBot.PrefixHandler
  ( useDatabasePrefix,
  )
where

import Calamity (Message)
import CalamityCommands.ParsePrefix (ParsePrefix(..))
import CalamityBot.Db
import Control.Lens
import qualified Data.Text as T
import Polysemy
import Database.Beam (runSelectReturningList)

firstJusts :: [Maybe a] -> Maybe a
firstJusts (Just x : _) = Just x
firstJusts (_ : xs) = firstJusts xs
firstJusts [] = Nothing

useDatabasePrefix :: Member DBEff r => T.Text -> Sem (ParsePrefix Message ': r) a -> Sem r a
useDatabasePrefix def =
  interpret \case
    ParsePrefix m -> do
      prefixes <- case m ^. #guildID of
        Just gid -> (def :) <$> usingConn (runSelectReturningList $ getPrefixes' gid)
        Nothing -> pure [def]
      pure $ firstJusts (map (\p -> (p,) <$> T.stripPrefix p (m ^. #content)) prefixes)
