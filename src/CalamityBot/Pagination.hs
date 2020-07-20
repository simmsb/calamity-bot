-- | Pagination helper functions
module CalamityBot.Pagination
  ( formatPagination,
    formatPagination2,
    Pagination (..),
    paginate,
  )
where

import Calamity (BotC, Channel, ChannelRequest (..), EventType (RawMessageReactionAddEvt), Message, RawEmoji (..), Tellable, ToMessage, getID, intoMsg, invoke, tell, waitUntil)
import Calamity.Utils
import Control.Lens
import qualified Data.Text.Lazy as L
import Data.Maybe
import Polysemy (Sem, raise)
import qualified Polysemy.Fail as P
import Relude.Extra (bimapBoth)
import qualified Polysemy.State as P
import Text.Emoji
import Text.Layout.Table

formatPagination :: Int -> Int -> [(t, Int)] -> (t -> LText) -> LText
formatPagination _ _ [] _ = codeline "No content"
formatPagination idx width xs@((_, total) : _) fmt =
  L.unlines
    [ codeblock' Nothing $ L.pack linefmt,
      "Page " <> show (idx + 1) <> " of " <> show totalPages <> " (total " <> show total <> " rows)"
    ]
  where
    formattedLines = map (fmt . fst) xs
    totalPages = (total + width - 1) `div` width
    linefmt =
      tableString
        [column def right def def, column (expandUntil 80) left def def]
        unicodeRoundS
        def
        (map (rowG . biList) $ zip (map show [(width * idx) ..]) (map L.unpack formattedLines))

formatPagination2 :: Int -> Int -> [(t, Int)] -> (t -> (LText, L.Text)) -> LText
formatPagination2 _ _ [] _ = codeline "No content"
formatPagination2 idx width xs@((_, total) : _) fmt =
  L.unlines
    [ codeblock' Nothing $ L.pack linefmt,
      "Page " <> show (idx + 1) <> " of " <> show totalPages <> " (total " <> show total <> " rows)"
    ]
  where
    formattedLines = map (fmt . fst) xs
    totalPages = (total + width - 1) `div` width
    linefmt =
      tableString
        [column def right def def, column (expandUntil 20) right def def, column (expandUntil 80) left def def]
        unicodeRoundS
        def
        ( map (rowG . (\(a, (b, c)) -> [a, b, c])) $
            zip
              (map show [(width * idx) ..])
              (map (bimapBoth L.unpack) formattedLines)
        )

data Pagination a = Pagination
  { page :: Int,
    content :: NonEmpty a
  }
  deriving (Generic, Show)

namedEmoji :: Text -> RawEmoji
namedEmoji = UnicodeEmoji . L.fromStrict . fromJust . emojiFromAlias

pattern ArrowLeft :: RawEmoji
pattern ArrowLeft <-
  ((== namedEmoji "arrow_left") -> True)
  where
    ArrowLeft = namedEmoji "arrow_left"

pattern ArrowRight :: RawEmoji
pattern ArrowRight <-
  ((== namedEmoji "arrow_right") -> True)
  where
    ArrowRight = namedEmoji "arrow_right"

data PaginationDir a = MoveLeft a | MoveRight a

paginate ::
  (BotC r, Tellable t, ToMessage m, Typeable a) =>
  -- | Getter function, Nothing = fetch initial page
  (Maybe (PaginationDir a) -> Sem r [a]) ->
  -- | Render function
  (Pagination a -> m) ->
  -- | Channel to send to
  t ->
  Sem r ()
paginate get render dest = (void . P.runFail) do
  Just content <- nonEmpty <$> raise (get Nothing)
  let initP = Pagination 1 content
  Right msg <- tell dest . render $ initP
  invoke $ CreateReaction msg msg ArrowLeft
  invoke $ CreateReaction msg msg ArrowRight

  -- TODO: timeout
  (P.evalState initP . forever) do
    r <-
      waitUntil @'RawMessageReactionAddEvt
        ( \r ->
            (getID @Message r == getID msg)
              && ((r ^. #emoji) `elem` [ArrowLeft, ArrowRight])
        )

    s <- P.get
    let (p, c) = (s ^. #page, s ^. #content)
    (nextPage, action) <- case r ^. #emoji of
      ArrowLeft -> pure (p - 1, MoveLeft $ head c)
      ArrowRight -> pure (p + 1, MoveRight $ last c)
      _ -> fail "not possible"

    c' <- nonEmpty <$> (raise . raise) (get $ Just action)
    case c' of
      Just c' -> do
        let s' = Pagination nextPage c'
        let renderedMsg = appEndo (intoMsg $ render s') def
        invoke $ EditMessage (getID @Channel msg) (getID @Message msg) (renderedMsg ^. #content) (renderedMsg ^. #embed)
        P.put s'
      Nothing ->
        -- TODO: stuff for showing lack of new pages, etc
        pure ()
    pure ()
