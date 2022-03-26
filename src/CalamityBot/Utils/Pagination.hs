-- | Pagination helper functions
module CalamityBot.Utils.Pagination (
  formatPagination2,
  Pagination (..),
  PaginationDir (..),
  paginate,
  renderPaginationEmbed,
) where

import Calamity
import Control.Lens
import Data.Hourglass (Duration (Duration))
import Data.Maybe
import qualified Data.Text as T
import Polysemy (Sem, raise)
import qualified Polysemy as P
import qualified Polysemy.Fail as P
import qualified Polysemy.State as P
import Polysemy.Timeout
import Relude.Extra (bimapBoth)
import Text.Emoji
import Text.Layout.Table
import TextShow (showt)

formatPagination2 :: [String] -> [t] -> (t -> (Text, Text)) -> Text
formatPagination2 _ [] _ = codeline "No content"
formatPagination2 titles xs fmt =
  codeblock' Nothing $ T.pack linefmt
  where
    formattedLines = map fmt xs
    linefmt =
      tableString
        [column (expandUntil 20) right def def, column (expandUntil 80) left def def]
        unicodeRoundS
        (titlesH titles)
        ( map ((rowG . (\(a, b) -> [a, b])) . bimapBoth T.unpack) formattedLines
        )

data Pagination a = Pagination
  { page :: Int
  , content :: NonEmpty a
  }
  deriving (Generic, Show)

namedEmoji :: Text -> RawEmoji
namedEmoji = UnicodeEmoji . fromJust . emojiFromAlias

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

data PaginationDir a = MoveLeft a | MoveRight a | Initial

mkembedFooter :: Text -> Embed
mkembedFooter t = def & #footer ?~ EmbedFooter t Nothing Nothing

renderPaginationEmbed :: ([a] -> Embed) -> (Pagination a -> Embed)
renderPaginationEmbed f (Pagination page content) =
  let e = f $ toList content
   in e <> mkembedFooter ("Page " <> showt page)

paginate ::
  (BotC r, P.Member Timeout r, Tellable t, ToMessage m, Typeable a) =>
  -- | Getter function, Nothing = fetch initial page
  (PaginationDir a -> Sem r [a]) ->
  -- | Render function
  (Pagination a -> m) ->
  -- | Channel to send to
  t ->
  Sem r ()
paginate get render dest = (void . P.runFail) do
  Just content <- nonEmpty <$> raise (get Initial)
  let initP = Pagination 1 content
  Right msg <- tell dest . render $ initP
  invoke $ CreateReaction msg msg ArrowLeft
  invoke $ CreateReaction msg msg ArrowRight

  (timeoutDuration (Duration 1 0 0 0) . P.evalState initP . forever) do
    r <-
      waitUntil @ 'RawMessageReactionAddEvt
        ( \r ->
            (getID @Message r == getID msg)
              && (getID @User r /= getID msg)
              && ((r ^. #emoji) `elem` [ArrowLeft, ArrowRight])
        )

    s <- P.get
    let (p, c) = (s ^. #page, s ^. #content)
    (nextPage, action) <- case r ^. #emoji of
      ArrowLeft -> pure (p - 1, MoveLeft $ head c)
      ArrowRight -> pure (p + 1, MoveRight $ last c)
      _ -> fail "not possible"

    c' <- nonEmpty <$> (raise . raise) (get action)
    case c' of
      Just c' -> do
        let s' = Pagination nextPage c'
        let renderedMsg = appEndo (intoMsg $ render s') def
        invoke $
          EditMessage
            (getID @Channel msg)
            (getID @Message msg)
            ( editMessageContent (renderedMsg ^. #content)
                <> editMessageEmbed (renderedMsg ^. #embed)
            )
        P.put s'
      Nothing ->
        pure ()
    pure ()
