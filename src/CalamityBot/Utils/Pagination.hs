{-# LANGUAGE ApplicativeDo #-}

-- | Pagination helper functions
module CalamityBot.Utils.Pagination (
  formatPagination2,
  Pagination (..),
  PaginationDir (..),
  paginate,
  renderPaginationEmbed,
) where

import Calamity
import Calamity.Interactions qualified as I
import Control.Monad (void)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Hourglass (Duration (Duration))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Monoid (Endo (..))
import Data.Text qualified as T
import GHC.Generics
import Optics
import Polysemy qualified as P
import Polysemy.Fail qualified as P
import Polysemy.State qualified as P
import Polysemy.Timeout
import Text.Emoji
import Text.Layout.Table
import TextShow (showt)

bimapBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
bimapBoth f = bimap f f

formatPagination2 :: [String] -> [t] -> (t -> (T.Text, T.Text)) -> T.Text
formatPagination2 _ [] _ = codeline "No content"
formatPagination2 titles xs fmt =
  codeblock' Nothing $ T.pack linefmt
  where
    formattedLines = fmap fmt xs
    linefmt =
      tableString
        [column (expandUntil 20) right def def, column (expandUntil 80) left def def]
        unicodeRoundS
        (titlesH titles)
        ( fmap ((rowG . (\(a, b) -> [a, b])) . bimapBoth T.unpack) formattedLines
        )

data Pagination a = Pagination
  { page :: Int
  , content :: NE.NonEmpty a
  }
  deriving (Generic, Show)

namedEmoji :: T.Text -> RawEmoji
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

mkembedFooter :: T.Text -> Embed
mkembedFooter t = def & #footer ?~ EmbedFooter t Nothing Nothing

renderPaginationEmbed :: ([a] -> Embed) -> (Pagination a -> Embed)
renderPaginationEmbed f (Pagination page content) =
  let e = f $ NE.toList content
   in e <> mkembedFooter ("Page " <> showt page)

paginate ::
  (BotC r, P.Member Timeout r, Tellable t, ToMessage m) =>
  -- | Getter function, Nothing = fetch initial page
  (PaginationDir a -> P.Sem r [a]) ->
  -- | Render function
  (Pagination a -> m) ->
  -- | Channel to send to
  t ->
  P.Sem r ()
paginate get render dest = (void . P.runFail) do
  Just content <- NE.nonEmpty <$> P.raise (get Initial)
  let initP = Pagination 1 content
  Right msg <- tell dest . render $ initP

  let pagView = I.row do
        bw <- I.button' (#emoji ?~ ArrowLeft)
        fw <- I.button' (#emoji ?~ ArrowRight)
        pure (bw, fw)

  let cTell dest st comp = tell dest (intoMsg comp <> intoMsg (render st))

  timeoutDuration (Duration 1 0 0 0) . P.evalState initP . I.runView pagView (cTell dest initP) $ \(bw, fw) -> do
    I.deferComponent
    s <- P.get
    let (p, c) = (s ^. #page, s ^. #content)
    (nextPage, action) <- case (bw, fw) of
      (True, False) -> pure (p - 1, MoveLeft $ NE.head c)
      (False, True) -> pure (p + 1, MoveRight $ NE.last c)
      _ -> fail "not possible"

    c' <- NE.nonEmpty <$> P.raise_ (get action)
    case c' of
      Just c' -> do
        let s' = Pagination nextPage c'
        let renderedMsg = appEndo (intoMsg $ render s') def
        invoke $
          EditMessage
            (getID @Channel msg)
            (getID @Message msg)
            ( editMessageContent (renderedMsg ^. #content)
                <> maybe mempty editMessageEmbeds (renderedMsg ^. #embeds)
            )
        P.put s'
      Nothing ->
        pure ()
    pure ()
