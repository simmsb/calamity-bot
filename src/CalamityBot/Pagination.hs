-- | Pagination helper functions
module CalamityBot.Pagination
  ( formatPagination,
    formatPagination2,
  )
where

import Calamity.Utils
import qualified Data.Text.Lazy as L
import Text.Layout.Table
import Relude.Extra (bimapBoth)

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
