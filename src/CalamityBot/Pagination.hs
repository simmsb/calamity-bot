-- | Pagination helper functions
module CalamityBot.Pagination
  ( formatPagination,
  )
where

import Calamity.Utils
import qualified Data.Text.Lazy as L
import Text.Layout.Table

formatPagination :: Int -> Int -> [(t, Int)] -> (t -> LText) -> LText
formatPagination _ _ [] _ = codeline "No content"
formatPagination idx width xs@((_, total) : _) fmt =
  L.unlines
    [ codeblock' Nothing $ L.pack linefmt,
      "Page " <> show idx <> " of " <> show totalPages
    ]
  where
    formattedLines = map (fmt . fst) xs
    totalPages = (total + width - 1) `div` total
    linefmt =
      tableString
        [column def right def def, column (expandUntil 80) left def def]
        unicodeRoundS
        def
        (map (rowG . biList) $ zip (map show [(width * idx) ..]) (map L.unpack formattedLines))
