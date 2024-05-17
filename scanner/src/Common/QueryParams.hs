module Common.QueryParams where

import Fmt
import Relude

type QueryParams = [(Text, Text)]

toQueryString :: QueryParams -> Text
toQueryString =
  foldl' (\queryString (key, value) -> queryString |+ "&" +| key |+ "=" +| value |+ "") ""
