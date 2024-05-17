module Common.TimeUnits where

import Data.Aeson qualified as Aeson
import Relude

-- |
-- Note: we dont use months ans years,
-- because they make the type more complex and context-dependent
data TimeUnits = TimeUnits
  { days :: Int,
    hours :: Int,
    minutes :: Int,
    seconds :: Int,
    millis :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)


toTextRepr :: TimeUnits -> Text
toTextRepr tu =
  [ (tu.days, "d"),
    (tu.hours, "h"),
    (tu.minutes, "m"),
    (tu.seconds, "s"),
    (tu.millis, "ms")
  ]
    & dropWhile ((== 0) . fst)
    & fmap (\(u, label) -> show u ++ label & toText)
    & (\xs -> if null xs then "less than 1ms" else unwords xs)

-- ---------------------
-- Constructors and Converters
-- ---------------------

fromMillis :: Int -> TimeUnits
fromMillis totalMillis =
  let (days, r1) =
        divMod totalMillis (24 * 3600000)

      (hours, r2) =
        divMod r1 3600000

      (minutes, r3) =
        divMod r2 60000

      (seconds, millis) =
        divMod r3 1000
   in TimeUnits
        { days = days,
          hours = hours,
          minutes = minutes,
          seconds = seconds,
          millis = millis
        }

toMillis :: TimeUnits -> Int
toMillis TimeUnits {days, hours, minutes, seconds, millis} =
  (days * 24 * 3600000) + hours * 3600000 + minutes * 60000 + seconds * 1000 + millis
