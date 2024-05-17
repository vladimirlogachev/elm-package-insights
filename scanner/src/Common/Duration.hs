module Common.Duration where

import Common.ElmDeriving (ElmType)
import Common.TimeUnits (TimeUnits (..))
import Common.TimeUnits qualified as TimeUnits
import Data.Aeson qualified as Aeson
import Data.Time qualified as Time
import Relude

-- |
-- Duration probably should never be negative, but that's not a final decision.
newtype Duration = DurationMillis Int
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- ---------------------
-- Constructors and Converters
-- ---------------------
-- Note: never used
{-# INLINE toTimeUnits #-}
toTimeUnits :: Duration -> TimeUnits
toTimeUnits (DurationMillis totalMillis) = TimeUnits.fromMillis totalMillis

{-# INLINE fromTimeUnits #-}
fromTimeUnits :: TimeUnits -> Duration
fromTimeUnits = DurationMillis . TimeUnits.toMillis

{-# INLINE toMicroseconds #-}
toMicroseconds :: Duration -> Integer
toMicroseconds (DurationMillis millis) = fromIntegral millis * 1000

{-# INLINE toMillis #-}
toMillis :: Duration -> Int
toMillis (DurationMillis totalMillis) = totalMillis

-- | Expected to be non-negative
{-# INLINE fromMillis #-}
fromMillis :: Int -> Duration
fromMillis x = DurationMillis (abs x)

{-# INLINE toDiffTime #-}
toDiffTime :: Duration -> Time.DiffTime
toDiffTime (DurationMillis totalMillis) = Time.picosecondsToDiffTime (fromIntegral totalMillis * 1_000_000_000)

-- | Expected to be non-negative
{-# INLINE fromSeconds #-}
fromSeconds :: Int -> Duration
fromSeconds x =
  fromTimeUnits
    $ TimeUnits
      { days = 0,
        hours = 0,
        minutes = 0,
        seconds = abs x,
        millis = 0
      }

-- | Expected to be non-negative
{-# INLINE fromMinutes #-}
fromMinutes :: Int -> Duration
fromMinutes x =
  fromTimeUnits
    $ TimeUnits
      { days = 0,
        hours = 0,
        minutes = abs x,
        seconds = 0,
        millis = 0
      }

-- | Expected to be non-negative
{-# INLINE fromHours #-}
fromHours :: Int -> Duration
fromHours x =
  fromTimeUnits
    $ TimeUnits
      { days = 0,
        hours = abs x,
        minutes = 0,
        seconds = 0,
        millis = 0
      }

-- | Expected to be non-negative
{-# INLINE fromDays #-}
fromDays :: Int -> Duration
fromDays x =
  fromTimeUnits
    $ TimeUnits
      { days = abs x,
        hours = 0,
        minutes = 0,
        seconds = 0,
        millis = 0
      }

-- ---------------------
-- Utils
-- ---------------------

{-# INLINE subtr #-}
subtr :: Duration -> Duration -> Duration
subtr (DurationMillis d1) (DurationMillis d2) = DurationMillis (d1 - d2)

{-# INLINE add #-}
add :: Duration -> Duration -> Duration
add (DurationMillis d1) (DurationMillis d2) = DurationMillis (d1 + d2)

{-# INLINE multiplyBy #-}
multiplyBy :: Int -> Duration -> Duration
multiplyBy x (DurationMillis d) = DurationMillis (d * x)
