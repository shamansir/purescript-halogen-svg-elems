module Halogen.Svg.Attributes.Duration
  ( Duration
  , printDuration
  , DurationF(..)
  , printDurationF
  ) where
-- TODO: DurationF and printDurationF probably shouldn't be exported but
-- removing them from the export list would technically be a breaking change

import Prelude
import Data.Maybe (Maybe, maybe)
import Halogen.Svg.Attributes.Utils (showWithSpaces)

-- | https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dur
data DurationF a = Duration (Maybe a) (Maybe a) (Maybe a) (Maybe a)
-- ^ TODO hours minutes seconds millis

derive instance eqDurationF :: Eq a => Eq (DurationF a)

derive instance functorDurationF :: Functor DurationF

instance showDurationF :: Show Duration where
  show (Duration h m s i) = "(Duration " <> showWithSpaces [h, m, s, i] <> ")"

printDurationF :: forall a. Show a => DurationF a -> String
printDurationF (Duration h m s i) = f "h" h <> f "m" m <> f "s" s <> f "i" i
  where f u = maybe "" (\val -> show val <> u)

type Duration = DurationF Number

-- TODO derive Show instance for DurationF

printDuration :: Duration -> String
printDuration = printDurationF
