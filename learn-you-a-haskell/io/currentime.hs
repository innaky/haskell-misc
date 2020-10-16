import Data.Time

now :: IO LocalTime
now = zonedTimeToLocalTime <$> getZonedTime
