module Data.Types
  ( TemperatureScale(..)
  , WeatherStatus(..)
  )
where

data TemperatureScale =
    Celcius
    | Fahrenheit
    deriving (Eq, Ord, Show)

data WeatherStatus =
    Clear
    | Clouds
    | Rain
    | Snow
    deriving (Eq, Ord, Show)

