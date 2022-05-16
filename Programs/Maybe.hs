module Maybe
  ( WeatherConditions(..)
  , wc
  , main
  )
where
import Data.Types
    ( WeatherStatus(Clouds)
    , TemperatureScale(Fahrenheit)
    )
data WeatherConditions =
    WeatherConditions
    { temperature :: Maybe Int
    , temperatureScale :: Maybe TemperatureScale
    , weatherStatus :: Maybe String
    }

weather :: WeatherConditions -> Maybe String
weather w = do
    temp <- temperature w
    scale <- temperatureScale w
    status <- weatherStatus w
    return $ "The current weather conditions are "++ status++ ", " ++ show temp ++ " degrees " ++ show scale


wc :: WeatherConditions
wc =
    WeatherConditions
    { temperature = Just 53
    , temperatureScale = Just Fahrenheit
    , weatherStatus = Just $ show Clouds
    }

main :: IO()
main =  do
    let Just v = weather wc
    putStrLn v