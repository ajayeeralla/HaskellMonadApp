module Reader
    ( WeatherConditions(..)
    , main
    , wc
    )
where
import Control.Monad.Reader
  ( MonadReader(ask)
  , runReader, Reader
  )
import Data.Types
  ( WeatherStatus(Clouds)
  , TemperatureScale(Fahrenheit)
  )

data WeatherConditions =
    WeatherConditions
    { temperature :: Int
    , temperatureScale :: TemperatureScale
    , weatherStatus :: WeatherStatus
    }

weatherConditionString :: Reader WeatherConditions String
weatherConditionString = do
                        weather <- ask
                        let temp = temperature weather
                            scale = temperatureScale weather
                            status = weatherStatus weather
                        return $ "The current weather conditions are "++ show status++ ", " ++ show temp ++ " degrees " ++ show scale

wc :: WeatherConditions
wc =
    WeatherConditions
    { temperature = 53
    , temperatureScale = Fahrenheit
    , weatherStatus = Clouds
    }

main :: IO ()
main =  do
    let str = runReader weatherConditionString wc
    putStrLn str