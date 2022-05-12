module MaybeT where
import Control.Monad.Reader
import Maybe ( WeatherConditions (..)
             , wc
             )

weatherConditionString :: ReaderT WeatherConditions Maybe String
weatherConditionString = do
                         weather <- ask
                         let Just temp = temperature weather
                             Just scale = temperatureScale weather
                             Just status = weatherStatus weather
                         return $ "The current weather conditions are "++ status++ ", " ++ show temp ++ " degrees " ++ show scale

main :: IO()
main = do
       let Just mstr = runReaderT weatherConditionString wc
       putStrLn mstr
