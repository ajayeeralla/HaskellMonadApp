module ReaderT where
import Control.Monad.Reader
import Data.AST
import Reader ( wc
              , WeatherConditions(..)
              )

printConditionString :: ReaderT WeatherConditions IO()
printConditionString =  do
                        weather <- ask
                        let temp = temperature weather
                            scale = temperatureScale weather
                            status = weatherStatus weather
                        liftIO $ putStrLn ("The current weather conditions are "++ show status++ ", " ++ show temp ++ " degrees " ++ show scale)

main :: IO()
main = runReaderT printConditionString wc