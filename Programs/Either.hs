{-# LANGUAGE ScopedTypeVariables #-}
module Either where
import Data.Either ()
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Control.Monad (when)

failIfOdd :: Int -> Either Int ()
failIfOdd n =
        when (odd n) $ Left n

failIfAnyOdd :: [Int] -> Either Int ()
failIfAnyOdd = mapM_ failIfOdd

main :: IO()
main = do
    x <- getArgs
    let ml::[Maybe Int] = map readMaybe x
        ls = map (fromMaybe 0) ml
        res = failIfAnyOdd ls
    print res
