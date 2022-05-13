{-# LANGUAGE ScopedTypeVariables #-}
module Either where
import Control.Monad.Fail
import Data.Either
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Data.Maybe ( fromMaybe )

failIfOdd :: Int -> Either Int ()
failIfOdd n = if n `mod` 2 == 0 then Right () else Left n

failIfAnyOdd :: [Int] -> Either Int ()
failIfAnyOdd = mapM_ failIfOdd

maybeInt :: Maybe Int -> Int
maybeInt = fromMaybe 0

main :: IO()
main = do
        --line <- getArgs
        let ls = [3, 4, 6]
        let res0 = failIfAnyOdd ls
        -- let ml::[Maybe Int] = map readMaybe line
        -- let res = failIfAnyOdd $ map (maybeInt.readMaybe) line
        putStrLn $ show res0
