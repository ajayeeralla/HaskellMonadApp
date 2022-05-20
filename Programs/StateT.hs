module StateT where
import Control.Monad.State.Lazy
import State
  ( Color(..)
  , ColorCounts(..)
  , defaultColorCounts
  )
import Data.Aeson ( decode )
import Data.Maybe ( fromMaybe )
import Data.ByteString.Lazy ()
import qualified Data.ByteString.Lazy as BL
import System.Environment ( getArgs )

countColor :: Color -> StateT ColorCounts IO ()
countColor c =
    case c of
        Red -> modify (\s ->  s { redCount = redCount s + 1 })
        Green -> modify (\s -> s { greenCount = greenCount s + 1 })
        Blue -> modify (\s -> s { blueCount = blueCount s + 1 })

countColors :: [Color] -> StateT ColorCounts IO ()
countColors = mapM_ countColor

countColorsInFile :: FilePath -> StateT ColorCounts IO ()
countColorsInFile file = do
    x <- liftIO $ BL.readFile file
    countColors $ fromMaybe  [] (decode x :: Maybe [Color])

countColorsInFiles :: [FilePath] -> StateT ColorCounts IO ()
countColorsInFiles = mapM_ countColorsInFile

main :: IO ()
main = do
    files <- getArgs
    res <- runStateT (countColorsInFiles files) defaultColorCounts
    -- x = pure defaultColorCounts : [ do { ci <- z ; res <- runStateT (countColorsInFile f) ci ; return $ snd res}| f <- filepath, z <- x ]
    -- forM_ filepath $ \f -> do
    --     res <- runStateT (countColorsInFile f) defaultColorCounts
    --     print $ snd res
    --res <- last (take 3 x)
    print $ snd res
