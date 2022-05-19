module StateT where
import Control.Monad.State.Lazy
import State
  ( ColorCounts
  , defaultColorCounts
  )
import Data.Aeson
-- import System.FilePath
import Data.Aeson.Types (FromJSON)
import Data.ByteString.Lazy ()
import Data.String (IsString(fromString))
import qualified Data.ByteString.Lazy as BL


countColorsInFile :: FilePath -> StateT ColorCounts IO ()
countColorsInFile file = do
    let res :: IO ()
        res = do
            bs <- BL.readFile file
            case (decode bs):: Maybe String of
                Nothing -> return ()
                Just x -> print x
    liftIO  res



main :: IO ()
main = do
    let filepath = "json"
        tmp = do
            x <- countColorsInFile filepath
            return x
    evalStateT tmp defaultColorCounts


-- countColorsInFile :: FilePath -> StateT ColorCounts IO ()
-- countColosInFiles :: [FilePath ] -> StateT ColorCounts IO ()