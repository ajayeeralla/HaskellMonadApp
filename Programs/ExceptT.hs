module ExceptT where
import Control.Exception
  ( IOException
  )
import System.Environment ( getArgs )
import Control.Monad.State.Lazy
  ( StateT
  , runStateT
  , liftIO
  )
import State
  ( defaultColorCounts
  , ColorCounts(..)
  )
import Control.Error
  ( ExceptT
  , handleExceptT
  , runExceptT
  )

countColorsInFile :: FilePath -> ExceptT String (StateT ColorCounts IO) ()
countColorsInFile fpath = do
    str <- liftIO $ readFile fpath
    handleExceptT handler $ liftIO $ print str
    where
        handler :: IOException -> String
        handler ioe = show ioe

countColorsInFiles :: [FilePath] -> ExceptT String (StateT ColorCounts IO) ()
countColorsInFiles = mapM_ countColorsInFile

main :: IO ()
main = do
    files <- getArgs
    let st = runExceptT (countColorsInFiles files)
    res <- runStateT st defaultColorCounts
    -- x = pure defaultColorCounts : [ do { ci <- z ; res <- runStateT (countColorsInFile f) ci ; return $ snd res}| f <- filepath, z <- x ]
    -- forM_ filepath $ \f -> do
    --     res <- runStateT (countColorsInFile f) defaultColorCounts
    --     print $ snd res
    --res <- last (take 3 x)
    print $ snd res
