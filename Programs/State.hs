module State where
import Control.Monad.State.Lazy (State, execState)
import Control.Monad.State.Class ( MonadState(get, put))
import Text.ParserCombinators.ReadP (count)
--import GHC.Show (Show)
--import GHC.Stack.CCS (ccLabel)

data Color =
        Red
    |   Green
    |   Blue
    deriving(Show)

data ColorCounts =
        ColorCounts
            { redCount :: Int
            , greenCount :: Int
            , blueCount :: Int
            }

setColorCountsZero :: ColorCounts
setColorCountsZero =
    ColorCounts
        { redCount = 0
        , greenCount = 0
        , blueCount = 0
        }

listColors :: [Color]
listColors = [Red, Green, Blue, Red, Blue]

countRed :: [Color] -> Int
countRed lc =
    case lc of
        [] -> 0
        h:t -> case h of
                Red -> 1 + countRed t
                _   -> countRed t

countGreen :: [Color] -> Int
countGreen lc =
    case lc of
        [] -> 0
        h:t -> case h of
                Green -> 1 + countGreen t
                _     ->  countGreen t

countBlue :: [Color] -> Int
countBlue lc =
    case lc of
        [] -> 0
        h:t -> case h of
                Blue -> 1 + countBlue t
                _    -> countBlue t


countColors :: State ColorCounts ()
countColors = do
              cc <- get
              let rc = countRed listColors
                  gc = countGreen listColors
                  bc = countBlue listColors
              put cc{redCount=rc, greenCount=gc, blueCount=bc}
--             --   let rc = redCount cc
--             --       gc = greenCount cc
--             --       bc = blueCount cc
--               --return cc

main :: IO()
main = do
       let cc = setColorCountsZero
           s = execState countColors cc
       putStrLn $ "redcount = " ++ show (redCount s) ++ ", greenCount = " ++ show (greenCount s) ++ ", blueCount = " ++ show (blueCount s)
