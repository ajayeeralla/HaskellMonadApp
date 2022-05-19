{-# LANGUAGE DeriveGeneric #-}
module State
  ( Color
  , ColorCounts
  , main
  , defaultColorCounts
  )
where
import Control.Monad.State.Lazy
  ( State
  , execState
  )
import Control.Monad.State.Class
  ( MonadState(get, put)
  )
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON )



data Color =
    Red
    | Green
    | Blue
    deriving(Show)

data ColorCounts =
    ColorCounts
    { redCount :: Int
    , greenCount :: Int
    , blueCount :: Int
    }
    deriving (Generic, Show)

instance FromJSON ColorCounts
instance ToJSON ColorCounts

incRed :: ColorCounts -> ColorCounts
incRed cc =
    let x = redCount cc in
    cc {redCount=x+1}

incGreen :: ColorCounts -> ColorCounts
incGreen cc =
    let x = greenCount cc in
    cc {greenCount=x+1}

incBlue :: ColorCounts -> ColorCounts
incBlue cc =
    let x = blueCount cc in
    cc {blueCount=x+1}

count :: [Color] -> State ColorCounts ()
count [] = do
    _ <- get
    return ()
count (c:cs) = do
    cc <- get
    case c of
        Red -> put $ incRed cc
        Green -> put $ incGreen cc
        Blue -> put $ incBlue cc
    count cs

defaultColorCounts :: ColorCounts
defaultColorCounts =
    ColorCounts
    { redCount = 0
    , greenCount = 0
    , blueCount = 0
    }

listColors :: [Color]
listColors = [Red, Green, Blue, Red, Blue]

main :: IO()
main = do
    let state = count listColors
    let s = execState state defaultColorCounts
    print $ "redcount = " ++ show (redCount s) ++ ", greenCount = " ++ show (greenCount s) ++ ", blueCount = " ++ show (blueCount s)
