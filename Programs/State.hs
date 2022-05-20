{-# LANGUAGE DeriveGeneric #-}
module State
  ( Color(..)
  , ColorCounts(..)
  , main
  , defaultColorCounts
  , countColors
  , listColors
  )
where
import Control.Monad.State.Lazy
  ( State
  , execState
  , modify
  )
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON )

data Color =
    Red
    | Green
    | Blue
    deriving(Show, Generic)

data ColorCounts =
    ColorCounts
    { redCount :: Int
    , greenCount :: Int
    , blueCount :: Int
    }
    deriving (Show)

instance FromJSON Color
instance ToJSON Color

countColor :: Color -> State ColorCounts ()
countColor c =
    case c of
        Red -> modify (\s ->  s { redCount = redCount s + 1 })
        Green -> modify (\s -> s { greenCount = greenCount s + 1 })
        Blue -> modify (\s -> s { blueCount = blueCount s + 1 })

countColors :: [Color] -> State ColorCounts ()
countColors = mapM_ countColor

defaultColorCounts :: ColorCounts
defaultColorCounts =
    ColorCounts
    { redCount = 0
    , greenCount = 0
    , blueCount = 0
    }

listColors :: [Color]
listColors = [Red, Green, Blue, Red, Blue]

main :: IO ()
main = do
    let state = countColors listColors
        s = execState state defaultColorCounts
    print $ "redcount = " ++ show (redCount s) ++ ", greenCount = " ++ show (greenCount s) ++ ", blueCount = " ++ show (blueCount s)
