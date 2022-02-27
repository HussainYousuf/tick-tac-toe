module Main where

import           Control.Applicative (Alternative((<|>)))
import           Control.Monad (guard)
import qualified Data.Set as Set
import           Data.List (unfoldr)
import           Data.Maybe (fromMaybe, isJust)
import           Graphics.Gloss (black, green, red, yellow, color, line, polygon
                               , thickCircle, rectangleWire, translate, play
                               , Display(InWindow), Path, Picture(Pictures)
                               , white, blue, rotate, rectangleSolid)
import           Graphics.Gloss.Data.Color (Color)
import           Graphics.Gloss.Interface.Pure.Game (Key(MouseButton)
                                                   , KeyState(Down)
                                                   , MouseButton(LeftButton)
                                                   , Event(EventKey))

data Player = Nought
            | Cross
            deriving Eq

data Board = Board { noughts :: [(Float, Float)]
                   , crosses :: [(Float, Float)]
                   , player :: Player
                   , winner :: Maybe Player
                   }

emptyBoard :: Board
emptyBoard = Board [] [] Cross Nothing

range :: [Float]
range = [-1.0 .. 1.0]

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

winnerSet :: [[(Float, Float)]]
winnerSet = [(i, i) | i <- range]
  :[(-i, i) | i <- range]
  :splitEvery 3 [(i, j) | i <- range, j <- range]
  ++ splitEvery 3 [(i, j) | i <- range, j <- range]

pushToken :: (Float, Float) -> Board -> Board
pushToken c b =
  if c `elem` noughts b || c `elem` crosses b || isJust (winner b)
  then b
  else case player b of
    Nought -> b { noughts = c:noughts b
                , player = Cross
                , winner = isWinner (Set.fromList (c:noughts b)) Nought
                }
    Cross  -> b { crosses = c:crosses b
                , player = Nought
                , winner = isWinner (Set.fromList (c:crosses b)) Cross
                }

isWinner :: Set.Set (Float, Float) -> Player -> Maybe Player
isWinner set player =
  if any ((== 3) . Set.size . Set.intersection set . Set.fromList) winnerSet
  then Just player
  else Nothing

getColor :: Maybe Player -> Color
getColor (Just Nought) = blue
getColor (Just Cross) = red
getColor Nothing = white

drawNought :: (Float, Float) -> Picture
drawNought (x, y) = let x' = spanLength * x
                        y' = spanLength * y
                    in color blue
                       $ translate x' y'
                       $ thickCircle (0.1 * spanLength) (spanLength / 2)

drawCross :: (Float, Float) -> Picture
drawCross (x, y) =
  let x' = spanLength * x
      y' = spanLength * y
  in color red
     $ translate x' y'
     $ Pictures
       [ rotate 45 $ rectangleSolid 10 (spanLength / 1.5)
       , rotate (-45) $ rectangleSolid 10 (spanLength / 1.5)]

drawBoard :: Board -> Picture
drawBoard b = Pictures $ grid:ns ++ cs
  where
    ns = drawNought <$> noughts b

    cs = drawCross <$> crosses b

    grid :: Picture
    grid = color (getColor $ winner b)
      $ Pictures
      $ fmap
        line
        [ [(fst leftSpan, fst centerSpan), (snd rightSpan, fst centerSpan)]
        , [(fst leftSpan, snd centerSpan), (snd rightSpan, snd centerSpan)]
        , [(fst centerSpan, fst leftSpan), (fst centerSpan, snd rightSpan)]
        , [(snd centerSpan, fst leftSpan), (snd centerSpan, snd rightSpan)]]

checkCoordinate :: Float -> Maybe Float
checkCoordinate f = (-1) <$ guard (fst leftSpan < f && f < snd leftSpan)
  <|> 0 <$ guard (fst centerSpan < f && f < snd centerSpan)
  <|> 1 <$ guard (fst rightSpan < f && f < snd rightSpan)

handleKeys :: Event -> Board -> Board
handleKeys (EventKey (MouseButton LeftButton) Down _ (x', y')) b = fromMaybe b
  $ do
    x <- checkCoordinate x'
    y <- checkCoordinate y'
    return $ pushToken (x, y) b
handleKeys _ b = b

-- any divisible of 3
size :: Float
size = 600

spanLength :: Float
spanLength = size / 3

leftSpan :: (Float, Float)
leftSpan = let start = -(size / 2)
               end = start + spanLength
           in (start, end)

rightSpan :: (Float, Float)
rightSpan = let end = size / 2
                start = end - spanLength
            in (start, end)

centerSpan :: (Float, Float)
centerSpan = (snd leftSpan, fst rightSpan)

main :: IO ()
main = let window = InWindow "Tic Tac Toe" (round size, round size) (10, 10)
       in play window black 1 emptyBoard drawBoard handleKeys (const id)