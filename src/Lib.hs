{-# LANGUAGE LambdaCase #-}

module Lib (runLines, runAnimatedLines) where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Point.Arithmetic as PA
import           Graphics.Gloss.Geometry.Line
import           Data.Maybe
import           System.Random (Random(randomRIO), randomRs, getStdGen)
import           Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import           Data.Functor ((<&>))

runLines :: IO ()
runLines = do
  stdGen <- getStdGen
  let rnd = take 400 $ getAction <$> randomRs (0, 2) stdGen
      gen = foldr (flip action) [] rnd
      pic = scale 2 2 $ color white $ line gen
  appendFile "generated/codes" (concat (show <$> rnd) <> "\n")
  display (InWindow "Never Intersect" (1024, 860) (0, 0)) black pic

runAnimatedLines :: IO ()
runAnimatedLines = do
  simulateIO
    (InWindow "Never Intersect" (1024, 860) (0, 0))
    black
    4
    []
    (return . scale 3 3 . color white . line)
    (\_ _ ps -> randomRIO (0, 2) <&> action ps . getAction)

data Action = ALeft
            | ARight
            | AForward

instance Show Action where
  show ALeft = "L"
  show ARight = "R"
  show AForward = "F"

parseAction :: String -> [Action]
parseAction (x:xs) = case x of
  'L' -> ALeft:parseAction xs
  'R' -> ARight:parseAction xs
  'F' -> AForward:parseAction xs
  _   -> error "Error parsing action string"
parseAction [] = []

data GlobalDirection = GUp
                     | GDown
                     | GRight
                     | GLeft

getAction :: Int -> Action
getAction 0 = AForward
getAction 1 = ALeft
getAction 2 = ARight
getAction _ = error "Number not between 0 and 2"

dirToVector :: GlobalDirection -> Point
dirToVector = \case
  GUp    -> (0, 1)
  GDown  -> (0, -1)
  GRight -> (1, 0)
  GLeft  -> (-1, 0)

toGlobalDir :: Point -> Point -> GlobalDirection
toGlobalDir p p' = case p PA.- p' of
  (0, 0) -> error "toGlobalDir: points are the same!"
  (0, y) -> if y > 0
            then GUp
            else GDown
  (x, 0) -> if x > 0
            then GRight
            else GLeft

resolveRDir :: Action -> GlobalDirection -> GlobalDirection
resolveRDir ALeft = \case
  GUp    -> GLeft
  GDown  -> GRight
  GRight -> GUp
  GLeft  -> GDown
resolveRDir ARight = \case
  GUp    -> GRight
  GDown  -> GLeft
  GRight -> GDown
  GLeft  -> GUp
resolveRDir AForward = id

isCrossing :: Path -> (Point, Point) -> Bool
isCrossing (p1:p2:ps) seg@(p, p') = isJust (intersectSegLine p p' p1 p2)
  || isCrossing (p2:ps) seg
isCrossing _ _ = False

action :: Path -> Action -> Path
action (p:p':ps) a = let nx = next p p' a
                         newPath = (nx:p:p':ps)
                     in if isCrossing ps (nx, p)
                        then action newPath AForward
                        else newPath
action [p] ALeft = p PA.+ dirToVector GLeft:[p]
action [p] ARight = p PA.+ dirToVector GRight:[p]
action [p] AForward = p PA.+ dirToVector GUp:[p]
action [] a = action [(0, 0)] a

next :: Point -> Point -> Action -> Point
next p p' dir = p PA.+ dirToVector (resolveRDir dir $ toGlobalDir p p')