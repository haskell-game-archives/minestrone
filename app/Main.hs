{-# LANGUAGE OverloadedStrings #-}
module Main where

import Pet
import Prelude hiding (floor)
import Control.Monad
import Control.Monad.IO.Class
import Foreign.C.Types (CInt)
import SDL
import SDL.Vect
import SDL.Map -- see src/SDL/Map.hs
import SDL.Tilesheet -- see src/SDL/Tilesheet.hs
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import Control.Concurrent (threadDelay)

screenSize :: Integral a => a
screenSize = 800

scale = (* 5)

main :: IO ()
main = do
  initialize [InitVideo]
  window <- createWindow "Pet" defaultWindow { windowInitialSize = V2 screenSize screenSize }
  houseMap <- loadMap "assets/house.map"
  tileset <- loadBMP "assets/tileset.bmp"
  showWindow window
  loop newPet houseMap tileset window
  destroyWindow window
  freeSurface tileset
  quit


loop :: Pet -> Map -> Surface -> Window -> IO ()
loop pet houseMap tileset window = do
  events <- pollEvents
  surface <- getWindowSurface window
  blitMap tileset surface houseMap
--  blitPet tileset surface pet
  updateWindowSurface window
  unless (quit events) $ do
    loop pet houseMap tileset window
  where quit events = elem QuitEvent $ map eventPayload events

blitMap :: Surface -> Surface -> Map -> IO ()
blitMap tileset target m = forM_ (tiles m) $ \t@(id, rect) -> do
  tile <- nthTile tileset id
  print $ tiles m
  surfaceBlitScaled tileset (Just tile) target (Just (Rectangle (fromIntegral <$> rect) (V2 80 80)))

blitPet :: Surface -> Surface -> Pet -> IO ()
blitPet tileset target pet = do
  dims@(Rectangle _ size) <- nthTile tileset 4
  surfaceBlitScaled tileset (Just dims) target (Just (Rectangle (location pet) (scale size)))


