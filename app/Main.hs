{-# LANGUAGE OverloadedStrings #-}
module Main where

import Pet
import Prelude hiding (floor)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Foreign.C.Types (CInt)
import SDL
import SDL.Vect
import SDL.TileMap
import SDL.Tilesheet -- see src/SDL/Tilesheet.hs
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map

data Context = Context {
  cwindow :: Window
, crenderer :: Renderer
, ctileset :: Texture
, chouse :: TileMap
, cpet :: Pet
}

type PetT m a = ReaderT Context m a
type PetM a = PetT IO a

runPet :: PetM a -> Context -> IO a
runPet = runReaderT

copyTile :: CInt -> Point V2 CInt -> PetM ()
copyTile n loc = do
  renderer <- asks crenderer
  tileset <- asks ctileset
  dims@(Rectangle _ size) <- nthTile tileset n
  copy renderer tileset (Just dims) (Just (Rectangle loc size))

main :: IO ()
main = do
  initialize [InitVideo]
  window <- createWindow "Pet" defaultWindow {
      windowInitialSize = V2 800 800
    , windowOpenGL = Just defaultOpenGL
    , windowResizable = True
    }
  renderer <- createRenderer window 0 defaultRenderer
  rendererLogicalSize renderer $= Just (V2 (10 * 16) (10 * 16))
  houseMap <- loadTileMap "assets/house.map"
  tilesetT <- loadBMP "assets/tileset_alt.bmp"
  tileset <- createTextureFromSurface renderer tilesetT
  showWindow window
  let context = Context {
    cwindow = window
    , crenderer = renderer
    , ctileset = tileset
    , chouse = houseMap
    , cpet = newPet
  }
  loop context
  destroyWindow window
  destroyTexture tileset
  freeSurface tilesetT
  destroyRenderer renderer
  quit

loop :: Context -> IO ()
loop context = do
  let renderer = crenderer context
  events <- pollEvents
  unless (quit events) $ do
    clear renderer
    runPet draw context
    present renderer
    loop context
  where quit events = elem QuitEvent $ map eventPayload events
        draw = blitMap >> blitPet

blitMap :: PetM ()
blitMap = do
  m <- asks chouse
  forM_ (tiles m) $ \(id, rect) ->
    copyTile id (fromIntegral <$> rect)

blitPet :: PetM ()
blitPet = do
  pet <- asks cpet
  copyTile 32 $ location pet
