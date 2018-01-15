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
import System.FilePath ((</>), takeFileName, dropFileName)
import System.Directory (setCurrentDirectory)
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

intToCInt :: Int -> CInt
intToCInt = fromInteger . toInteger

copyTile :: Int -> (Int, Int) -> PetM ()
copyTile n (x, y) = do
  renderer <- asks crenderer
  tileset <- asks ctileset
  dims@(Rectangle _ size) <- nthTile tileset n
  copy
    renderer
    tileset
    (Just dims)
    (Just (Rectangle (P $ V2 (intToCInt x) (intToCInt y))
      size))

main :: IO ()
main = do
  execPath <- getExecutablePath
  unless (takeFileName execPath == "ghci") (setCurrentDirectory (dropFileName execPath))
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
        draw = blitMap >> blitStatus >> blitPet

blitMap :: PetM ()
blitMap = do
  m <- asks chouse
  forM_ (tiles m) $ uncurry copyTile

blitPet :: PetM ()
blitPet = do
  pet <- asks cpet
  copyTile 32 $ location pet

blitStatus :: PetM ()
blitStatus = do
  copyIconStatus 1 11 hygiene
  copyIconStatus 4 12 hunger
  copyIconStatus 7 13 entertainment
  where copyIcon x n = copyTile n (16 * x, 0)
        copyStatus x f = do
          pet <- asks cpet
          copyTile (statusTile $ f pet) (16 * x, 0)
        copyIconStatus x n f = do
          copyIcon   (x+0) n
          copyStatus (x+1) f
