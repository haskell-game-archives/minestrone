-- tile based sprite support
module SDL.Tilesheet where

import SDL
import SDL.Vect
import Foreign.C.Types

-- takes a surface and an index and returns the
-- rectangle for the sprite at the index
nthTile :: Surface -> CInt -> IO (Rectangle CInt)
nthTile surface index = do
  (V2 width height) <- surfaceDimensions surface
  let y = index `div` (height `div` 16) in
    let x = index `mod` (width `div` 16) in
      return $ Rectangle (P $ V2 (x * 16) (y * 16)) (V2 16 16)
