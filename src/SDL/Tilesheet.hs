-- tile based sprite support
module SDL.Tilesheet where

import Control.Monad.IO.Class
import Foreign.C.Types
import SDL
import SDL.Vect

-- takes a surface and an index and returns the
-- rectangle for the sprite at the index
nthTileIO :: Texture -> Int -> IO (Rectangle CInt)
nthTileIO texture index = do
  (TextureInfo _ _ width height) <- queryTexture texture
  let y = (fromInteger $ toInteger index) `div` (height `div` 16) in
    let x = (fromInteger $ toInteger index) `mod` (width `div` 16) in
      return $ Rectangle (P $ V2 (x * 16) (y * 16)) (V2 16 16)

nthTile :: MonadIO m => Texture -> Int -> m (Rectangle CInt)
nthTile t n = liftIO $ nthTileIO t n
