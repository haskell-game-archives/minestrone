-- tile based sprite support
module SDL.Tilesheet where

import SDL
import Control.Monad.IO.Class
import SDL.Vect
import Foreign.C.Types

-- takes a surface and an index and returns the
-- rectangle for the sprite at the index
nthTileIO :: Texture -> CInt -> IO (Rectangle CInt)
nthTileIO texture index = do
  (TextureInfo _ _ width height) <- queryTexture texture
  let y = index `div` (height `div` 16) in
    let x = index `mod` (width `div` 16) in
      return $ Rectangle (P $ V2 (x * 16) (y * 16)) (V2 16 16)

nthTile :: MonadIO m => Texture -> CInt -> m (Rectangle CInt)
nthTile t n = liftIO $ nthTileIO t n
