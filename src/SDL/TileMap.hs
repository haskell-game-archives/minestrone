-- CSV map support based on SDL
module SDL.TileMap where

import SDL
import Foreign.C.Types
import Data.List (intercalate)

data TileMap = TileMap { width  :: Int
               , height :: Int
               , tiles  :: [(CInt, Point V2 Int)] }
           deriving (Read, Show)

separate :: Char -> String -> [String]
separate e l = case dropWhile (== e) l of
                 [] -> []
                 l' -> w : separate e l''
                   where (w, l'') = break (== e) l'

loadTileMap :: FilePath -> IO TileMap
loadTileMap fp = do
  mapData <- readFile fp
  return $ buildTileMap mapData

buildTileMap :: String -> TileMap
buildTileMap txt = TileMap { width  = w
                   , height = h
                   , tiles = zip tiles grid }
  where rows   = lines txt
        matrix = map (separate ',') rows
        w = length rows
        h = length $ head matrix
        readTile t = read t :: CInt
        tiles  = map readTile $ separate ',' $ intercalate "," rows

grid :: Integral a => [Point V2 a]
grid = [P $ V2 (x*16) (y*16) | y <- [0..9], x <- [0..9]]
