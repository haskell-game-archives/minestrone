-- CSV map support based on SDL
module SDL.TileMap where

import Data.List (intercalate)
import Foreign.C.Types
import SDL

data TileMap = TileMap { width  :: Int
               , height :: Int
               , tiles  :: [(Int, (Int, Int))] }
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
        tiles  = map read $ separate ',' $ intercalate "," rows

grid :: [(Int, Int)]
grid = [(x*16, y*16) | y <- [0..9], x <- [0..9]]
