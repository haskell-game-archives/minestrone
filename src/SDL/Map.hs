-- CSV map support based on SDL
module SDL.Map where

import SDL
import Foreign.C.Types
import Data.List (intercalate)

data Map = Map { width  :: Int
               , height :: Int
               , tiles  :: [(CInt, Point V2 Int)] }
           deriving (Read, Show)

separate :: Char -> String -> [String]
separate e l = case dropWhile (== e) l of
                 [] -> []
                 l' -> w : separate e l''
                   where (w, l'') = break (== e) l'

loadMap :: FilePath -> IO Map
loadMap fp = do
  mapData <- readFile fp
  return $ buildMap mapData

buildMap :: String -> Map
buildMap txt = Map { width  = w
                   , height = h
                   , tiles = zip tiles grid }
  where rows   = lines txt
        matrix = map (separate ',') rows
        w = length $ rows
        h = length $ head $ matrix
        readTile t = read t :: CInt
        tiles  = map readTile $ separate ',' $ intercalate "," rows

-- TODO: work out why V2s are (y, x)
grid :: Integral a => [Point V2 a]
grid = [(P $ V2 x y) | y <- [0..720], x <- [0..720], x `mod` 80 == 0, y `mod` 80 == 0]
