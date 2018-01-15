module Pet where

import Foreign.C.Types (CInt)
import SDL

data Status = Happy | Satiated | Lacking | Alert deriving (Read, Show, Eq, Ord)

data Pet = Pet {
  age :: Int
, location :: Point V2 CInt
, hygiene :: Status
, hunger :: Status
, entertainment :: Status
} deriving (Read, Show)

newPet :: Pet
newPet = Pet { age = 0
             , location = P (V2 0 0)
             , hygiene = Happy
             , hunger = Happy
             , entertainment = Happy
             }
