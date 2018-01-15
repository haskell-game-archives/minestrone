module Pet where

import SDL

data Status = Happy | Satiated | Lacking | Alert deriving (Read, Show, Eq, Ord)

data Pet = Pet {
  age :: Int
, location :: (Int, Int)
, hygiene :: Status
, hunger :: Status
, entertainment :: Status
} deriving (Read, Show)

newPet :: Pet
newPet = Pet { age = 0
             , location = (80, 120)
             , hygiene = Happy
             , hunger = Happy
             , entertainment = Happy
             }

statusTile :: Status -> Int
statusTile Happy = 2
statusTile Satiated = 3
statusTile Lacking = 4
statusTile Alert = 5
