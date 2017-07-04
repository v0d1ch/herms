module Herms.Persist.Sqlite where

import Database.Persist
import Database.Persist.Quasi
import Database.Persist.TH

-- data Ingredient = Ingredient { quantity :: Ratio Int
--                              , unit :: String
--                              , ingredientName :: String
--                              , attribute :: String
--                              } deriving (Eq, Show, Read)

-- data Recipe = Recipe { recipeName :: String
--                      , description :: String
--                      , ingredients :: [Ingredient]
--                      , directions :: [String]
--                      , tags :: [String]
--                      } deriving (Eq, Show, Read)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Recipe
    name String
    description String
    deriving Eq Show

Ingredient
    quantity Rational
    unit String
    name String
    attribute String
    recipeId RecipeId
    deriving Eq Show

Direction
    instructions String
    recipeId RecipeId
    deriving Eq Show

Tag
    name String
    recipeId RecipeId
    deriving Eq Show
|]
