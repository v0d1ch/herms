module Herms.Persist where

import Herms.Persist.Sqlite
import qualified Herms.Types as Types

import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Foldable (traverse_)
import Data.Pool
import Data.Ratio ((%))
import Data.Text (Text)
-- import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite

-- You can get help at #haskell-beginners on Freenode IRC if you want to learn more Haskell
-- The book is at http://haskellbook.com

-- database: "_env:SQLITE_DATABASE:yesod-sqlite.sqlite3"
-- poolsize: "_env:SQLITE_POOLSIZE:10"

    -- -- Create the database connection pool
    -- pool <- flip runLoggingT logFunc $ createSqlitePool
    --     (sqlDatabase $ appDatabaseConf appSettings)
    --     (sqlPoolSize $ appDatabaseConf appSettings)

    -- -- Perform database migration using our application's logging settings.
    -- runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

-- | dbName: yesod-sqlite.sqlite3
--   poolSize: 10
makeSqlitePool :: Text -> Int -> IO (Pool SqlBackend)
makeSqlitePool dbName poolSize =
  runNoLoggingT $ createSqlitePool dbName poolSize

defaultSqlitePool :: IO (Pool SqlBackend)
defaultSqlitePool =
  makeSqlitePool "recipes.sqlite3" 10

migrateSqliteDatabase :: Pool SqlBackend -> IO ()
migrateSqliteDatabase pool =
  runNoLoggingT (runSqlPool (runMigration migrateAll) pool)

runDB :: Pool SqlBackend -> SqlPersistM a -> IO a
runDB pool action = do
  runResourceT $ runNoLoggingT $ runSqlPool action pool

toDatabaseIngredient :: Types.Ingredient -> Key Recipe -> Ingredient
toDatabaseIngredient ingredient recipeId =
  Ingredient
  (toRational $ Types.quantity ingredient)
  (Types.unit ingredient)
  (Types.ingredientName ingredient)
  (Types.attribute ingredient)
  recipeId

toDatabaseDirection :: String -> Key Recipe -> Direction
toDatabaseDirection = Direction

toDatabaseTag :: String -> Key Recipe -> Tag
toDatabaseTag = Tag

toDatabaseRecipe :: Types.Recipe -> ( Recipe
                                    , [Key Recipe -> Ingredient]
                                    , [Key Recipe -> Direction]
                                    , [Key Recipe -> Tag]
                                    )
toDatabaseRecipe recipe =
  let dbRecipe = Recipe (Types.recipeName recipe) (Types.description recipe)
  in ( dbRecipe
     , fmap toDatabaseIngredient (Types.ingredients recipe)
     , fmap toDatabaseDirection (Types.directions recipe)
     , fmap toDatabaseTag (Types.tags recipe)
     )

testRecipe :: Types.Recipe
testRecipe = Types.Recipe
                    { recipeName = "Undergrad Pad Thai"
                    , description = "Actually tasty and super cheap ramen that tastes a bit like Pad Thai but not really."
                    , ingredients = [ Types.Ingredient {quantity = 1 % 1, unit = "package", ingredientName = "oriental-flavor instant ramen", attribute = ""}
                                    , Types.Ingredient {quantity = 1 % 1, unit = "cups", ingredientName = "frozen stir-fry vegetables", attribute = ""}
                                    , Types.Ingredient {quantity = 1 % 1, unit = "Tbsp", ingredientName = "peanut butter", attribute = "chunky or creamy"}
                                    , Types.Ingredient {quantity = 0 % 1, unit = "", ingredientName = "Your favorite hot sauce", attribute = "(optional)"}]
                    , directions = ["Place the dry noodles in a bowl. Add frozen veggies.","Add enough water to cover the noodles without overflowing your bowl. It's okay if the veggies aren't covered.","Microwave for approximately 5 minutes. You ideally want the water to boil a bit.","Drain about 3/4 of the water from the bowl.","Add peanut butter, about 3/5 of the flavor packet, and hot sauce.","Stir until the peanut butter has become one with the ramen. Enjoy!"]
                    , tags = ["vegan"]}

insertRecipe :: Types.Recipe -> SqlPersistM (Key Recipe)
insertRecipe recipe = do
  let (dbRecipe, ingredients, directions, tags) = toDatabaseRecipe recipe
  recipeId <- insert dbRecipe
  traverse_ insert $ sequenceA ingredients recipeId
  traverse_ insert $ sequenceA directions recipeId
  traverse_ insert $ sequenceA tags recipeId
  return recipeId

getRecipeBookDB :: SqlPersistM [(Key Recipe, Types.Recipe)]
getRecipeBookDB = undefined
