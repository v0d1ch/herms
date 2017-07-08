module Herms.Persist where

import Herms.Persist.Sqlite
import qualified Herms.Types as Types

import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Foldable (for_, traverse_)
import Data.List (intercalate)
import Data.Pool
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable (for)
-- import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite
import Safe (readMay)
import Text.Blaze.Renderer.Pretty (renderMarkup)
import Text.Hamlet

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

defaultSqliteName = "recipes.sqlite3"

defaultSqlitePool :: IO (Pool SqlBackend)
defaultSqlitePool =
  makeSqlitePool defaultSqliteName 10

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

fromDatabaseTag :: Tag -> String
fromDatabaseTag Tag{..} = tagName

fromDatabaseDirection :: Direction -> String
fromDatabaseDirection Direction{..} = directionInstructions

fromDatabaseIngredient :: Ingredient -> Types.Ingredient
fromDatabaseIngredient Ingredient{..} =
  Types.Ingredient { quantity = fromRational ingredientQuantity
                   , unit = ingredientUnit
                   , ingredientName = ingredientName
                   , attribute = ingredientAttribute }

fromDatabaseRecipe :: [Direction] -> [Tag] -> [Ingredient] -> Recipe -> Types.Recipe
fromDatabaseRecipe directions tags ingredients Recipe{..} =
  Types.Recipe { recipeName = recipeName
               , description = recipeDescription
               , ingredients = fmap fromDatabaseIngredient ingredients
               , directions = fmap fromDatabaseDirection directions
               , tags = fmap fromDatabaseTag tags
               }

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

migrate :: [String] -> IO ()
migrate [filename] = do
  recipeBook <- Types.getRecipeBook filename
  pool <- defaultSqlitePool
  runDB pool $ do
    runMigration migrateAll
    traverse_ insertRecipe recipeBook
  destroyAllResources pool
  putStrLn "Successfully migrated recipe book!"
migrate _ =
  error "We want a single argument which is the filename of your recipes file."

fromRecipeDB :: Entity Recipe -> SqlPersistM (Key Recipe, Types.Recipe)
fromRecipeDB (Entity recipeKey recipe) = do
  eingredients <- selectList [IngredientRecipeId ==. recipeKey] []
  etags <- selectList [TagRecipeId ==. recipeKey] []
  edirections <- selectList [DirectionRecipeId ==. recipeKey] []
  let ingredients = fmap entityVal eingredients
      tags = fmap entityVal etags
      directions = fmap entityVal edirections
  return (recipeKey, fromDatabaseRecipe directions tags ingredients recipe)

getRecipeBookDB :: SqlPersistM [(Key Recipe, Types.Recipe)]
getRecipeBookDB = do
  recipes <- selectList [] []
  traverse fromRecipeDB recipes

recipeTupName :: (Key Recipe, Types.Recipe) -> (String, (Key Recipe, Types.Recipe))
recipeTupName (k, r@Types.Recipe{..}) = (recipeName, (k, r))

lookupRecipe :: [(Key Recipe, Types.Recipe)] -> String -> Maybe (Key Recipe, Types.Recipe)
lookupRecipe recipeBook key =
  case readMay key of
    (Just n) -> ((toSqlKey n),) <$> lookup (toSqlKey n) recipeBook
    Nothing -> lookup key $ fmap recipeTupName recipeBook

renderIngredient :: Types.Ingredient -> String
renderIngredient = undefined

-- We can do a much better job with this.
prettyPrint :: Types.Recipe -> IO ()
-- prettyPrint = undefined
prettyPrint (Types.Recipe recipeName description ingredients directions tags) =
  let tagLine = intercalate " " tags
  in Text.putStrLn $ Text.pack $ concat $
    [ "- " <> recipeName <> "\n\n"
    , "Tags: " <> intercalate " " tags <> "\n\n"
    , "Ingredients:\n"
    , ingredients >>= renderIngredient
    ]


-- Ingredients:
--     $forall Types.Ingredient quantity unit ingredientName attribute <- ingredients
--         #{ingredientName}

    -- Directions:
    -- $forall (n, direction) <- directions
    --     #{n}. #{direction}

-- Ingredients:
--       10 ounces fettuccini pasta
--       ½ cup butterContent for class
--       5 cloves garlic, choppedContent for class
--       1 cup heavy creamContent for class
--       1 egg yolkContent for class
--       2 cups freshly grated Parmesan cheese
--       2 tablespoons dried parsley
-- Directions:
-- 1.	Bring a large pot of lightly salted water to a boil. Add fettuccine pasta and cook for 8 to 10 minutes or until al dente; drain.
-- 2.	In a large skillet melt the butter and add the chopped garlic. Cook on low for about 5 minutes, stirring often, making sure not to burn the garlic.
-- 3.	Pour about a ¼ cup of the heavy cream into a small bowl. Add the egg yolk and beat together; put aside. Pour the remaining cream into the frying pan. Increase the heat to medium-high. As the cream starts to boil, mix rapidly using a whisk. Slowly add the cream/egg mixture. You do not want the egg to curdle. Continue whisking until well blended.
-- 4.	Add 1 cup of the Parmesan cheese and continue to mix the cream. Pour in the remaining Parmesan and the parsley, mix until smooth. Immediately remove from stove. Serve over cooked pasta.

view :: [String] -> IO ()
view targets = do
  pool <- defaultSqlitePool
  runDB pool $ do
    runMigration migrateAll
    recipeBook <- getRecipeBookDB
    for_ targets $ \target -> do
      liftIO $ putStrLn $ case lookupRecipe recipeBook target of
        Nothing -> target ++ " does not exist"
        (Just recip) -> show recip
  destroyAllResources pool

-- view :: [String] -> IO ()
-- view targets = do
--   recipeBook <- getRecipeBook defaultRecipeFile
--   forM_ targets $ \ target -> do
--     putStr $ case readRecipeRef target recipeBook of
--       Nothing   -> target ++ " does not exist\n"
--       Just recp -> showRecipe recp

-- list :: [String] -> IO ()
-- list _  = do
--   recipes <- getRecipeBook defaultRecipeFile
--   let recipeList = map recipeName recipes
--       size       = length $ show $ length recipeList
--       indices    = map (padLeft size . show) [1..]
--   putStr $ unlines $ zipWith (\ i -> ((i ++ ". ") ++)) indices recipeList

-- remove :: [String] -> IO ()
-- remove targets = forM_ targets $ \ target -> do
--   recipeBook <- getRecipeBook defaultRecipeFile
--   (tempName, tempHandle) <- openTempFile "." "herms_temp"
--   case readRecipeRef target recipeBook of
--     Nothing   -> putStrLn $ target ++ " does not exist\n"
--     Just recp -> do
--       let newRecpBook = delete recp recipeBook
--       putStrLn $ "Removing recipe: " ++ recipeName recp ++ "..."
--       hPutStr tempHandle $ unlines $ show <$> newRecpBook
--       putStrLn "Recipe deleted."
--   hClose tempHandle
--   removeFile defaultRecipeFile
--   renameFile tempName defaultRecipeFile
