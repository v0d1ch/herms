module Herms where

import System.Directory (removeFile, renameFile)
import System.IO (hClose, hPutStr, openTempFile)
import Control.Applicative (Alternative(..))
import Control.Monad (forM_, guard)
import Data.List (delete)
import Text.Read (readMaybe)

import Herms.AddCLI
import Herms.Utils
import Herms.Types
import qualified Herms.Persist as P

-- Constant
defaultRecipeFile = "recipes"

add :: [String] -> IO ()
add _ = do
  input <- getAddInput 
  let newRecipe = readRecipe input
  putStrLn $ showRecipe newRecipe
  putStrLn "Save recipe? (Y)es  (N)o"
  response <- getLine
  if response == "y" || response == "Y" 
    then do 
    appendFile defaultRecipeFile (show newRecipe ++ "\n")
    putStrLn "Recipe saved!"
  else
    putStrLn "Recipe discarded."

-- | `readRecipeRef target book` interprets the string `target`
--   as either an index or a recipe's name and looks up the
--   corresponding recipe in the `book`
readRecipeRef :: String -> [Recipe] -> Maybe Recipe
readRecipeRef target recipeBook =
  (safeLookup recipeBook . pred =<< readMaybe target)
  <|> getRecipe target recipeBook

view :: [String] -> IO ()
view targets = do
  recipeBook <- getRecipeBook defaultRecipeFile
  forM_ targets $ \ target -> do
    putStr $ case readRecipeRef target recipeBook of
      Nothing   -> target ++ " does not exist\n"
      Just recp -> showRecipe recp

list :: [String] -> IO ()
list _  = do
  recipes <- getRecipeBook defaultRecipeFile
  let recipeList = map recipeName recipes
      size       = length $ show $ length recipeList
      indices    = map (padLeft size . show) [1..]
  putStr $ unlines $ zipWith (\ i -> ((i ++ ". ") ++)) indices recipeList

remove :: [String] -> IO ()
remove targets = forM_ targets $ \ target -> do
  recipeBook <- getRecipeBook defaultRecipeFile
  (tempName, tempHandle) <- openTempFile "." "herms_temp"
  case readRecipeRef target recipeBook of
    Nothing   -> putStrLn $ target ++ " does not exist\n"
    Just recp -> do
      let newRecpBook = delete recp recipeBook
      putStrLn $ "Removing recipe: " ++ recipeName recp ++ "..."
      hPutStr tempHandle $ unlines $ show <$> newRecpBook
      putStrLn "Recipe deleted."
  hClose tempHandle
  removeFile defaultRecipeFile
  renameFile tempName defaultRecipeFile

help :: [String] -> IO ()
help _ = putStr $ unlines $ "Usage:" : usage
  where
    usage = map (\ (c, d) -> concat [ padRight size c, " - ", d ]) desc
    size  = maximum $ map (length . fst) desc

    desc :: [(String, String)]
    desc  = [ ("./herms list", "list recipes")
            , ("./herms view (\"Recipe Name\"|Index)", "view a particular recipe")
            , ("./herms add", "add a new recipe (interactive)")
            , ("./herms remove (\"Recipe Name\"|Index)", "remove a particular recipe")
            , ("./herms migrate recipes", "Migrate the recipes flat-file to a sqlite3 database")
            , ("./herms help", "display this help")
            ]

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", P.view)
           , ("remove", remove)
           , ("list", list)
           , ("migrate", P.migrate)
           , ("help", help)
           ]

herms :: [String]      -- command line arguments
      -> Maybe (IO ()) -- failure or resulting IO action
herms args = do
  guard (not $ null args)
  action <- lookup (head args) dispatch
  return $ action (tail args)
