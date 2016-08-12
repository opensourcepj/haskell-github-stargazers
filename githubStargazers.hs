{-# LANGUAGE DeriveDataTypeable #-}

import Text.JSON
import Text.JSON.Generic
import Data.List
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Environment
import System.Directory
import Control.Monad
import System.IO.Error
import System.IO
import Control.Exception


data StargazerItem  = StargazerItem {
user_name :: String,
starred_at :: String
}
               deriving (Show,Data,Typeable)

instance Ord StargazerItem where
    compare = comparing user_name

instance Eq StargazerItem where
    x == y = user_name x == user_name y


--instance Ord StargazerItem where
--    compare x y = compare (user_name x) (user_name y)


sortByName :: [StargazerItem] -> [StargazerItem]
sortByName = sortBy (comparing user_name)

data Stargazer = Stargazer {repo_id :: Integer
                       , repo_owner :: String
                       ,repo_name::String
                       ,full_name::String
                       ,items :: [StargazerItem]}
                deriving (Eq,Show,Data,Typeable, Ord)

-- get the items for Stargazer
getItems :: Stargazer -> [StargazerItem]
getItems (Stargazer _ _ _ _ items) = items

compareStargazerItems :: [StargazerItem] -> [StargazerItem] -> [String]
compareStargazerItems [] []  = []
compareStargazerItems [] _   = []
compareStargazerItems _  []  = []
compareStargazerItems (x1:xs1) (x2:xs2)
    | x1 ==  x2     = (user_name x1):compareStargazerItems  xs1 xs2
    | x1 <  x2      = compareStargazerItems  xs1 (x2:xs2)
    | otherwise     = compareStargazerItems (x1:xs1) xs2

-- processItem :: [StargazerItem] -> String -> IO String


innerProcess :: [StargazerItem] -> String -> IO()
innerProcess file1ItemsSort json2 = do
  let file2Stargazer = decodeJSON json2 :: Stargazer
  let file2Items = getItems file2Stargazer
  let file2ItemsSort = sort file2Items
  let difference1 =  compareStargazerItems file1ItemsSort file2ItemsSort
  putStrLn $ concat [(full_name file2Stargazer), "," , show $ length difference1]


catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

processItem :: [StargazerItem] -> String -> IO()
processItem file1ItemsSort file2 = do
  json2 <- readFile ("/home/pj/data/github/stargazers/" ++ file2)
  let handler = (\ _ -> putStr "") :: SomeException -> IO()
  catch (innerProcess file1ItemsSort json2) handler
    -- let file2Stargazer = decodeJSON json2 :: Stargazer
    -- let file2Items = getItems file2Stargazer
    -- let file2ItemsSort = sort file2Items
    -- let difference1 =  compareStargazerItems file1ItemsSort file2ItemsSort
    -- putStrLn $ concat [(full_name file2Stargazer), "," , show $ length difference1]


main = do
  [file1] <- getArgs
  files <- getDirectoryContents "data/"
  let fileToProcess = [x1 | x1 <- files, x1 /= ".", x1 /= ".." ]
  file1json <- readFile ("/home/pj/data/github/stargazers/" ++ file1)
  let file1Stargazer = decodeJSON file1json :: Stargazer
  let file1Items = getItems file1Stargazer
  let file1ItemsSort = sort file1Items
  -- let results = mapM (\x -> [processItem file1ItemsSort x]) fileToProcess
  -- let handler = (\ e -> print "") :: Exception -> IO()

  forM_ fileToProcess (processItem file1ItemsSort)
  -- putStrLn $ show (length fileToProcess)
