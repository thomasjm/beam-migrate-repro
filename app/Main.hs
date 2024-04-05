module Main (main) where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL8
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate as PM
import Lib


main :: IO ()
main = do
  putStrLn "Initial setup:"
  forM_ (PM.migrateScript (initialSetupStep @Postgres)) BL8.putStrLn

  putStrLn "-------------------------"

  let desiredPredicates = collectChecks $ dbChecked @Postgres
  putStrLn "Got desired:"
  forM_ desiredPredicates $ \pred' -> do
    putStrLn $ show pred'
