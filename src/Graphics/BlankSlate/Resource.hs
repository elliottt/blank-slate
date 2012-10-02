module Graphics.BlankSlate.Resource (
    Resource(..)
  , withResource
  ) where

import qualified Control.Exception as X

class Resource r where
  genResource :: IO r
  genResources :: Int -> IO [r]
  freeResource :: r -> IO ()
  freeResources :: [r] -> IO ()


withResource :: Resource r => (r -> IO a) -> IO a
withResource  = X.bracket genResource freeResource

withResources :: Resource r => Int -> ([r] -> IO a) -> IO a
withResources n = X.bracket (genResources n) freeResources
