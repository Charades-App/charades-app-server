module Main where

import           Network.HTTP.Types.Status (ok200)
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/api/v1/health" $ status ok200
