{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}
import qualified Web.Scotty as S

import Data.Monoid ((<>))
import Control.Monad (forM_,forM)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

import Text.Blaze
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text

import Database.MySQL.Simple as Db

import qualified Data.Text as T
import Data.Text (Text)

import Data.Text.Lazy.Builder

import Data.Pool

indexHandler pool = S.get "/" $ do
  let queryDb conn = query_ conn "select id,name,count,price from Items"
  items <- liftIO $ withResource pool queryDb
  S.html $ renderHtml $ do
    table $ do
      forM_ items $ \(id'::Int,name::T.Text,count::Double,price::Double) -> do
        tr $ do
          td $ toHtml id'
          td $ toHtml name
          td $ toHtml price
          td $ toHtml count

main = do
  let createConn = connect defaultConnectInfo {  
                                        connectUser = "root",
                                        connectPassword = "root",
                                        connectDatabase = "ScottyTest"
                                      }
  pool <- createPool createConn close 10 100 100
  S.scotty 3000 $ do
    indexHandler pool