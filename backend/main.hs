{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Network.Wai.Handler.Warp (run)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Control.Concurrent.MVar

import Types

type UsuarioAPI =
       "usuarios" :> Get '[JSON] [Usuario]
  :<|> "usuarios" :> ReqBody '[JSON] Usuario :> Post '[JSON] Usuario

type ProdutoAPI =
       "produtos" :> Get '[JSON] [Produto]
  :<|> "produtos" :> ReqBody '[JSON] Produto :> Post '[JSON] Produto

type API = UsuarioAPI :<|> ProdutoAPI

server :: MVar [Usuario] -> MVar [Produto] -> Server API
server mvUsers mvProducts =
       (getUsuarios mvUsers :<|> postUsuario mvUsers)
  :<|> (getProdutos mvProducts :<|> postProduto mvProducts)
  where
    getUsuarios mv = liftIO $ readMVar mv
    postUsuario mv user = do
      liftIO $ modifyMVar_ mv (\xs -> return (xs ++ [user]))
      return user
    getProdutos mv = liftIO $ readMVar mv
    postProduto mv prod = do
      liftIO $ modifyMVar_ mv (\xs -> return (xs ++ [prod]))
      return prod

api :: Proxy API
api = Proxy

app :: MVar [Usuario] -> MVar [Produto] -> Application
app mvU mvP = serve api (server mvU mvP)

main :: IO ()
main = do
  mvUsers <- newMVar ([] :: [Usuario])
  mvProducts <- newMVar ([] :: [Produto])
  putStrLn "Server placeholder (scaffold) — endpoints: /usuarios /produtos"
  run 8080 (app mvUsers mvProducts)
