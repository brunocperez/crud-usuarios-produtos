{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import Usuarios
import Produtos

import Control.Concurrent.MVar
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-- Rotas da API
type API =
       "usuarios"  :> Get '[JSON] [Usuario]
  :<|> "usuarios"  :> ReqBody '[JSON] Usuario :> Post '[JSON] Usuario
  :<|> "usuarios"  :> Capture "id" Int :> ReqBody '[JSON] Usuario :> Put '[JSON] (Maybe Usuario)
  :<|> "usuarios"  :> Capture "id" Int :> Delete '[JSON] Bool
  :<|> "produtos"  :> Get '[JSON] [Produto]
  :<|> "produtos"  :> ReqBody '[JSON] Produto :> Post '[JSON] Produto
  :<|> "produtos"  :> Capture "id" Int :> ReqBody '[JSON] Produto :> Put '[JSON] (Maybe Produto)
  :<|> "produtos"  :> Capture "id" Int :> Delete '[JSON] Bool

api :: Proxy API
api = Proxy

-- Servidor inicial (retorna apenas as respostas vazias por enquanto)
server :: UsuariosDB -> ProdutosDB -> Server API
server usuariosDB produtosDB =
       listarUsuarios usuariosDB
  :<|> criarUsuario usuariosDB
  :<|> atualizarUsuario usuariosDB
  :<|> deletarUsuario usuariosDB
  :<|> listarProdutos produtosDB
  :<|> criarProduto produtosDB
  :<|> atualizarProduto produtosDB
  :<|> deletarProduto produtosDB

app :: UsuariosDB -> ProdutosDB -> Application
app u p = serve api (server u p)

main :: IO ()
main = do
    usuarios <- newMVar []
    produtos <- newMVar []
    putStrLn "Servidor rodando em http://localhost:8080"
    run 8080 (app usuarios produtos)
