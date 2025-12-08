{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Routes where

import Api.Model
import Data.Proxy
import Network.Wai
import Servant.API
import Servant.Server
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Control.Monad.Except

type API =
         "hello" :> Get '[PlainText] String
    :<|> "soma" :> ReqBody '[JSON] Calculadora :> Post '[JSON] ResultadoResponse
    :<|> "soma" :> Verb 'OPTIONS 200 '[JSON] ()
    :<|> "filme" :> ReqBody '[JSON] Filme :> Post '[JSON] ResultadoResponse
    :<|> "filme" :> Verb 'OPTIONS 200 '[JSON] ()
    :<|> "filmes" :> Get '[JSON] FilmeResponse

handlerFilmeTodos :: Connection -> Handler FilmeResponse
handlerFilmeTodos conn = do
    res <- liftIO $ query_ conn "SELECT id, titulo, diretor, ano FROM Filme"
    let result = map (\(id', titulo', diretor', ano') -> Filme id' titulo' diretor' ano') res
    pure (FilmeResponse result)

handlerFilme :: Connection -> Filme -> Handler ResultadoResponse
handlerFilme conn filme = do
    res <- liftIO $ query conn "INSERT INTO Filme (titulo, diretor, ano) VALUES (?,?,?) RETURNING id" 
        (titulo filme, diretor filme, ano filme)
    case res of
        [Only novoId] -> pure (ResultadoResponse novoId)
        _ -> throwError err500

handlerSoma :: Calculadora -> Handler ResultadoResponse
handlerSoma (Calculadora x y) = pure (ResultadoResponse $ x + y)

options :: Handler ()
options = pure ()

handlerHello :: Handler String
handlerHello = pure "Olá, mundo!"

server :: Connection -> Server API
server conn = handlerHello
         :<|> handlerSoma
         :<|> options
         :<|> handlerFilme conn
         :<|> options
         :<|> handlerFilmeTodos conn

addCorsHeader :: Middleware
addCorsHeader app' req resp =
    app' req $ \res ->
        resp $ mapResponseHeaders
            (\hs ->
                [ ("Access-Control-Allow-Origin", "*")
                , ("Access-Control-Allow-Headers", "Content-Type, Authorization")
                , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
                ] ++ hs
            )
            res

app :: Connection -> Application
app conn = addCorsHeader (serve (Proxy @API) (server conn))