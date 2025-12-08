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
import Data.Aeson (Value, object, (.=))


type API =
         "filme"  :> ReqBody '[JSON] Filme  :> Post '[JSON] ResultadoResponse
    :<|> "filme"  :> Capture "id" Int       :> Get '[JSON] Value
    :<|> "filme"  :> Capture "id" Int       :> ReqBody '[JSON] Filme :> Put '[JSON] ResultadoResponse
    :<|> "filme"  :> Capture "id" Int       :> DeleteNoContent
    :<|> "filme"  :> Verb 'OPTIONS 200 '[JSON] ()
    :<|> "filme"  :> Capture "id" Int       :> Verb 'OPTIONS 200 '[JSON] ()
    :<|> "filmes" :> Get '[JSON] FilmeResponse


handlerFilme :: Connection -> Filme -> Handler ResultadoResponse
handlerFilme conn filme = do
    res <- liftIO $
        query conn
            "INSERT INTO Filme (titulo, diretor, ano) VALUES (?,?,?) RETURNING id"
            (titulo filme, diretor filme, ano filme)

    case res of
        [Only novoId] -> pure (ResultadoResponse novoId)
        _ -> throwError err500


handlerFilmeTodos :: Connection -> Handler FilmeResponse
handlerFilmeTodos conn = do
    rows <- liftIO $ query_ conn
        "SELECT id, titulo, diretor, ano FROM Filme ORDER BY id DESC"
    
    let filmesDb = map (\(id', t, d, a) -> Filme id' t d a) rows
    pure (FilmeResponse filmesDb)


handlerFilmePorId :: Connection -> Int -> Handler Value
handlerFilmePorId conn ident = do
    res <- liftIO $
        query conn
            "SELECT id, titulo, diretor, ano FROM Filme WHERE id = ?"
            (Only ident)

    case res of
        [(id', t, d, a)] ->
            pure $ object [ "filme" .= Filme id' t d a ]
        _ -> throwError err404


handlerAtualizar :: Connection -> Int -> Filme -> Handler ResultadoResponse
handlerAtualizar conn ident filme = do
    _ <- liftIO $
        execute conn
            "UPDATE Filme SET titulo=?, diretor=?, ano=? WHERE id=?"
            (titulo filme, diretor filme, ano filme, ident)
    pure (ResultadoResponse ident)


handlerExcluir :: Connection -> Int -> Handler NoContent
handlerExcluir conn ident = do
    _ <- liftIO $
        execute conn "DELETE FROM Filme WHERE id=?" (Only ident)
    pure NoContent


options :: Handler ()
options = pure ()

optionsId :: Connection -> Int -> Handler ()
optionsId _ _ = pure ()


server :: Connection -> Server API
server conn =
         handlerFilme conn
    :<|> handlerFilmePorId conn
    :<|> handlerAtualizar conn
    :<|> handlerExcluir conn
    :<|> options
    :<|> optionsId conn
    :<|> handlerFilmeTodos conn


addCorsHeader :: Middleware
addCorsHeader app' req resp =
    app' req $ \res ->
        resp $
            mapResponseHeaders
                ( \hs ->
                    [ ("Access-Control-Allow-Origin", "*")
                    , ("Access-Control-Allow-Headers", "Content-Type, Authorization")
                    , ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
                    ] ++ hs
                )
                res

app :: Connection -> Application
app conn = addCorsHeader (serve (Proxy @API) (server conn))