{-# LANGUAGE DeriveGeneric #-}

module Api.Model where

import Data.Aeson
import GHC.Generics

data Calculadora = Calculadora {
    n1 :: Int,
    n2 :: Int
} deriving (Show, Generic)

instance FromJSON Calculadora
instance ToJSON Calculadora

data ResultadoResponse = ResultadoResponse {
    resultado :: Int
} deriving (Show, Generic)

instance ToJSON ResultadoResponse

data Filme = Filme {
    id :: Int,
    titulo :: String,
    diretor :: String,
    ano :: Int
} deriving (Show, Generic)

instance FromJSON Filme
instance ToJSON Filme

data FilmeResponse = FilmeResponse {
    filmes :: [Filme]
} deriving (Show, Generic)

instance ToJSON FilmeResponse