{-# LANGUAGE DeriveGeneric #-}

module Api.Model where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Filme = Filme
  { filmeId :: Int
  , titulo  :: String
  , diretor :: String
  , ano     :: Int
  } deriving (Show, Generic)

instance FromJSON Filme
instance ToJSON Filme

data FilmeResponse = FilmeResponse
  { filmes :: [Filme]
  } deriving (Show, Generic)

instance ToJSON FilmeResponse

data ResultadoResponse = ResultadoResponse
  { idGerado :: Int
  } deriving (Show, Generic)

instance ToJSON ResultadoResponse