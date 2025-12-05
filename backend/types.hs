{-# LANGUAGE DeriveGeneric #-}
module Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Usuario = Usuario
  { usuarioId :: Int
  , nome :: String
  , email :: String
  } deriving (Show, Generic)

instance ToJSON Usuario
instance FromJSON Usuario

data Produto = Produto
  { produtoId :: Int
  , nomeProduto :: String
  , preco :: Double
  } deriving (Show, Generic)

instance ToJSON Produto
instance FromJSON Produto
