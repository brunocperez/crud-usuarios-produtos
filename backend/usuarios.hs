module Usuarios where

import Types
import Control.Concurrent.MVar

type UsuariosDB = MVar [Usuario]

listarUsuarios :: UsuariosDB -> IO [Usuario]
listarUsuarios _ = pure []

criarUsuario :: UsuariosDB -> Usuario -> IO Usuario
criarUsuario _ usuario = pure usuario

atualizarUsuario :: UsuariosDB -> Int -> Usuario -> IO (Maybe Usuario)
atualizarUsuario _ _ usuario = pure (Just usuario)

deletarUsuario :: UsuariosDB -> Int -> IO Bool
deletarUsuario _ _ = pure True
