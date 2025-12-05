module Produtos where

import Types
import Control.Concurrent.MVar

type ProdutosDB = MVar [Produto]

listarProdutos :: ProdutosDB -> IO [Produto]
listarProdutos _ = pure []

criarProduto :: ProdutosDB -> Produto -> IO Produto
criarProduto _ produto = pure produto

atualizarProduto :: ProdutosDB -> Int -> Produto -> IO (Maybe Produto)
atualizarProduto _ _ produto = pure (Just produto)

deletarProduto :: ProdutosDB -> Int -> IO Bool
deletarProduto _ _ = pure True
