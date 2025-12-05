// URL base do backend - ajuste conforme necessário
const API_URL = 'http://localhost:8080';

// Elementos do formulário de Usuário
const formUsuario = document.getElementById('formUsuario');
const usuarioIdInput = document.getElementById('usuarioId');
const nomeUsuarioInput = document.getElementById('nome');
const emailInput = document.getElementById('email');
const idadeInput = document.getElementById('idade');
const btnSalvarUsuario = document.getElementById('btnSalvarUsuario');
const btnCancelarUsuario = document.getElementById('btnCancelarUsuario');
const btnCarregarUsuarios = document.getElementById('btnCarregarUsuarios');
const corpoTabelaUsuarios = document.getElementById('corpoTabelaUsuarios');

// Elementos do formulário de Produto
const formProduto = document.getElementById('formProduto');
const produtoIdInput = document.getElementById('produtoId');
const nomeProdutoInput = document.getElementById('nomeProduto');
const descricaoInput = document.getElementById('descricao');
const precoInput = document.getElementById('preco');
const quantidadeInput = document.getElementById('quantidade');
const btnSalvarProduto = document.getElementById('btnSalvarProduto');
const btnCancelarProduto = document.getElementById('btnCancelarProduto');
const btnCarregarProdutos = document.getElementById('btnCarregarProdutos');
const corpoTabelaProdutos = document.getElementById('corpoTabelaProdutos');

// ========== FUNÇÕES PARA USUÁRIOS ==========

// Carregar usuários
async function carregarUsuarios() {
    try {
        const response = await fetch(`${API_URL}/usuarios`);
        if (!response.ok) throw new Error('Erro ao carregar usuários');
        const usuarios = await response.json();
        exibirUsuarios(usuarios);
    } catch (error) {
        alert(error.message);
    }
}

// Exibir usuários na tabela
function exibirUsuarios(usuarios) {
    corpoTabelaUsuarios.innerHTML = '';
    usuarios.forEach(usuario => {
        const tr = document.createElement('tr');
        tr.innerHTML = `
            <td>${usuario.id || usuario._id}</td>
            <td>${usuario.nome}</td>
            <td>${usuario.email}</td>
            <td>${usuario.idade}</td>
            <td>
                <button onclick="editarUsuario('${usuario.id || usuario._id}')">Editar</button>
                <button onclick="excluirUsuario('${usuario.id || usuario._id}')">Excluir</button>
            </td>
        `;
        corpoTabelaUsuarios.appendChild(tr);
    });
}

// Criar ou atualizar usuário
formUsuario.addEventListener('submit', async (e) => {
    e.preventDefault();
    
    const usuario = {
        nome: nomeUsuarioInput.value,
        email: emailInput.value,
        idade: parseInt(idadeInput.value) || 0
    };
    
    const id = usuarioIdInput.value;
    const url = id ? `${API_URL}/usuarios/${id}` : `${API_URL}/usuarios`;
    const method = id ? 'PUT' : 'POST';
    
    try {
        const response = await fetch(url, {
            method,
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(usuario)
        });
        
        if (!response.ok) throw new Error('Erro ao salvar usuário');
        
        limparFormularioUsuario();
        carregarUsuarios();
    } catch (error) {
        alert(error.message);
    }
});

// Editar usuário
window.editarUsuario = async (id) => {
    try {
        const response = await fetch(`${API_URL}/usuarios/${id}`);
        if (!response.ok) throw new Error('Erro ao carregar usuário');
        const usuario = await response.json();
        
        usuarioIdInput.value = usuario.id || usuario._id;
        nomeUsuarioInput.value = usuario.nome;
        emailInput.value = usuario.email;
        idadeInput.value = usuario.idade;
        
        window.scrollTo({ top: 0, behavior: 'smooth' });
    } catch (error) {
        alert(error.message);
    }
};

// Excluir usuário
window.excluirUsuario = async (id) => {
    if (!confirm('Tem certeza que deseja excluir este usuário?')) return;
    
    try {
        const response = await fetch(`${API_URL}/usuarios/${id}`, { method: 'DELETE' });
        if (!response.ok) throw new Error('Erro ao excluir usuário');
        
        carregarUsuarios();
    } catch (error) {
        alert(error.message);
    }
};

// Limpar formulário de usuário
function limparFormularioUsuario() {
    usuarioIdInput.value = '';
    formUsuario.reset();
}

// Cancelar edição de usuário
btnCancelarUsuario.addEventListener('click', limparFormularioUsuario);

// Carregar usuários ao clicar no botão
btnCarregarUsuarios.addEventListener('click', carregarUsuarios);

// ========== FUNÇÕES PARA PRODUTOS ==========

// Carregar produtos
async function carregarProdutos() {
    try {
        const response = await fetch(`${API_URL}/produtos`);
        if (!response.ok) throw new Error('Erro ao carregar produtos');
        const produtos = await response.json();
        exibirProdutos(produtos);
    } catch (error) {
        alert(error.message);
    }
}

// Exibir produtos na tabela
function exibirProdutos(produtos) {
    corpoTabelaProdutos.innerHTML = '';
    produtos.forEach(produto => {
        const tr = document.createElement('tr');
        tr.innerHTML = `
            <td>${produto.id || produto._id}</td>
            <td>${produto.nome}</td>
            <td>${produto.descricao || ''}</td>
            <td>${parseFloat(produto.preco || 0).toFixed(2)}</td>
            <td>${produto.quantidade || 0}</td>
            <td>
                <button onclick="editarProduto('${produto.id || produto._id}')">Editar</button>
                <button onclick="excluirProduto('${produto.id || produto._id}')">Excluir</button>
            </td>
        `;
        corpoTabelaProdutos.appendChild(tr);
    });
}

// Criar ou atualizar produto
formProduto.addEventListener('submit', async (e) => {
    e.preventDefault();
    
    const produto = {
        nome: nomeProdutoInput.value,
        descricao: descricaoInput.value,
        preco: parseFloat(precoInput.value) || 0,
        quantidade: parseInt(quantidadeInput.value) || 0
    };
    
    const id = produtoIdInput.value;
    const url = id ? `${API_URL}/produtos/${id}` : `${API_URL}/produtos`;
    const method = id ? 'PUT' : 'POST';
    
    try {
        const response = await fetch(url, {
            method,
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(produto)
        });
        
        if (!response.ok) throw new Error('Erro ao salvar produto');
        
        limparFormularioProduto();
        carregarProdutos();
    } catch (error) {
        alert(error.message);
    }
});

// Editar produto
window.editarProduto = async (id) => {
    try {
        const response = await fetch(`${API_URL}/produtos/${id}`);
        if (!response.ok) throw new Error('Erro ao carregar produto');
        const produto = await response.json();
        
        produtoIdInput.value = produto.id || produto._id;
        nomeProdutoInput.value = produto.nome;
        descricaoInput.value = produto.descricao || '';
        precoInput.value = produto.preco || 0;
        quantidadeInput.value = produto.quantidade || 0;
        
        window.scrollTo({ top: document.querySelector('.crud-section:nth-child(2)').offsetTop, behavior: 'smooth' });
    } catch (error) {
        alert(error.message);
    }
};

// Excluir produto
window.excluirProduto = async (id) => {
    if (!confirm('Tem certeza que deseja excluir este produto?')) return;
    
    try {
        const response = await fetch(`${API_URL}/produtos/${id}`, { method: 'DELETE' });
        if (!response.ok) throw new Error('Erro ao excluir produto');
        
        carregarProdutos();
    } catch (error) {
        alert(error.message);
    }
};

// Limpar formulário de produto
function limparFormularioProduto() {
    produtoIdInput.value = '';
    formProduto.reset();
}

// Cancelar edição de produto
btnCancelarProduto.addEventListener('click', limparFormularioProduto);

// Carregar produtos ao clicar no botão
btnCarregarProdutos.addEventListener('click', carregarProdutos);

// ========== INICIALIZAÇÃO ==========
// Carregar dados iniciais (opcional)
// document.addEventListener('DOMContentLoaded', () => {
//     carregarUsuarios();
//     carregarProdutos();
// });