document.addEventListener('DOMContentLoaded', function() {
    const form = document.getElementById('form-filme');
    const filmeIdInput = document.getElementById('filme-id');
    const tituloInput = document.getElementById('titulo');
    const diretorInput = document.getElementById('diretor');
    const anoInput = document.getElementById('ano');
    
    const btnSalvar = document.getElementById('btn-salvar');
    const btnCancelar = document.getElementById('btn-cancelar');
    const formTitle = document.getElementById('form-title');
    
    const filmesContainer = document.getElementById('filmes-container');
    const resultadoDiv = document.getElementById('resultado');

    const API_URL = 'http://localhost:8080';

    form.addEventListener('submit', function(event) {
        event.preventDefault();
        salvarFilme();
    });

    btnCancelar.addEventListener('click', resetarFormulario);

    function salvarFilme() {
        const id = filmeIdInput.value;
        const titulo = tituloInput.value;
        const diretor = diretorInput.value;
        const ano = parseInt(anoInput.value);

        const filme = {
            filmeId: id ? parseInt(id) : 0, 
            titulo: titulo,
            diretor: diretor,
            ano: ano
        };

        const metodo = id ? 'PUT' : 'POST';
        const url = id ? `${API_URL}/filme/${id}` : `${API_URL}/filme`;

        fetch(url, {
            method: metodo,
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(filme)
        })
        .then(response => {
            if (!response.ok) throw new Error('Erro na requisição');
            return response.json();
        })
        .then(json => {
            const acao = id ? 'atualizado' : 'adicionado';
            mostrarResultado(`✅ Filme ${acao} com sucesso!`, 'success');
            resetarFormulario();
            carregarFilmes();
        })
        .catch(error => {
            mostrarResultado(`❌ Erro: ${error.message}`, 'error');
        });
    }

    window.carregarFilmes = function() {
        fetch(`${API_URL}/filmes`)
        .then(response => response.json())
        .then(json => {
            filmesContainer.innerHTML = '';
           
            if (!json.filmes || json.filmes.length === 0) {
                filmesContainer.innerHTML = '<p style="text-align:center; padding:20px;">Nenhum filme cadastrado.</p>';
                return;
            }
           
            json.filmes.forEach(filme => {
                const card = document.createElement('div');
                card.className = 'movie-card';
                card.innerHTML = `
                    <h3>${filme.titulo}</h3>
                    <p>🎥 <strong>Diretor:</strong> ${filme.diretor}</p>
                    <p>📅 <strong>Ano:</strong> <span class="ano">${filme.ano}</span></p>
                    <div class="actions">
                        <button class="btn-edit" onclick="iniciarEdicao(${filme.filmeId}, '${escapeHtml(filme.titulo)}', '${escapeHtml(filme.diretor)}', ${filme.ano})">✏️ Editar</button>
                        <button class="btn-delete" onclick="deletarFilme(${filme.filmeId})">🗑️ Excluir</button>
                    </div>
                `;
                filmesContainer.appendChild(card);
            });
        })
        .catch(err => console.error(err));
    };

    window.iniciarEdicao = function(id, titulo, diretor, ano) {
        filmeIdInput.value = id;
        tituloInput.value = titulo;
        diretorInput.value = diretor;
        anoInput.value = ano;

        formTitle.innerText = "✏️ Editar Filme";
        btnSalvar.innerText = "💾 Salvar Alterações";
        btnCancelar.style.display = 'block';
        
        form.scrollIntoView({ behavior: 'smooth' });
    };


    window.deletarFilme = function(id) {
        if(!confirm("Tem certeza que deseja excluir este filme?")) return;

        fetch(`${API_URL}/filme/${id}`, {
            method: 'DELETE'
        })
        .then(res => {
            if(res.ok) {
                mostrarResultado("🗑️ Filme excluído.", "success");
                carregarFilmes();
                if(filmeIdInput.value == id) resetarFormulario();
            } else {
                throw new Error("Falha ao excluir");
            }
        })
        .catch(err => mostrarResultado("Erro ao excluir", "error"));
    };

    function resetarFormulario() {
        form.reset();
        filmeIdInput.value = '';
        formTitle.innerText = "Adicionar Filme";
        btnSalvar.innerText = "🎬 Adicionar ao Catálogo";
        btnCancelar.style.display = 'none';
    }

    function mostrarResultado(msg, tipo) {
        resultadoDiv.textContent = msg;
        resultadoDiv.className = tipo;
        resultadoDiv.style.display = 'block';
        setTimeout(() => { resultadoDiv.style.display = 'none'; }, 4000);
    }

    function escapeHtml(text) {
        if (!text) return "";
        return text
            .replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;")
            .replace(/"/g, "&quot;")
            .replace(/'/g, "&#039;");
    }

    carregarFilmes();
});