document.addEventListener('DOMContentLoaded', function() {
    const form = document.getElementById('form-filme');
    const btnCarregar = document.getElementById('btn-carregar');
    const filmesContainer = document.getElementById('filmes-container');
    const resultadoDiv = document.getElementById('resultado');

    form.addEventListener('submit', function(event) {
        event.preventDefault();
        adicionarFilme();
    });

    btnCarregar.addEventListener('click', function() {
        carregarFilmes();
    });

    function adicionarFilme() {
        const titulo = document.getElementById('titulo').value;
        const diretor = document.getElementById('diretor').value;
        const ano = document.getElementById('ano').value;

        if (!titulo || !diretor || !ano) {
            mostrarResultado('Preencha todos os campos!', 'error');
            return;
        }

        const filme = {
            id: 0,
            titulo: titulo,
            diretor: diretor,
            ano: parseInt(ano)
        };

        fetch('http://localhost:8080/filme', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(filme)
        })
        .then(response => {
            if (!response.ok) throw new Error('Erro na requisição');
            return response.json();
        })
        .then(json => {
            mostrarResultado(`✅ Filme "${titulo}" adicionado com ID: ${json.resultado}`, 'success');
            form.reset();
            carregarFilmes();
        })
        .catch(error => {
            mostrarResultado(`❌ Erro ao adicionar filme: ${error.message}`, 'error');
        });
    }

    function carregarFilmes() {
        fetch('http://localhost:8080/filmes')
        .then(response => {
            if (!response.ok) throw new Error('Erro ao carregar filmes');
            return response.json();
        })
        .then(json => {
            filmesContainer.innerHTML = '';
            
            if (json.filmes.length === 0) {
                filmesContainer.innerHTML = `
                    <div class="empty-state">
                        <h3>Nenhum filme cadastrado</h3>
                        <p>Adicione seu primeiro filme usando o formulário ao lado!</p>
                    </div>
                `;
                return;
            }
            
            json.filmes.forEach(filme => {
                const movieCard = document.createElement('div');
                movieCard.className = 'movie-card';
                movieCard.innerHTML = `
                    <h3>${filme.titulo}</h3>
                    <p>🎥 <strong>Diretor:</strong> ${filme.diretor}</p>
                    <p>📅 <strong>Ano:</strong> <span class="ano">${filme.ano}</span></p>
                    <div class="id-badge">ID: ${filme.id}</div>
                `;
                filmesContainer.appendChild(movieCard);
            });
        })
        .catch(error => {
            mostrarResultado(`❌ Erro ao carregar filmes: ${error.message}`, 'error');
        });
    }

    function mostrarResultado(mensagem, tipo) {
        resultadoDiv.textContent = mensagem;
        resultadoDiv.className = tipo;
        resultadoDiv.style.display = 'block';
        
        setTimeout(() => {
            resultadoDiv.style.display = 'none';
        }, 5000);
    }

    // Carregar filmes automaticamente ao iniciar
    carregarFilmes();
});