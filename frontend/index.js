function servidor() {
    let x = document.querySelector("#n1").value;
    let y = document.querySelector("#n2").value;
    
    if (!x || !y) {
        alert("Por favor, preencha ambos os números!");
        return;
    }
    
    fetch("http://localhost:8080/soma", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({n1: parseInt(x), n2: parseInt(y)}),
    })
    .then(response => response.json())
    .then(json => {
        document.querySelector("#resultado").innerHTML = 
            `🎯 Resultado: ${x} + ${y} = <span style="color:#764ba2;font-size:28px;">${json.resultado}</span>`;
    })
    .catch(error => {
        document.querySelector("#resultado").innerHTML = 
            `❌ Erro: ${error}`;
    });
}

function teste() {
    document.querySelector("#btn").addEventListener("click", () => {
        servidor();
    });
    
    // Adicionar suporte para Enter
    document.querySelector("#n1").addEventListener("keypress", function(e) {
        if (e.key === 'Enter') servidor();
    });
    
    document.querySelector("#n2").addEventListener("keypress", function(e) {
        if (e.key === 'Enter') servidor();
    });
}

window.onload = teste;