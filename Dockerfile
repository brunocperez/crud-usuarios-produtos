FROM haskell:9.6.7 AS build

WORKDIR /app

# Instalar dependências de maneira mais robusta
RUN apt-get update && \
    apt-get install -y \
    wget \
    gnupg \
    lsb-release \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && apt-get update && \
    apt-get install -y \
    postgresql-server-dev-15 \
    libpq-dev \
    libgmp-dev \
    zlib1g-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Configurar variáveis de ambiente para postgresql-libpq
ENV PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig

COPY projetoFilmes.cabal cabal.project ./

RUN cabal update

# Forçar flags para postgresql-libpq
RUN cabal install postgresql-libpq --flags="+use-pkg-config" --constraint="postgresql-libpq +use-pkg-config"

COPY . .

RUN cabal build

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    libgmp10 \
    libpq5 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/dist-newstyle/build/*/*/projetoFilmes-*/x/projetoFilmes/build/projetoFilmes .

EXPOSE 8080

CMD ["/app/projetoFilmes"]