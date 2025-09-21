# Use an official Haskell image as a base image
FROM haskell:latest


# Set up the working directory
WORKDIR /workspace


# Evita [Y/N] ao instalar dependências
ENV DEBIAN_FRONTEND=noninteractive


# Install system deps useful for Haskell web dev and sqlite
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    git \
    sqlite3 \
    libsqlite3-dev \
    pkg-config \
    build-essential \
    inotify-tools \
    entr \
    && rm -rf /var/lib/apt/lists/*


RUN curl -sSL https://get.haskellstack.org/ | sh || true


# Criar um non-root user
RUN useradd -m devuser || true
USER devuser


# Expose default dev port
EXPOSE 3000


# Default workdir
WORKDIR /workspace


# Convenience entrypoint: keep container running for Codespaces
CMD ["/bin/bash"]