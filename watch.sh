#!/usr/bin/env bash
# simples watcher usando entr (instalado no container via apt)
# vai rebuildar e executar o binário quando arquivos Haskell mudarem


set -e


PROJECT_EXE=scotty-api-exe


stack build --fast


# roda o binário em background, mata ao reiniciar
run() {
    stack exec -- $PROJECT_EXE &
    child=$!
}


# build initial and run
stack build --fast
run


# watch .hs files and on change rebuild+restart
find . -type f \( -name "*.hs" -o -name "package.yaml" -o -name "stack.yaml" \) 
    | entr -r sh -c 'stack build --fast && pkill -f "stack exec -- $PROJECT_EXE" || true && stack exec -- $PROJECT_EXE &'