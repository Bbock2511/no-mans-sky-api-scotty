#!/usr/bin/env bash
set -e

echo "Iniciando watcher com entr..."

find app src -type f -name "*.hs" | \
  entr -r bash -c '
    echo "==> Recompilando..."
    stack build --fast
    stack exec scotty-api-exe &
  '
