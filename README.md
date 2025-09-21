# Scotty + Stack Codespace


Abra este repositório no GitHub Codespaces; o container será construído automaticamente com Haskell + Stack.


**Portas:** 3000 (Scotty)


**Comandos úteis**


- `stack setup` — instala GHC/local toolchain
- `stack build` — build do projeto
- `stack exec scotty-api-exe` — executa o servidor
- `./watch.sh` — watcher simples que reconstrói e reinicia o binário quando arquivos mudarem (usa `entr`)


Se algo der errado com o resolver (`stack.yaml`), atualize `resolver` para uma versão de GHC/LTS compatível com a imagem `haskell:latest` ou rode `stack setup` para deixar o ambiente consistente.