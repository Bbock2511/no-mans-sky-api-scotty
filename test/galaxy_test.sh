#!/bin/bash

# --- CONFIGURAÇÃO ---
BASE_URL="http://localhost:3000" # URL da porta

echo "Executando demonstração de requisições para a API de Galáxias..."
echo "Certifique-se de que o servidor Scotty está rodando em $BASE_URL"
echo "================================================================"

# Assumimos que o primeiro item criado no banco de dados limpo terá o ID 1.
GALAXY_ID=1

# --- 1. CRIAR UMA NOVA GALÁXIA (POST) ---
echo -e "\n--- 1. Enviando POST para /galaxies para criar 'Andromeda' ---\n"
PAYLOAD_CREATE='{"galaxyNameInput": "Andromeda", "galaxyTypeInput": "Spiral", "galaxyNoteInput": "Vizinha"}'

curl -X POST \
  -H "Content-Type: application/json" \
  -d "$PAYLOAD_CREATE" \
  "$BASE_URL/galaxies"

echo -e "\n================================================================"


# --- 2. BUSCAR A GALÁXIA CRIADA (GET by ID) ---
echo -e "\n--- 2. Enviando GET para /galaxies/$GALAXY_ID para buscar 'Andromeda' ---\n"

curl "$BASE_URL/galaxies/$GALAXY_ID"

echo -e "\n================================================================"


# --- 3. ATUALIZAR A GALÁXIA (PUT) ---
echo -e "\n--- 3. Enviando PUT para /galaxies/$GALAXY_ID para renomear para 'Andromeda II' ---\n"
PAYLOAD_UPDATE='{"galaxyNameInput": "Andromeda II", "galaxyTypeInput": "Dwarf", "galaxyNoteInput": "Atualizada"}'

curl -X PUT \
  -H "Content-Type: application/json" \
  -d "$PAYLOAD_UPDATE" \
  "$BASE_URL/galaxies/$GALAXY_ID"

echo -e "\n================================================================"


# --- 4. VERIFICAR A ATUALIZAÇÃO (GET by ID) ---
echo -e "\n--- 4. Enviando GET para /galaxies/$GALAXY_ID para verificar a atualização ---\n"

curl "$BASE_URL/galaxies/$GALAXY_ID"

echo -e "\n================================================================"


# --- 5. LISTAR TODAS AS GALÁXIAS (GET All) ---
echo -e "\n--- 5. Enviando GET para /galaxies para listar todas as galáxias ---\n"

curl "$BASE_URL/galaxies"

echo -e "\n================================================================"


# --- 6. DELETAR A GALÁXIA (DELETE) ---
echo -e "\n--- 6. Enviando DELETE para /galaxies/$GALAXY_ID para remover a galáxia ---\n"

curl -X DELETE "$BASE_URL/galaxies/$GALAXY_ID"

echo -e "\n================================================================"


# --- 7. VERIFICAR A EXCLUSÃO (GET by ID) ---
echo -e "\n--- 7. Enviando GET para /galaxies/$GALAXY_ID para confirmar que foi deletada (esperado 404) ---\n"

curl "$BASE_URL/galaxies/$GALAXY_ID"

echo -e "\n\nDemonstração concluída."