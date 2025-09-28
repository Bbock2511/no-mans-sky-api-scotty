#!/bin/bash

# --- CONFIGURAÇÃO ---
BASE_URL="http://localhost:3000"

echo "Executando demonstração de requisições para a API de Sistemas Solares..."
echo "Certifique-se de que o servidor Scotty está rodando em $BASE_URL"
echo "================================================================"

# --- ETAPA 0: SETUP - CRIAR UMA GALÁXIA PAI ---
echo -e "\n--- 0. PREPARAÇÃO: Enviando POST para /galaxies para criar uma galáxia de referência ---\n"
GALAXY_PAYLOAD='{"galaxyNameInput": "Euclid", "galaxyTypeInput": "Normal", "galaxyNoteInput": null}'

# Capturamos a resposta para referência, mas não a usamos ativamente.
curl -s -X POST \
  -H "Content-Type: application/json" \
  -d "$GALAXY_PAYLOAD" \
  "$BASE_URL/galaxies"
echo -e "\nGaláxia de referência criada."

# Assumimos que a galáxia criada tem ID 1 e o sistema solar que criaremos também terá ID 1.
GALAXY_ID=1
SOLAR_SYSTEM_ID=1

echo "================================================================"


# --- 1. CRIAR UM NOVO SISTEMA SOLAR (POST) ---
echo -e "\n--- 1. Enviando POST para /solar-systems para criar 'Sol' ---\n"
PAYLOAD_CREATE='{"ssNameInput": "Sol", "ssRaceInput": "Human", "ssEconomyInput": "Trading", "ssConflictInput": "Low", "ssNotesInput": "Sistema natal", "ssGalaxyIdInput": '$GALAXY_ID'}'

curl -s -X POST \
  -H "Content-Type: application/json" \
  -d "$PAYLOAD_CREATE" \
  "$BASE_URL/solar-systems"

echo -e "\n================================================================"


# --- 2. BUSCAR O SISTEMA SOLAR CRIADO (GET by ID) ---
echo -e "\n--- 2. Enviando GET para /solar-systems/$SOLAR_SYSTEM_ID para buscar 'Sol' ---\n"

curl -s "$BASE_URL/solar-systems/$SOLAR_SYSTEM_ID"

echo -e "\n================================================================"


# --- 3. ATUALIZAR O SISTEMA SOLAR (PUT) ---
# No seu controller, a rota de update está como POST, vamos usar PUT para seguir o padrão REST.
# Por favor, ajuste seu Controller.SolarSystemController de 'post "/solar-systems/:id"' para 'put "/solar-systems/:id"'
echo -e "\n--- 3. Enviando PUT para /solar-systems/$SOLAR_SYSTEM_ID para atualizar dados ---\n"
PAYLOAD_UPDATE='{"ssNameInput": "Sol II", "ssRaceInput": "Gek", "ssEconomyInput": "Mining", "ssConflictInput": "High", "ssNotesInput": "Atualizado", "ssGalaxyIdInput": '$GALAXY_ID'}'

curl -s -X PUT \
  -H "Content-Type: application/json" \
  -d "$PAYLOAD_UPDATE" \
  "$BASE_URL/solar-systems/$SOLAR_SYSTEM_ID"

echo -e "\n================================================================"


# --- 4. VERIFICAR A ATUALIZAÇÃO (GET by ID) ---
echo -e "\n--- 4. Enviando GET para /solar-systems/$SOLAR_SYSTEM_ID para verificar a atualização ---\n"

curl -s "$BASE_URL/solar-systems/$SOLAR_SYSTEM_ID"

echo -e "\n================================================================"


# --- 5. LISTAR TODOS OS SISTEMAS SOLARES (GET All) ---
echo -e "\n--- 5. Enviando GET para /solar-systems para listar todos ---\n"

curl -s "$BASE_URL/solar-systems"

echo -e "\n================================================================"


# --- 6. DELETAR O SISTEMA SOLAR (DELETE) ---
echo -e "\n--- 6. Enviando DELETE para /solar-systems/$SOLAR_SYSTEM_ID para remover ---\n"

curl -s -X DELETE "$BASE_URL/solar-systems/$SOLAR_SYSTEM_ID"

echo -e "\n================================================================"


# --- 7. VERIFICAR A EXCLUSÃO (GET by ID) ---
echo -e "\n--- 7. Enviando GET para /solar-systems/$SOLAR_SYSTEM_ID para confirmar que foi deletado ---\n"

curl -s "$BASE_URL/solar-systems/$SOLAR_SYSTEM_ID"

echo -e "\n\nDemonstração para Sistemas Solares concluída."