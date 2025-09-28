#!/bin/bash

# --- CONFIGURAÇÃO ---
BASE_URL="http://localhost:3000"

echo "Executando demonstração de requisições para a API de Planetas..."
echo "Certifique-se de que o servidor Scotty está rodando em $BASE_URL"
echo "================================================================"

# --- ETAPA 0: SETUP - CRIAR DEPENDÊNCIAS (GALAXY E SOLAR SYSTEM) ---
echo -e "\n--- 0.1 PREPARAÇÃO: Criando uma Galáxia de referência (Euclid) ---\n"
GALAXY_PAYLOAD='{"galaxyNameInput": "Euclid", "galaxyTypeInput": "Normal"}'
curl -s -X POST -H "Content-Type: application/json" -d "$GALAXY_PAYLOAD" "$BASE_URL/galaxies"
echo -e "\nGaláxia 'Euclid' criada."
GALAXY_ID=1 # Assumindo ID 1

echo -e "\n--- 0.2 PREPARAÇÃO: Criando um Sistema Solar de referência (Sol) ---\n"
SOLAR_SYSTEM_PAYLOAD='{"ssNameInput": "Sol", "ssRaceInput": "Human", "ssEconomyInput": "Trading", "ssConflictInput": "Low", "ssGalaxyIdInput": '$GALAXY_ID'}'
curl -s -X POST -H "Content-Type: application/json" -d "$SOLAR_SYSTEM_PAYLOAD" "$BASE_URL/solar-systems"
echo -e "\nSistema Solar 'Sol' criado."
SOLAR_SYSTEM_ID=1 # Assumindo ID 1

echo "================================================================"


# --- 1. CRIAR UM NOVO PLANETA (POST) ---
echo -e "\n--- 1. Enviando POST para /planets para criar 'Terra' ---\n"
PAYLOAD_CREATE='{
  "planetInsertName": "Terra",
  "planetInsertWeather": "Temperado",
  "planetInsertSentinels": "Baixo",
  "planetInsertFlora": "Rica",
  "planetInsertFauna": "Abundante",
  "planetInsertResources": "Cobre, Parafina",
  "planetInsertNotes": "Planeta natal",
  "planetInsertSolarSystemId": '$SOLAR_SYSTEM_ID'
}'

curl -s -X POST \
  -H "Content-Type: application/json" \
  -d "$PAYLOAD_CREATE" \
  "$BASE_URL/planets"

PLANET_ID=1 # Assumindo que o primeiro planeta criado terá o ID 1
echo -e "\n================================================================"


# --- 2. BUSCAR O PLANETA CRIADO (GET by ID) ---
echo -e "\n--- 2. Enviando GET para /planets/$PLANET_ID para buscar 'Terra' ---\n"

curl -s "$BASE_URL/planets/$PLANET_ID"

echo -e "\n================================================================"


# --- 3. ATUALIZAR O PLANETA (PUT) ---
echo -e "\n--- 3. Enviando PUT para /planets/$PLANET_ID para atualizar para 'Marte' ---\n"
PAYLOAD_UPDATE='{
  "planetInsertName": "Marte",
  "planetInsertWeather": "Arido",
  "planetInsertSentinels": "Alto",
  "planetInsertFlora": "Inexistente",
  "planetInsertFauna": "Inexistente",
  "planetInsertResources": "Ferro, Urânio",
  "planetInsertNotes": "Colonizado",
  "planetInsertSolarSystemId": '$SOLAR_SYSTEM_ID'
}'

curl -s -X PUT \
  -H "Content-Type: application/json" \
  -d "$PAYLOAD_UPDATE" \
  "$BASE_URL/planets/$PLANET_ID"

echo -e "\n================================================================"


# --- 4. VERIFICAR A ATUALIZAÇÃO (GET by ID) ---
echo -e "\n--- 4. Enviando GET para /planets/$PLANET_ID para verificar a atualização ---\n"

curl -s "$BASE_URL/planets/$PLANET_ID"

echo -e "\n================================================================"


# --- 5. LISTAR TODOS OS PLANETAS (GET All) ---
echo -e "\n--- 5. Enviando GET para /planets para listar todos ---\n"

curl -s "$BASE_URL/planets"

echo -e "\n================================================================"


# --- 6. DELETAR O PLANETA (DELETE) ---
echo -e "\n--- 6. Enviando DELETE para /planets/$PLANET_ID para remover o planeta ---\n"

curl -s -X DELETE "$BASE_URL/planets/$PLANET_ID"

echo -e "\n================================================================"


# --- 7. VERIFICAR A EXCLUSÃO (GET by ID) ---
echo -e "\n--- 7. Enviando GET para /planets/$PLANET_ID para confirmar que foi deletado ---\n"

curl -s "$BASE_URL/planets/$PLANET_ID"

echo -e "\n\nDemonstração para Planetas concluída."