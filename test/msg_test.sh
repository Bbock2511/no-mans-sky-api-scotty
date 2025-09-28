#!/bin/bash

# --- CONFIGURAÇÃO ---
BASE_URL="http://localhost:3000"

echo "Executando demonstração de requisições para a API de Mensagens..."
echo "Certifique-se de que o servidor Scotty está rodando em $BASE_URL"
echo "================================================================"

# Assumimos que o primeiro item criado no banco de dados limpo terá o ID 1.
MSG_ID=1

# --- 1. CRIAR UMA NOVA MENSAGEM (POST) ---
echo -e "\n--- 1. Enviando POST para /msgs para criar uma mensagem ---\n"
PAYLOAD_CREATE='{"inputText": "Olá, No Man'\''s Sky!"}'

curl -s -X POST \
  -H "Content-Type: application/json" \
  -d "$PAYLOAD_CREATE" \
  "$BASE_URL/msgs"

echo -e "\n================================================================"


# --- 2. BUSCAR A MENSAGEM CRIADA (GET by ID) ---
echo -e "\n--- 2. Enviando GET para /msgs/$MSG_ID para buscar a mensagem ---\n"

curl -s "$BASE_URL/msgs/$MSG_ID"

echo -e "\n================================================================"


# --- 3. ATUALIZAR A MENSAGEM (PUT) ---
echo -e "\n--- 3. Enviando PUT para /msgs/$MSG_ID para atualizar o texto ---\n"
PAYLOAD_UPDATE='{"inputText": "Olá, Haskell e Scotty!"}'

curl -s -X PUT \
  -H "Content-Type: application/json" \
  -d "$PAYLOAD_UPDATE" \
  "$BASE_URL/msgs/$MSG_ID"

echo -e "\n================================================================"


# --- 4. VERIFICAR A ATUALIZAÇÃO (GET by ID) ---
echo -e "\n--- 4. Enviando GET para /msgs/$MSG_ID para verificar a atualização ---\n"

curl -s "$BASE_URL/msgs/$MSG_ID"

echo -e "\n================================================================"


# --- 5. LISTAR TODAS AS MENSAGENS (GET All) ---
echo -e "\n--- 5. Enviando GET para /msgs para listar todas as mensagens ---\n"

curl -s "$BASE_URL/msgs"

echo -e "\n================================================================"


# --- 6. DELETAR A MENSAGEM (DELETE) ---
echo -e "\n--- 6. Enviando DELETE para /msgs/$MSG_ID para remover a mensagem ---\n"

curl -s -X DELETE "$BASE_URL/msgs/$MSG_ID"

echo -e "\n================================================================"


# --- 7. VERIFICAR A EXCLUSÃO (GET by ID) ---
echo -e "\n--- 7. Enviando GET para /msgs/$MSG_ID para confirmar que foi deletada ---\n"

curl -s "$BASE_URL/msgs/$MSG_ID"

echo -e "\n\nDemonstração para Mensagens concluída."