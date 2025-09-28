#!/bin/bash

# --- Configuração ---
DB_FILE="db.sqlite"

echo "Tentando resetar o banco de dados: $DB_FILE"
echo "-----------------------------------------"

# Verifica se o arquivo do banco de dados existe
if [ -f "$DB_FILE" ]; then
    echo "Arquivo '$DB_FILE' encontrado. Removendo..."
    
    # Remove o arquivo do banco de dados
    rm "$DB_FILE"
    
    echo "Banco de dados resetado com sucesso."
    echo "O arquivo será recriado na próxima vez que você iniciar a aplicação."
else
    echo "Arquivo '$DB_FILE' não encontrado. Nada a fazer."
fi

echo "-----------------------------------------"
echo "Script concluído."