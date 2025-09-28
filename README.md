## Identificação
- **Nome:** Bruno H. Böck  
- **Curso:** Sistemas de Informação  

---

## Tema / Objetivo
*"No Man's Sky"* é um aclamado jogo de exploração espacial que gera um universo com mais de 18 quintilhões de planetas, cada um com seus próprios sistemas solares, flora, fauna e recursos.  

O objetivo deste trabalho é desenvolver uma **API dedicada a servir como banco de dados pessoal para descobertas dentro do jogo**.  
A API permite registrar e gerenciar informações estruturadas sobre:
- **Galáxias**: Nome, tipo e anotações pessoais.  
- **Sistemas Solares**: Nome, coordenadas, raça dominante, economia, nível de conflito e a galáxia a que pertencem.  
- **Planetas**: Nome, tipo, clima, sentinelas, flora, fauna, recursos principais e anotações personalizadas.
- **Mensagens**: Permite salvar mensagens importantes
Essa API supre a limitação do diário de descobertas interno do jogo, oferecendo uma forma robusta e pessoal de organizar informações.

---

## Processo de Desenvolvimento
Durante o desenvolvimento, utilizei **Haskell** com **Scotty** e **SQLite**.
Utilizei da famosa arquitetura de back-end MVC (**Model, View, Controller**) para desenvolvimento e organização modular do projeto, como desenvolvedor experiente, o entendimento dos conceitos eu já possuía, então minha maior dificuldade foi com a síntaxe e conceitos da linguagem `haskell`, até entender algumas coisas, recebi erro http 500.
Também utilizei de ferramentas de geração de código para me auxiliarem, tanto no desenvolvimento, quanto na compreensão de conceitos importantes no desenvolvimento com a linguagem e bibliotecas utilizadas, além da geração de arquivos como o `watcher` e utilização do `stack`.
A intenção é de ampliar o projeto com o passar do tempo para me auxiliar na exploração dos mais de 18 quintilhões de planetas enormes e diversos do jogo.
Gostaria de agradecer a oportunidade de conhecer novas tecnologias, mesmo com os momentos de estresse, ver que está dando tudo certo, o trabalho me proporcionou um sentimento de conquista muito grande.

### Principais desafios:
- Configuração do projeto no *Codespaces*.
- Problemas com **módulos não encontrados** (imports não automáticos).  
- Síntaxe e funcionalidades do haskell e scotty.

### Tentativas de solução:
- Ajustes no `package.yaml` para incluir dependências de forma eficiente.  
- Utilização das ferramentas Insomnia e Postman para validação das rotas.
- Associar com conceitos de outras linguagens mais conhecidas.
- Leitura da documentação e utilização de LLMs.
- Estruturação em camadas (**Model, View, Controller**) para maior organização.  

Esses erros e correções ajudaram a consolidar o aprendizado sobre **tipagem forte em Haskell**, **mapeamento entre JSON e SQL**, e **boas práticas de modularização**.

---

## Orientações para Execução

### Instalação das dependências
Instala dependências

```bash
stack setup
```

Build do projeto

```bash
stack build
```

Executa o servidor sem "escutar" modificações

```bash
stack exec scotty-api-exe
```

Adiciona permissão necessária

```bash
chmod +x watch.sh
```

Executa o servidor "escutando" alterações nas pastas `src` e `app`

```bash
./watch.sh
```

ou use o seguinte comando para não precisar adicionar permissão `+x`

```bash
bash watch.sh
```

Executando testes das funções dos **Models**

```bash
stack test
```

Para testar os **Controllers** de forma automatizada

```bash
cd test/
```

Então

```bash
bash <nome_do_sh>
```

OBS: Para executar cada teste, use
```bash
bash reset_db.sh
```
e reinicie o servidor.

Posteriormente pretendo adicionar um arquivo de configuração para Postman e Insomnia, facilitando o teste personalizado e individual de cada rota.

Se algo der errado com o resolver (`stack.yaml`), rode `stack setup` para deixar o ambiente consistente.