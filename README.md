<p align="center">
  <img src="img/logo_faroljus.png" width="250" alt="Farol Jus">
</p>

<p align="center">
  <b>Sistema de análise de alertas e conflitos do Quadro de Pessoal e Auxiliar (MPM/CNJ)</b>
</p>

<p align="center">
  <a href="LICENSE"><img src="https://img.shields.io/badge/Licen%C3%A7a-MIT-green.svg" alt="Licença MIT"></a>
  <a href="https://www.r-project.org/"><img src="https://img.shields.io/badge/R-4.x-blue.svg" alt="R"></a>
  <a href="https://shiny.posit.co/"><img src="https://img.shields.io/badge/Shiny-Web%20App-lightblue.svg" alt="Shiny"></a>
  <img src="https://img.shields.io/badge/Status-Em%20desenvolvimento-yellow.svg" alt="Status">
  <a href="https://github.com/KlebersonTJSE/FarolJus"><img src="https://img.shields.io/github/last-commit/KlebersonTJSE/FarolJus" alt="Último commit"></a>
</p>

---

## 📖 Sobre o projeto

O **Farol Jus** é uma aplicação desenvolvida para apoiar órgãos do Poder Judiciário na análise dos alertas e conflitos apontados pelo **Módulo de Produtividade Mensal (MPM)** do Conselho Nacional de Justiça (CNJ) no Quadro de Pessoal e Auxiliar (servidores e auxiliares).

A ferramenta consolida os arquivos de alertas em uma única visão, permitindo identificar rapidamente os registros com problema, priorizar correções e acompanhar a conformidade das informações encaminhadas ao CNJ.

O nome **Farol Jus** representa a missão da ferramenta: iluminar inconsistências e orientar os tribunais na melhoria contínua da qualidade dos dados institucionais.

---

## 🔐 Acesso ao sistema

Há duas formas de entrar:

| Método | Como funciona | O que libera |
|---|---|---|
| **Login Corporativo (AD)** | Escolha da empresa, usuário e senha de domínio (Active Directory/LDAP da empresa escolhida). | Alertas, **Administração** e **Trocar empresa**. |
| **Código Authenticator (TOTP)** | Usuário e código de 6 dígitos gerado no celular (Microsoft Authenticator, Google Authenticator etc.). | Alertas, sempre da empresa definida no cadastro TOTP. |

O acesso por Authenticator precisa ser cadastrado antes por um usuário AD, na janela **Administração**.

---

## 🧭 Navegação

A barra lateral esquerda reúne os atalhos da aplicação:

| Ícone | Função |
|---|---|
| ☰ **Menu** | Expande ou recolhe a barra, mostrando a descrição de cada ícone. |
| **Mostrar/ocultar** | Mostra ou esconde o quadro de informações do usuário no topo da tela. |
| **Administração** *(somente AD)* | Abre a janela de cadastro de acessos TOTP e de auditoria de login. |
| **Trocar empresa** *(somente AD)* | Muda a empresa com a qual a sessão está trabalhando, sem sair do sistema. |
| **Ver instruções** | Abre este documento. |
| **Sair** | Encerra a sessão. |

No topo da tela ficam as informações do usuário: nome, departamento, gestor, empresa, datas de criação e último acesso, e o método de acesso (no login TOTP: nome, login, empresa e método).

---

## ⚠️ Alertas: Quadro de Pessoal e Auxiliar

### Gerenciar arquivos

O botão **Gerenciar Arquivos** abre uma janela para enviar um ou mais arquivos CSV de alertas, ou apagar os arquivos já existentes. Os arquivos ficam separados por empresa, em `data/<EMPRESA>/alertas_csv`, e são consolidados automaticamente. O botão **Atualizar Dados** relê a pasta.

### Filtros

- **Alerta** e **Conflito**: mostram só os registros com o tipo escolhido (escolher um volta o outro para "Todos");
- **Cargo**: filtra pela descrição do cargo;
- **CPF** e **Nome**: busca por parte do texto;
- **Limpar Filtros**: volta todos os filtros ao padrão.

### Abas

- **Tabela**: um registro por linha, com os alertas e os conflitos de cada registro reunidos em uma única coluna. As colunas **Situação Profissional Atual** e **Cargo** aparecem pela descrição, e não pelo código do arquivo;
- **Gráfico**: quantidade de registros por tipo de alerta/conflito e por cargo;
- **Gerar arquivo CSV**: exporta exatamente o que está na tabela, respeitando os filtros e a busca da própria tabela.

Em **Magistrados → Base de Dados → Gráfico**, clicar numa barra abre uma janela com o resumo (registros e magistrados distintos) e os registros daquele grupo, respeitando os filtros da aba. Isso vale para qualquer opção de **Agrupar por**:

- **Cargo**: CPF, Nome, Órgão de lotação do magistrado(a), Cargo, Situação Profissional, Data de Início e Data de Saída da Situação;
- **demais opções**: CPF, Nome, Cargo, Situação Profissional, Data de Início e Data de Saída da Situação. A coluna agrupada (Órgão, Forma de promoção, Sexo, Raça / Cor, Naturalidade ou Status) entra logo depois do Nome.

**Pessoal e Auxiliar → Gráfico** tem o mesmo comportamento, com o mesmo código (`servidor_grafico_agrupado()`). Na janela de Cargo, a coluna de órgão é a "Órgão de lotação do(a) servidor(a)".

### 📊 Tipo de gráfico automático (toda a aplicação)

O tipo de cada gráfico depende de quantos grupos ele mostra (depois do **Exibir**, quando houver). É sempre feito por `grafico_automatico_girafe()`/`grafico_automatico_gg()` e, no Painel, por `painel_grafico()`:

| Grupos | Tipo |
|---|---|
| até 6 | **pizza** (rosca), com o total no centro, o percentual nas fatias de 5% ou mais e a legenda com categoria, quantidade e percentual |
| 7 a 8 | **barras verticais**, com quantidade e percentual acima de cada barra |
| 9 ou mais | **barras horizontais**, com quantidade e percentual no fim de cada barra; a altura cresce com o número de grupos |

Isso vale para todos os gráficos de uma dimensão:
- abas Base de Dados e Envio de Magistrados, Pessoal e Auxiliar e Serventias;
- gráficos por tipo de alerta e por cargo/status das abas Alertas;
- gráficos de usuário, empresa e método da Administração;
- Sexo, Identidade de gênero, Raça/cor e Cargo no Painel.

Exceções:
- Município abrangido e Competência (Serventias) e Tipo de deficiência (Painel) nunca viram pizza. Neles uma serventia ou pessoa pode estar em mais de um grupo, e as fatias não somariam o todo.
- A pirâmide etária, os comparativos do Painel e o gráfico de sucesso x falha por período da Administração têm duas dimensões e mantêm o formato.

As **cores** são as mesmas em pizza e barras:
- Sexo e Raça/Cor iguais às do Painel;
- em Status, Ativo é verde e Inativo, grafite;
- os cargos mais comuns têm cor fixa (as mesmas no Painel);
- "(Não informado)" fica em cinza;
- as demais categorias seguem a paleta na ordem do maior para o menor grupo. Depois das 5 cores vêm tons mais claros e mais escuros delas; a partir da 16ª categoria, as cores se repetem.

O percentual é sobre o total de registros, mesmo quando o Exibir mostra só parte dos grupos. **Agrupar por** e **Exibir** ficam sempre lado a lado.

A opção **Mostrar → Apenas CPFs distintos** deixa uma linha por CPF, com a situação mais recente de cada magistrado. O botão **Gerar arquivo CSV** da janela exporta exatamente o que a tabela mostra: a opção escolhida, a busca e os filtros das colunas.

Todas as janelas têm um **X** no canto superior direito, que fecha como o botão Fechar/Cancelar.

Todos os gráficos usam a mesma paleta, definida em `R/utils.R` (`PALETA_GRAFICOS`): `#00B8D9`, `#005B96`, `#FF8C42`, `#4A4A4A` e `#4CAF50`. Quando há mais de cinco categorias, entram tons mais claros dessas mesmas cores.

---

## 🛡️ Administração *(somente login AD)*

Janela com o controle de acesso por Authenticator:

- **Cadastro**: gera (ou refaz) a chave TOTP de um usuário, associando-o a uma empresa. É exibido um QR Code para leitura no aplicativo;
- **Desativação**: revoga o acesso TOTP de um login;
- **Usuários cadastrados**: clique em uma linha para carregar os dados no formulário;
- **Auditoria - Tabela**: histórico de tentativas de login (AD e TOTP), com filtro por período do dia (Manhã, Tarde, Noite);
- **Auditoria - Gráfico**: acessos por usuário, empresa, método e período do dia.

> **Período da auditoria:** informe a **Data inicial** e a **Data final** (digitando no formato dd/mm/aaaa ou pelo calendário). Se uma ou as duas estiverem em branco, é considerado todo o histórico gravado no banco. O período vale para as duas abas de auditoria.

Ao fechar a janela, a chave recém-gerada e os campos do formulário são limpos.

---

## 🛠 Tecnologias utilizadas

| Finalidade | Pacotes / ferramentas |
|---|---|
| Aplicação web | `shiny`, `bslib`, `here` |
| Dados | `readr`, `dplyr`, `purrr`, `stringr`, `stringi` |
| Tabelas e gráficos | `DT`, `ggplot2`, `ggiraph` |
| Banco local (acessos e auditoria) | `DBI`, `RSQLite` |
| Banco IRIS (InterSystems) | `RJDBC`, `rJava`, driver JDBC do IRIS |
| Login AD | `reticulate` + Python com `ldap3` |
| Login TOTP | `digest`, `jsonlite`, `openssl` (chave aleatória segura), `qrcode` (opcional, para o QR Code) |

---

## 📂 Estrutura do projeto

```text
FarolJus/
├── app.R                  # UI, servidor, login e barra lateral
├── R/
│   ├── utils.R            # empresas (DISTRO_N) e utilitários
│   ├── auth.R             # login AD (LDAP via Python)
│   ├── auth_totp.R        # login TOTP, cadastro e auditoria
│   ├── database.R         # conexão com o IRIS (JDBC)
│   └── tabelas_referencia.R  # tabelas auxiliares do MPM no SQLite (carga e tradução de códigos)
├── referencias/           # arquivos padrão das tabelas auxiliares (versionados)
│   ├── tabelas_auxiliares_mpm.xlsx
│   ├── municipios.csv
│   ├── competencias.csv
│   └── tipos_classificacao_unidade.csv
├── modules/
│   ├── mod_pessoal_auxiliar.R  # Pessoal e Auxiliar (base, envio, alertas, inconsistências)
│   ├── mod_magistrados.R       # Magistrados (base, envio, alertas, inconsistências)
│   ├── mod_serventias.R        # Serventias (base, envio, alertas, inconsistências)
│   ├── mod_inconsistencias.R   # aba de inconsistências (comum aos módulos)
│   ├── mod_tabelas_auxiliares.R # Configurações → Tabelas Auxiliares (consulta e Gerenciar Arquivos)
│   ├── mod_painel.R       # Painel (indicadores de Magistrados e Servidores)
│   ├── mod_matricula.R    # matrícula por nome (consulta ao IRIS)
│   └── mod_totp_admin.R   # janela Administração
├── tests/
│   └── test_seguranca.R   # testes de segurança e desempenho
├── data/
│   ├── faroljus.db        # banco SQLite (criado automaticamente; não versionado)
│   ├── referencias/       # tabelas auxiliares enviadas pelo app (não versionadas)
│   └── <EMPRESA>/         # não versionados
│       ├── pessoal_auxiliar/   # base (pessoal_auxiliar.xlsx/.csv) e envio
│       ├── alertas_csv/        # alertas de Pessoal e Auxiliar do MPM
│       ├── serventias/     # serventias, serventias_envio e alertas_serventias (.xlsx/.csv)
│       └── magistrados/    # magistrados, magistrados_envio e alertas_magistrados (.xlsx/.csv)
├── img/
├── README.md
├── LICENSE
└── .Renviron              # configuração local (não versionar)
```

---

## ⚙️ Configuração (`.Renviron`)

| Variável | Descrição |
|---|---|
| `DISTRO_1`, `DISTRO_2`, ... | Empresas disponíveis (ex.: `TJSE`, `MPRO`), em sequência sem pular números. |
| `<EMPRESA>_LDAP_SERVER`, `_LDAP_PORT`, `_LDAP_DOMAIN`, `_LDAP_SEARCH_BASE` | Active Directory de cada empresa. |
| `<EMPRESA>_IRIS_URL`, `_IRIS_USER`, `_IRIS_PASSWORD` | Banco IRIS de cada empresa. |
| `IRIS_DRIVER_CLASS`, `IRIS_JAR_PATH` | Classe e arquivo `.jar` do driver JDBC do IRIS. |
| `JAVA_HOME` | Pasta do JDK/JRE usado pelo rJava. |
| `RETICULATE_PYTHON` | Caminho do Python com o pacote `ldap3`. |
| `PASTA_ALERTAS` | Pasta raiz dos arquivos de alertas (padrão: `data`). |
| `MAX_UPLOAD_MB` | Limite de envio de arquivos, em MB (padrão: `50`). |
| `TOTP_EMISSOR` | Nome exibido no aplicativo Authenticator (padrão: `Farol Jus`). |
| `ADMINS_AD` | Logins AD (separados por vírgula) com acesso à Administração e à troca de empresa. Vazio = qualquer login AD (comportamento anterior). |
| `LOGIN_MAX_FALHAS`, `LOGIN_JANELA_MIN` | Bloqueio por tentativas: após `LOGIN_MAX_FALHAS` falhas do mesmo login em `LOGIN_JANELA_MIN` minutos, o login fica bloqueado até a janela passar (padrão: `5` e `15`). |
| `LOGIN_MAX_FALHAS_SESSAO` | Tentativas inválidas aceitas por sessão do navegador (padrão: `10`). |
| `FUSO_AUDITORIA` | Fuso usado para exibir a auditoria de login, gravada em UTC (padrão: `America/Maceio`). |
| `SQLITE_WAL` | Modo WAL do SQLite (padrão: `TRUE`). Use `FALSE` se `data/` estiver em pasta de rede. |
| `CACHE_ARQUIVOS_MAX` | Quantos arquivos lidos ficam em cache na memória, compartilhados entre as sessões (padrão: `30`; `0` desliga). |

Há um modelo em `.Renviron.example`. O `.Renviron` contém senhas: não o inclua no repositório nem em pacotes de publicação.

### 📁 Pastas de Pessoal e Auxiliar (layout do FarolJus Lite)

Na primeira execução desta versão, se existir a pasta antiga `<EMPRESA>/pessoal/`, o app **copia** a base (`quadro_pessoal_auxiliar.*`) para `pessoal_auxiliar/pessoal_auxiliar.*` e os demais arquivos para `alertas_csv/`, e renomeia a pasta antiga para `pessoal_migrado_<data>`. Nada é apagado.

### 🗂️ Tabelas auxiliares do MPM (nomes no lugar de códigos)

Na inicialização, o app grava no `faroljus.db` 16 tabelas auxiliares (`codigo`, `descricao` e, quando houver, colunas extras):

| Origem | Tabelas |
|---|---|
| `tabelas_auxiliares_mpm.xlsx` (uma aba por tabela) | cargo, situação profissional e forma de promoção de magistrado; cargo e situação profissional de servidor; área de atuação; sexo; identidade de gênero; raça/cor; deficiência; regime de cotas; tipo de unidade; entrância |
| `municipios.csv`, `competencias.csv`, `tipos_classificacao_unidade.csv` | município, competência, classificação de unidade (o CSV específico tem prioridade sobre a aba equivalente da planilha) |

Com elas, Magistrados, Pessoal e Auxiliar, Serventias, Painel e Inconsistências mostram descrições (ex.: cargo 4 → Desembargador(a), situação 12 → Aposentado(a), município 366 → AREIA BRANCA). Códigos desconhecidos são mantidos como estão. O **Gerar Novo Envio** continua saindo com os códigos, como nos arquivos do MPM. A tabela de entrância é só de consulta: ela descreve a estrutura do tribunal e não corresponde à coluna Entrância das serventias.

Cada tabela só é recarregada quando o arquivo de origem muda (controle por MD5 em `tab_carga_referencia`); ajustes feitos diretamente no banco são preservados até lá.

**Configurações → Tabelas Auxiliares** mostra o conteúdo de cada tabela, onde ela é usada, o arquivo de origem e a data da carga, além de um resumo de todas e a exportação em CSV. Para administradores, **Gerenciar Arquivos** permite enviar a planilha do MPM e/ou os CSVs (o tipo é identificado pelo conteúdo), substituir ou apagar o arquivo enviado. Os arquivos enviados ficam em `<PASTA_ALERTAS>/referencias/`, valem para todas as empresas e têm prioridade sobre os padrões de `referencias/`; ao apagar, volta a valer o padrão. As tabelas são recarregadas na hora.

> Os códigos de magistrados foram alinhados às tabelas oficiais do MPM: situação 10 = Licenças ou Concessões e 12 = Aposentado(a); cargo 3 = Juiz(a) Substituto(a) de 2º grau e 4 = Desembargador(a). As regras de inconsistências, Painel e Novo Envio usam esses códigos.

### 🔒 Dados que não vão para o Git

O banco `data/faroljus.db` (chaves TOTP e auditoria de login) e as planilhas/CSVs de `data/` (CPF, nome e dados funcionais de pessoas reais — LGPD) **não são versionados** (ver `.gitignore`). As pastas continuam existindo localmente; o banco é criado automaticamente na primeira execução.

---

## 🚀 Instalação

### Clonar o repositório

```bash
git clone https://github.com/KlebersonTJSE/FarolJus.git
```

### Instalar as dependências do R

```r
install.packages(c(
  "shiny", "bslib", "here", "readr", "dplyr", "purrr", "stringr",
  "stringi", "DT", "ggplot2", "ggiraph", "DBI",
  "RSQLite", "RJDBC", "rJava", "reticulate", "digest", "jsonlite",
  "openssl", "readxl", "qrcode"
))
```

### Instalar a dependência do Python (login AD)

```bash
pip install ldap3
```

---

## ▶️ Execução

```r
shiny::runApp()
```

Para testar a conexão com o IRIS de uma empresa, no console:

```r
testar_iris("TJSE")
```

Testes de segurança e desempenho (não precisam de AD, IRIS nem do banco real):

```bash
Rscript tests/test_seguranca.R
```

---

## 📊 Governança e conformidade

O Farol Jus foi concebido para fortalecer a qualidade das informações encaminhadas ao Conselho Nacional de Justiça, contribuindo para:

- Governança de dados;
- Transparência institucional;
- Conformidade regulatória;
- Integridade das informações;
- Eficiência administrativa;
- Tomada de decisão baseada em dados.

---

## 👥 Público-alvo

Tribunais de Justiça, Tribunais Regionais e demais órgãos do Poder Judiciário, especialmente Secretarias de Gestão de Pessoas, unidades estatísticas, unidades de governança e equipes responsáveis pelas remessas ao CNJ.

---

## 👨‍💻 Desenvolvedores

**Edison Carvalho**<br>
Técnico Judiciário - Programação de Sistemas<br>
Tribunal de Justiça do Estado de Sergipe (TJSE)

**Kleberson Carlos Pinto**<br>
Técnico Judiciário - Programação de Sistemas<br>
Tribunal de Justiça do Estado de Sergipe (TJSE)<br>
🔗 [LinkedIn](https://www.linkedin.com/in/kleberson-pinto-91010a345/)

---

## 📜 Licença

O Farol Jus é disponibilizado sob a Licença MIT por entendermos que soluções voltadas ao aprimoramento da gestão pública devem incentivar a colaboração, a transparência e o compartilhamento de conhecimento entre as instituições. A licença permite que outros órgãos do Poder Judiciário utilizem, adaptem e aprimorem a ferramenta livremente, preservando o devido reconhecimento aos seus autores. Consulte o arquivo `LICENSE`.

---

## 🤝 Contribuição

Sugestões, correções e melhorias podem ser registradas na seção **Issues** do GitHub.

---

> **Farol Jus**: orientação segura para a conformidade no MPM.

<p align="center">
  <sub>Desenvolvido no Tribunal de Justiça do Estado de Sergipe (TJSE).</sub>
</p>