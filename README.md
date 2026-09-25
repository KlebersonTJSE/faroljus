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
| Tabelas e gráficos | `DT`, `echarts4r`, `ggplot2`, `ggiraph` |
| Banco local (acessos e auditoria) | `DBI`, `RSQLite` |
| Banco IRIS (InterSystems) | `RJDBC`, `rJava`, driver JDBC do IRIS |
| Login AD | `reticulate` + Python com `ldap3` |
| Login TOTP | `digest`, `jsonlite`, `qrcode` (opcional, para o QR Code) |

---

## 📂 Estrutura do projeto

```text
FarolJus/
├── app.R                  # UI, servidor, login e barra lateral
├── R/
│   ├── utils.R            # empresas (DISTRO_N) e utilitários
│   ├── auth.R             # login AD (LDAP via Python)
│   ├── auth_totp.R        # login TOTP, cadastro e auditoria
│   └── database.R         # conexão com o IRIS (JDBC)
├── modules/
│   ├── mod_alertas.R      # Alertas (tabela, gráficos, CSV, arquivos)
│   └── mod_totp_admin.R   # janela Administração
├── data/
│   ├── radarsocial.db     # banco SQLite (criado automaticamente)
│   └── <EMPRESA>/alertas_csv/
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

O `.Renviron` contém senhas: não o inclua no repositório nem em pacotes de publicação.

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
  "stringi", "DT", "echarts4r", "ggplot2", "ggiraph", "DBI",
  "RSQLite", "RJDBC", "rJava", "reticulate", "digest", "jsonlite",
  "qrcode"
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