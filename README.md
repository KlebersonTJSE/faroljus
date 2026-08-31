---
title: ""
output: github_document
---

<img src="img/logo_faroljus.png" width="250r">

### Sistema de análise de erros, alertas e inconsistências do MPM/CNJ.


[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSEg.shields.io/badge/R-%.3-blue](https://www.r-project.org/)

[![Shiny](https://img.shields.io/badge/Shiny-Web%20App-lightblue)](https://shiny.[![Status](https://img.shields.io/badge/Status-Emvimento-yellow]()

[![Last Commit](https://img.shields.io/github/last-commit/KlebersonTJSE/FarolJus)](https://github.com/KlebersonTJSE/Fp align="center">

---

## 📖 Sobre o Projeto

<div style="text-align: justify;">
O **FarolJus** é uma aplicação desenvolvida para apoiar órgãos do Poder Judiciário na análise de relatórios de erros, alertas e inconsistências gerados pelo **Modelo de Pontuação da Magistratura (MPM)** do Conselho Nacional de Justiça (CNJ).

Seu propósito é fornecer uma visão consolidada dos apontamentos do MPM, permitindo a rápida identificação de problemas, priorização de correções e monitoramento da conformidade das informações encaminhadas ao CNJ.

O nome **FarolJus** representa a missão da ferramenta: iluminar inconsistências e orientar os tribunais na melhoria contínua da qualidade dos dados institucionais.
</div>
---

## 🎯 Objetivos

- Identificar erros e alertas presentes nos relatórios do MPM;
- Facilitar a correção de inconsistências;
- Apoiar as unidades responsáveis pelo envio de dados ao CNJ;
- Produzir indicadores de qualidade da informação;
- Auxiliar ações de governança de dados;
- Promover a conformidade das informações institucionais.

---

## ✨ Funcionalidades

- 📥 Importação de relatórios do MPM;
- 🔍 Consolidação automática de erros e alertas;
- 📊 Dashboards gerenciais;
- 📈 Indicadores de qualidade dos dados;
- 📑 Exportação de relatórios;
- 🏷️ Classificação de inconsistências;
- 📚 Histórico de correções;
- 🚨 Monitoramento de alertas críticos;
- 🔎 Pesquisa e filtros avançados.

---

## 👥 Público-Alvo

- Tribunais de Justiça;
- Tribunais Regionais;
- Órgãos do Poder Judiciário;
- Secretarias de Gestão de Pessoas;
- Unidades Estatísticas;
- Unidades de Governança;
- Equipes responsáveis pelas remessas ao CNJ.

---

## ✅ Benefícios

- Maior consistência dos dados enviados ao CNJ;
- Redução do retrabalho;
- Identificação rápida de inconsistências;
- Melhoria dos indicadores institucionais;
- Apoio à governança e transparência;
- Maior segurança na prestação das informações.

---

## 🛠 Tecnologias Utilizadas

```r
R
Shiny
shinydashboard
dplyr
DT
plotly
ggplot2
stringr
readr
lubridate
```

---

## 📂 Estrutura do Projeto

```text
FarolJus/
│
├── app/
│   ├── ui.R
│   ├── server.R
│   └── global.R
│
├── data/
├── docs/
├── img/
│   └── logo_faroljus.png
│
├── relatorios/
├── scripts/
├── tests/
│
├── README.Rmd
├── README.md
├── LICENSE
└── .gitignore
```

---

## 🚀 Instalação

### Clonar o repositório

```bash
git clone https://github.com/SEU-USUARIO/FarolJus.git
```

### Instalar dependências

```r
install.packages(
  c(
    "shiny",
    "shinydashboard",
    "DT",
    "dplyr",
    "plotly",
    "ggplot2",
    "stringr",
    "readr",
    "lubridate"
  )
)
```

---

## ▶️ Execução

```r
shiny::runApp()
```

---

## 📊 Governança e Conformidade

O FarolJus foi concebido para fortalecer a qualidade das informações encaminhadas ao Conselho Nacional de Justiça, contribuindo para:

- Governança de Dados;
- Transparência Institucional;
- Conformidade Regulatória;
- Integridade das Informações;
- Eficiência Administrativa;
- Tomada de Decisão Baseada em Dados.

---

## 👨‍💻 Desenvolvedores

### Edison Carvalho

**Técnico Judiciário - Programação de Sistemas**  
Tribunal de Justiça do Estado de Sergipe (TJSE)

### Kleberson Carlos Pinto

**Técnico Judiciário - Programação de Sistemas**  
Tribunal de Justiça do Estado de Sergipe (TJSE)
🔗 LinkedIn: [Kleberson Carlos Pinto](https://www.linkedin.com/in/kleberson-pinto-91010a345/)

---

## 🏛 Instituição

**Tribunal de Justiça do Estado de Sergipe - TJSE**

---

## 📜 Licença

<div style="text-align: justify;">
O FarolJus é disponibilizado sob a Licença MIT por entendermos que soluções voltadas ao aprimoramento da gestão pública devem incentivar a colaboração, a transparência e o compartilhamento de conhecimento entre as instituições. A adoção dessa licença permite que outros órgãos do Poder Judiciário utilizem, adaptem e aprimorem a ferramenta livremente, preservando o devido reconhecimento aos seus autores e contribuindo para a evolução contínua da qualidade das informações prestadas ao Conselho Nacional de Justiça (CNJ).

Consulte o arquivo `LICENSE`.
</div>
---

## 🤝 Contribuição

Sugestões, correções e melhorias podem ser registradas por meio da seção **Issues** do GitHub.

---

## 📌 Slogan

> **FarolJus**
>
> *Orientação segura para a conformidade no MPM.*

---

<p align="center">
Desenvolvido no Tribunal de Justiça do Estado de Sergipe (TJSE).
</p>
