# =====================================================
# R/tabelas_auxiliares.R
# -----------------------------------------------------
# Tabelas de códigos do MPM/CNJ (Situação profissional atual e Cargo, de
# servidores e de magistrados) guardadas no SQLite da aplicação.
#
# Os módulos (vindos do FarolJus Lite) traduzem os códigos usando as
# constantes SITUACAO_PROFISSIONAL_SERVIDOR, CARGO_SERVIDOR,
# SITUACAO_PROFISSIONAL_MAGISTRADO e CARGO_MAGISTRADO. No FarolJus, as
# nomenclaturas continuam editáveis no banco (como antes):
#
#   1) na inicialização, as tabelas são criadas e os códigos que faltam
#      são inseridos a partir dessas constantes (INSERT OR IGNORE — nunca
#      sobrescreve um ajuste feito no banco), tudo numa única transação;
#   2) em seguida, o conteúdo das tabelas substitui as constantes, então
#      um ajuste feito direto no banco vale para todas as telas e o
#      Painel.
#
# Um ajuste no banco passa a valer na próxima inicialização do app.
# Qualquer erro aqui só gera um aviso: os módulos seguem com as
# nomenclaturas padrão do manual.
#
#   UPDATE tab_cargo_magistrado SET nomenclatura = '...' WHERE codigo = 1;
# =====================================================

TABELAS_AUXILIARES <- c(
    tab_situacao_profissional_servidor   = "SITUACAO_PROFISSIONAL_SERVIDOR",
    tab_cargo_servidor                   = "CARGO_SERVIDOR",
    tab_situacao_profissional_magistrado = "SITUACAO_PROFISSIONAL_MAGISTRADO",
    tab_cargo_magistrado                 = "CARGO_MAGISTRADO"
)

carregar_tabelas_auxiliares <- function(con, ambiente = globalenv()) {

    tryCatch({

        DBI::dbWithTransaction(con, {

            for (tabela in names(TABELAS_AUXILIARES)) {

                semente <- get(TABELAS_AUXILIARES[[tabela]], envir = ambiente)

                DBI::dbExecute(con, sprintf(
                    "CREATE TABLE IF NOT EXISTS %s (codigo INTEGER PRIMARY KEY, nomenclatura TEXT NOT NULL)",
                    tabela
                ))

                DBI::dbExecute(
                    con,
                    sprintf("INSERT OR IGNORE INTO %s (codigo, nomenclatura) VALUES (?, ?)", tabela),
                    params = list(as.integer(names(semente)), unname(semente))
                )

            }

        })

        for (tabela in names(TABELAS_AUXILIARES)) {

            ref <- DBI::dbGetQuery(
                con,
                sprintf("SELECT codigo, nomenclatura FROM %s ORDER BY codigo", tabela)
            )

            if (nrow(ref) > 0) {
                assign(
                    TABELAS_AUXILIARES[[tabela]],
                    setNames(as.character(ref$nomenclatura), as.character(ref$codigo)),
                    envir = ambiente
                )
            }

        }

        invisible(TRUE)

    }, error = function(e) {

        warning(
            "Não foi possível preparar as tabelas auxiliares (códigos do MPM): ",
            conditionMessage(e), ". Usando as nomenclaturas padrão."
        )

        invisible(FALSE)

    })

}
