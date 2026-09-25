# modules/mod_usuario.R

mod_usuario_ui <- function(id){

  ns <- NS(id)

  fluidPage(

    fluidRow(

      column(
        3,

        uiOutput(ns("foto"))

      ),

      column(
        9,

        uiOutput(ns("dados"))
      )

    ),

    hr(),

    DTOutput(ns("atributos"))

  )

}

mod_usuario_server <- function(
  id,
  dados_usuario,
  foto_usuario
){

  moduleServer(id,function(
    input,
    output,
    session
  ){

    output$foto <- renderUI({

      req(foto_usuario())

      tags$img(
        src = foto_usuario(),
        class = "foto"
      )

    })

    output$dados <- renderUI({

      req(dados_usuario())

      dados <- dados_usuario()

      tagList(

        h3(
          obter_campo(
            dados,
            "displayName"
          )
        ),

        p(
          strong("Email: "),
          obter_campo(dados,"mail")
        ),

        p(
          strong("Departamento: "),
          obter_campo(dados,"department")
        ),

        p(
          strong("Cargo: "),
          obter_campo(dados,"title")
        ),

        p(
          strong("Gestor: "),
          extrair_manager(
            dados$manager
          )
        )

      )

    })

    output$atributos <- renderDT({

      req(dados_usuario())

      dados <- dados_usuario()

      tabela <- data.frame(

        Atributo = names(dados),

        Valor = sapply(
          dados,
          function(x)
            paste(
              as.character(x),
              collapse=";"
            )
        )

      )

      datatable(
        tabela,
        options=list(
          pageLength=15,
          scrollX=TRUE
        )
      )

    })

  })

}