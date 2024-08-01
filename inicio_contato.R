# UI ----

inicio_contato_ui <- function(id) {
  
  tagList(
    fluidRow(
      column(12, 
             h3(strong('Contato')),
      )
    ),
    fluidRow(
      column(12,
             div(
               style = "padding: 20px; text-align: left;",
               tags$p("Críticas, correções, sugestões ou interesse em participar do projeto devem ser enviadas para:"),
               tags$p(tags$b("garuestatistica@unifesp.br")),
               br(),
               tags$p('Garu Estatística, 2024. Versão 1.0.8'),
               tags$p('Última atualização: 16/07/2024')
             )
      )
    )
  )
}

# server ----

inicio_contato_server <- function(input, output, session) {
  
}