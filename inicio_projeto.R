# UI ----

inicio_projeto_ui <- function(id) {
  
  tagList(
    
    tags$head(
      tags$style(HTML("
        .project-text {
          font-size: 14px;
          line-height: 1.6;
          align = 'left'
        }
      "))
    ),
    
    fluidRow(
      column(12, 
             div(
               HTML('<img src="images/garu_3.png" style="max-width:100%; height:auto;">'),
               align = 'center'
               )
             )
    ),
    
    fluidRow(
      column(12, 
             div(class = "project-text",
                 tags$br(''),
                 tags$p("Este é o projeto Garu Estatística."),
                 tags$p("Aqui você pode encontrar várias funcionalidades, 
                     como os principais 
                         conceitos da estatística descritiva e
                         inferencial, além de exercícios."
                 ),
                 tags$p(
                   "Garu Estatística é um aplicativo gratuito, 
                     desenvolvido em R/Shiny por alunos de graduação e pós-graduação
                     da Unifesp."
                 ),
                 tags$p(
                   "O app não é autossuficiente no ensino de estatística.
                     A equipe se esforça para manter a correção e
                     integridade das informações, mas reconhecemos que
                     podem ocorrer falhas. Estamos sempre abertos a 
                     críticas construtivas."
                 ),
                 tags$p(
                   "A equipe está aberta a sugestões da comunidade para
                     a abordagem de problemas sugeridos pelos usuários,
                     dentro dos limites da equipe." 
                 ),
                 tags$p(
                   "Interessados em participar do projeto, incluindo alunos
                       de outras instituições, podem entrar em contato com a 
                       coordenação por meio do endereço de e-mail
                       garuestatistica@unifesp.br."
                 ),
             )
      )
    )
  )
}


# server ----

inicio_projeto_server <- function(input, output, session) {
  
}