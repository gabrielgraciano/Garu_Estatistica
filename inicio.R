inicio <-
  tabItem(tabName = 'inicio',
          fluidPage(
            fluidRow(
              HTML('<center><img src="images/garu_3.png"></center>')),
            fluidRow(column(12,
                            
                            wellPanel(
                              h3(strong('Sobre o Garu')),
                              HTML('Inserir texto explicando o projeto.')
                            ),
                            wellPanel(
                              HTML("<p><strong>Alunos participantes:</strong> João Henrique de Araujo Morais e Gabriel Graciano Dias</p>"),
                              HTML("<p><strong>Orientadoras:</strong> Profa. Dra. Camila Bertini Martins e Téc. Dra. Alessandra A. S. Menezes</p>"),
                              HTML("<p><strong>Contato: </strong><a> joao.morais@unifesp.br, gabriel.graciano@unifesp.br </a></p>"),
                              helpText("GARU Estatística, 2024. Versão 1.0.5"),
                              helpText("Última atualização: 17/03/2024")
                            ),
                            wellPanel(
                              h3(strong("Fonte de Dados")),
                              fluidRow(
                                column(6, 
                                       selectInput("dataSource", "Fonte de Dados", choices = c("Food choices (nativo)" = "nativo"))
                                )
                                
                              ),
                              downloadButton("botaoBaixarDados", "Download dos dados")
                            )
            )
            )
          )
  )



              
              




              