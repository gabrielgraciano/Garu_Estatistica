inicio <-
  #Podemos usar a imagem do Garu 2.0 que a Alê criou também
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
                              HTML("<p><strong>Contato: </strong><a> garuestatistica@gmail.com </a></p>"),
                              helpText("GARU Estatística, 2024. Versão 1.0.8"),
                              helpText("Última atualização: 29/04/2024")
                            )
                            
            )
            )
          )
  )



              
              




              