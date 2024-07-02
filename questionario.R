library(shinyalert)
questionario <-
  tabItem(tabName = 'questionario',
          fluidPage(
            titlePanel('Questionário'),
            sidebarLayout(
              column(8,
              wellPanel(
                uiOutput('pergunta'),
                br(),
                uiOutput('opcoes_resposta'),
                useShinyjs(),
                actionButton('anterior', 'Questão anterior'),
                actionButton('confirmar_resposta','Verificar'), #trocar para apenas verificar, sem pular
                actionButton('proximo', 'Próxima Questão'),
                textOutput('resultado')
              )
              ),
              column(4,
              wellPanel(
                gt_output('score')
              )

              )
            )
          )
  )

#                gt_output('score')