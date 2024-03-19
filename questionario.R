library(shinyalert)
questionario <-
  tabItem(tabName = 'questionario',
          fluidPage(
            titlePanel('Questionário'),
            sidebarLayout(
              sidebarPanel(
                uiOutput('pergunta'),
                uiOutput('opcoes_resposta'),
                useShinyjs(),
                actionButton('anterior', 'Questão anterior'),
                actionButton('confirmar_resposta','Verificar'), #trocar para apenas verificar, sem pular
                actionButton('proximo', 'Próxima Questão'),
                textOutput('resultado')
              ),
              mainPanel(
                gt_output('score')

              )
            )
          )
  )

#                gt_output('score')