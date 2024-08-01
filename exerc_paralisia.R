# Questões ----
questoes_paralisia <- list(
  "1" = "<left>
  <p>Utilize as caixas de seleção para agrupar as variáveis 
  em <b>qualitativas</b> e <b>quantitativas</b>.<p>
  </left>",
  "2" = "<left>
  <p>Utilize a caixa de seleção “Medidas-resumo” para selecionar 
  <b>todas</b> as medidas-resumo
  adequadas aos <b>tempos de deglutição de alimentos (líquidos, pastosos e
  sólidos)</b>.<p>
  </left>",
  "3" = "<left>
  <p>Utilize a caixa de seleção “Medidas-resumo”
  para selecionar <b>todas</b>  as medidas-resumo adequadas ao <b>distúrbio 
  de comunicação</b>.<p>
  </left>",
  "4" = "<left>
  <p>Utilize as caixas de seleção para construir
  um gráfico adequado para visualizar o <b>grau de disfunção motora oral
  (DMO)</b>.<p>
  </left>",
  "5" = "<left>
  <p>Utilize as caixas de seleção para construir um gráfico
  adequado para visualizar os <b>distúrbios de comunicação</b>.<p>
  </left>",
  "6" = "<left>
  <p>Utilize as caixas de seleção para construir um gráfico 
  adequado para visualizar o <b>tempo de deglutição
  de alimentos líquidos</b>.<p>
  </left>",
  "7" = "<left>
  <p>As Tabelas 1 e 2 abaixo apresentam, respectivamente, a distribuição
  observada e a distribuição esperada (sob hipótese de não associação) 
  da perda auditiva pelos grupos de crianças com paralisia cerebral e as 
  sem acometimentos neurológicos</b>.<p>
  </left>",
  "8" = "<left>
  <p>As Tabelas 1 e 2 abaixo apresentam, respectivamente, a distribuição
  observada e a distribuição esperada (sob hipótese de não associação) dos 
  distúrbios de comunicação pelos grupos de crianças SAN e PC.</b><p>
  </left>",
  "9" = "<left>
  <p>Utilize as caixas de seleção para construir 
  um gráfico para visualizar a relação do <b>tempo de deglutição de alimentos
  líquidos</b> com os <b> grupos de crianças PC e SAN.</b><p>
  </left>",
  "10" = "<left>
  <p>Utilize as caixas de seleção para construir 
  um gráfico para visualizar a relação do <b>tempo de deglutição de alimentos 
  líquidos </b> com o <b>tempo de deglutição de alimentos sólidos.</b><p>
  </left>"
)

# UI ---- 
exerc_paralisia_ui <- function(id) {
  
  ns <- NS(id)
  tabItem(tabName = 'exerc_paralisia',
          
          fluidPage(
            
            h3(strong('Exercícios Práticos - Banco de Dados Paralisia Cerebral')),
            
            fluidRow(
              column(10,
                     wellPanel(
                       HTML("<p>Segundo as 
                <a href= 'https://bvsms.saude.gov.br/bvs/publicacoes/diretrizes_atencao_paralisia_cerebral.pdf' 
                target='_blank' > Diretrizes de Atenção à
              Pessoa com Paralisia Cerebral do Ministério da Saúde (2014) </a>,
              a paralisia cerebral (PC) descreve um grupo de desordens da 
              desenvolução do movimento e postura atribuído
              à distúrbio não progressivo durante o desenvolvimento do 
              cérebro fetal ou infantil.
             <br>
             <p>Estima-se que nos países em desenvolvimento, 7 a cada 1000 
             nascidos vivos
             sejam acometidos por PC, em diferentes graus de comprometimento
             dos movimentos e postura.
             <br>
             <p>Algumas informações fornecidas nas diretrizes 
             do Ministério da Saúde e achados de <a href= 'https://www.scielo.br/j/rboto/a/hvQv9C6JY9h7MbrrSY98WpB/abstract/?lang=pt' target='_blank'> 
             Aurélio et al. (2002)</a> na comparação do padrão de deglutição 
             de alimentos entre crianças com PC e crianças sem acometimentos
             neurológicos
             (SAN), encontram-se simulados no banco de dados que será utilizado
             nos exercícios abaixo.</p><br>
             <p> Existem nove variáveis nesse banco de dados, sendo elas:</p>
             <br>
             <p><b>Sexo:</b> menino ou menina
             <p><b>Idade:</b> idade em anos completos
             <p><b>Grupo:</b> grupo das crianças por condição de saúde (SAN ou PC)
             <p><b>Perda auditiva:</b> existência ou não de perda auditiva (Não ou Sim)
             <p><b>Distúrbio de Comunicação:</b> existência ou não de distúrbio de
             comunicação (Não ou Sim)
             <p><b>DMO:</b> grau de Disfunção Motora Oral (DMO), em quatro categorias 
             (Normal, Leve, Moderada ou Severa)
             <p><b>Tempo líquido:</b> tempo, em segundos, para deglutição de 100 ml 
             de suco de laranja
             <p><b>Tempo pastoso:</b> tempo, em segundos, para deglutição de 140 g de
             iogurte de morango homogêneo
             e sem pedaços de fruta
             <p><b>Tempo sólido:</b> tempo, em segundos, para deglutição de 12 g de 
             bolacha recheada de chocolate"
                       )
                     )
              )
            ),
            
            fluidRow(
              column(10,
                     wellPanel(
                       tabsetPanel(
                         tabPanel('Ex01', uiOutput(ns('ex1'))),
                         tabPanel("Ex02", uiOutput(ns('ex2'))),
                         tabPanel('Ex03', uiOutput(ns('ex3'))),
                         tabPanel('Ex04', uiOutput(ns('ex4'))),
                         tabPanel('Ex05', uiOutput(ns('ex5'))),
                         tabPanel('Ex06', uiOutput(ns('ex6'))),
                         tabPanel('Ex07', uiOutput(ns('ex7'))),
                         tabPanel('Ex08', uiOutput(ns('ex8'))),
                         #tabPanel('Ex9', uiOutput(ns('ex9'))),
                         tabPanel('Ex09', uiOutput(ns('ex10')))
                       )
                     )
              )
            )
          )
  )
}

# server ----
exerc_paralisia_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$tabs, {
      if (input$tabs == 'ex1') {
        numero_do_exercicio(1)  
      } else if (input$tabs == 'ex2') {
        numero_do_exercicio(2)  
      } else if (input$tabs == 'ex3') {
        numero_do_exercicio(3)  
      } else if (input$tabs == 'ex4') {
        numero_do_exercicio(4)  
      } else if (input$tabs == 'ex5') {
        numero_do_exercicio(5)  
      } else if (input$tabs == 'ex6') {
        numero_do_exercicio(6)  
      } else if (input$tabs == 'ex7') {
        numero_do_exercicio(7)  
      } else if (input$tabs == 'ex8') {
        numero_do_exercicio(8)  
      } else if (input$tabs == 'ex9') {
        numero_do_exercicio(9)  
      } else if (input$tabs == 'ex10') {
        numero_do_exercicio(10)  
      }
    })
    
    ### 01UI para exercício 1 ----
    output$ex1 <- renderUI({
      enunciado <- questoes_paralisia[[1]]
      fluidRow(
        tags$head(tags$script(HTML('
    $.fn.selectpicker.defaults = {
      noneSelectedText: "Nada selecionado",
      noneResultsText: "Nenhum resultado encontrado {0}",
      countSelectedText: "{0} itens selecionados",
      maxOptionsText: ["Limite atingido ({n} itens)", "Limite de grupo atingido ({n} itens)"],
      selectAllText: "Selecionar todos",
      deselectAllText: "Desmarcar todos",
      multipleSeparator: ", "
    };
  '))),
        
        h3(''),
        column(10,
               wellPanel(
                 p(HTML(enunciado))
               ),
               column(9,
                      pickerInput(ns('variavel_quali'), 'Quais variáveis são qualitativas?',
                                  choices = nomes_exibidos, 
                                  multiple = TRUE,
                                  selected = 'Nada selecionado'),
                      pickerInput(ns('variavel_quanti'), 'Quais variáveis são quantitativas?',
                                  choices = nomes_exibidos, 
                                  multiple = TRUE,
                                  selected = 'Nada selecionado'),
                      actionButton(ns('verif_resp_ex1'), 'Verificar'),
                      align = 'center'
               )
        )
      )
    })
    
    # Ex1 - Validação das respostas
    observeEvent(input$verif_resp_ex1, {
      mensagem <- reactive({
        if (is.null(input$variavel_quali) && is.null(input$variavel_quanti)) {
          return("Você não selecionou as respostas!")
        } else {
          if (identical(input$variavel_quali, c("sexo", "grupo", "perda_audit", "dist_comun", "dmo")) & 
              identical(input$variavel_quanti, c("idade", "td_liquido", "td_pastoso", "td_solido"))) {
            return("Resposta correta.")
          } else {
            return("Há algo errado com as suas seleções.")
          }
        }
      })
      shinyalert(
        title = "",
        text = mensagem(),
        type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
      )
    }, ignoreNULL = TRUE)
    
    
    ### 02UI para exercício 2 ----
    output$ex2 <- renderUI({
      enunciado <- questoes_paralisia[[2]]
      fluidRow(
        
        h3(''),
        column(10,
               wellPanel(
                 p(HTML(enunciado))
               ),
               column(9,
                      pickerInput(ns('variavel_ex2'), 'Medidas-resumo',
                                  choices = c('Média', 'Mediana', 
                                              'Porcentagem', 'Frequência absoluta',
                                              'Desvio-padrão', 'Frequência relativa', 'Quartis'), 
                                  multiple = TRUE,
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      actionButton(ns('verif_resp_ex2'), 'Verificar'),
                      align = 'center'
               )
        )
      )
    })
    
    # Ex2 - Validação das respostas
    observeEvent(input$verif_resp_ex2, {
      mensagem <- reactive({
        if (is.null(input$variavel_ex2)) {
          return("Você não selecionou as respostas!")
        } else {
          if (identical(input$variavel_ex2, c('Média', 'Mediana', 'Desvio-padrão', 'Quartis'))) {
            return("Resposta correta.")
          } else {
            return("Há algo errado com sua seleção.")
          }
        }
      })
      shinyalert(
        title = "",
        text = mensagem(),
        type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
      )
    }, ignoreNULL = TRUE)
    
    ### 03UI para exercício 3 ----
    output$ex3 <- renderUI({
      enunciado <- questoes_paralisia[[3]]
      fluidRow(
        h3(''),
        column(10,
               wellPanel(
                 p(HTML(enunciado))
               ),
               column(9,
                      pickerInput(ns('variavel_ex3'), 'Medidas-resumo',
                                  choices = c('Média', 'Mediana', 'Porcentagem',
                                              'Frequência absoluta', 'Desvio-padrão',
                                              'Frequência relativa'),
                                  multiple = TRUE,
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      actionButton(ns('verif_resp_ex3'), 'Verificar'),
                      align = 'center')
        )
      )
    })
    
    # Ex3 - Validação das respostas
    observeEvent(input$verif_resp_ex3, {
      mensagem <- reactive({
        if (is.null(input$variavel_ex3)) {
          return("Você não selecionou as respostas!")
        } else {
          if (identical(input$variavel_ex3, c('Porcentagem'))) {
            return("Resposta correta.")
          } else {
            return("Há algo errado com sua seleção.")
          }
        }
      })
      shinyalert(
        title = "",
        text = mensagem(),
        type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
      )
    }, ignoreNULL = TRUE)
    
    ### 04UI para exercício 4 ----
    output$ex4 <- renderUI({
      enunciado <- questoes_paralisia[[4]]
      fluidRow(
        h3(''),
        column(10,
               wellPanel(
                 p(HTML(enunciado))
               ),
               column(9, offset = 1,
                      pickerInput(ns('variavel_ex4x'), 'Eixo x',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput(ns('variavel_ex4y'), 'Eixo y',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput(ns('variavel_ex4_graf'), 'Tipo de gráfico',
                                  choices = c('Barras', 'Boxplot', 'Dispersão'),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      actionButton(ns('graf_ex4'), 'Gerar gráfico'),
                      align = 'center'
               ),
               br(),
               column(9, offset = 1,
                      br(),
                      plotOutput(ns('plot_ex4')),
                      align = 'center'
               )
        )
      )
    })
    
    # Ex4 - Validação das respostas
    observeEvent(input$graf_ex4, {
      output$plot_ex4 <- renderPlot({
        req(input$variavel_ex4x != input$variavel_ex4y,
            input$variavel_ex4x == 'dmo' || input$variavel_ex4x == 'Não se aplica',
            input$variavel_ex4y == 'dmo' || input$variavel_ex4y == 'Não se aplica',
            input$variavel_ex4_graf == 'Barras')
        
        x_label <- if(input$variavel_ex4x == 'dmo'){
          'Disfunção Motora Oral'
        } else{
          'Frequência absoluta'
        }
        
        y_label <- if(input$variavel_ex4y == 'dmo'){
          'Disfunção Motora Oral'
        } else{
          'Frequência absoluta'
        }
        
        if(input$variavel_ex4x == 'dmo'){
          plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_ex4x, fill = 'dmo')) +
            geom_bar(stat = 'count') +
            theme_minimal() +
            labs(x = x_label, y = y_label, fill = 'DMO') +
            scale_fill_manual(values = c(colorful[1], colorful[2], colorful[3], colorful[4], colorful[5]))
          return(plot)
        }
        if (input$variavel_ex4y == 'dmo'){
          plot <- ggplot(dados_paralisia, aes_string(y = input$variavel_ex4y, fill = 'dmo')) +
            geom_bar(stat = 'count') +
            theme_minimal() +
            labs(x = x_label, y = y_label, fill = 'DMO') +
            scale_fill_manual(values = c(colorful[1], colorful[2], colorful[3], colorful[4], colorful[5]))
          return(plot)
        }
      })
      mensagem <- reactive({
        if (is.null(input$variavel_ex4x) && is.null(input$variavel_ex4y) && is.null(input$variavel_ex4_graf)) {
          return("Você não selecionou as respostas!")
        } else {
          if (identical(input$variavel_ex4x, 'dmo') && identical(input$variavel_ex4y, 'Não se aplica') && identical(input$variavel_ex4_graf, 'Barras')) {
            return("Resposta correta.")
          } else if (identical(input$variavel_ex4x, 'Não se aplica') && identical(input$variavel_ex4y, 'dmo') && identical(input$variavel_ex4_graf, 'Barras')) {
            return("Resposta correta.")
          } else {
            return("Há algo errado com sua seleção.")
          }
        }
      })
      shinyalert(
        title = "",
        text = mensagem(),
        type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
      )
    }, ignoreNULL = TRUE)
    
   
    ### 05UI para exercício 5 ----
    output$ex5 <- renderUI({
      enunciado <- questoes_paralisia[[5]]
      fluidRow(
        h3(''),
        column(10,
               wellPanel(
                 p(HTML(enunciado))
               ),
               column(9, offset = 1,
                      pickerInput(ns('variavel_ex5x'), 'Eixo x',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput(ns('variavel_ex5y'), 'Eixo y',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput(ns('variavel_ex5_graf'), 'Tipo de gráfico',
                                  choices = c('Barras', 'Boxplot', 'Dispersão'),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      actionButton(ns('graf_ex5'), 'Gerar gráfico'),
                      align = 'center'
               ),
               column(9, offset = 1,
                      br(),
                      plotOutput(ns('plot_ex5')),
                      align = 'center'
               )
        )
      )
    })
    
    # Ex5 - Validação das respostas
    observeEvent(input$graf_ex5, {
      output$plot_ex5 <- renderPlot({
        req(input$variavel_ex5x != input$variavel_ex5y,
            input$variavel_ex5x == 'dist_comun' || input$variavel_ex5x == 'Não se aplica',
            input$variavel_ex5y == 'dist_comun' || input$variavel_ex5y == 'Não se aplica',
            input$variavel_ex5_graf == 'Barras')
        
        x_label <- if(input$variavel_ex5x == 'dist_comun'){
          'Distúrbio de Comunicação'
        } else{
          'Frequência absoluta'
        }
        
        y_label <- if(input$variavel_ex5y == 'dist_comun'){
          'Distúrbio de Comunicação'
        } else{
          'Frequência absoluta'
        }
        
        if(input$variavel_ex5x == 'dist_comun'){
          plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_ex5x, fill = 'dist_comun')) +
            geom_bar(stat = 'count') +
            theme_minimal() +
            labs(x = x_label, y = y_label, fill = 'Distúrbio de Comunicação') +
            scale_fill_manual(values = c(colorful[1], colorful[2], colorful[3], colorful[4], colorful[5]))
          return(plot)
        }
        if (input$variavel_ex5y == 'dist_comun'){
          plot <- ggplot(dados_paralisia, aes_string(y = input$variavel_ex5y, fill = 'dist_comun')) +
            geom_bar(stat = 'count') +
            theme_minimal() +
            labs(x = x_label, y = y_label, fill = 'Distúrbio de Comunicação') +
            scale_fill_manual(values = c(colorful[1], colorful[2], colorful[3], colorful[4], colorful[5]))
          return(plot)
        }
      })
      mensagem <- reactive({
        if (is.null(input$variavel_ex5x) && is.null(input$variavel_ex5y) && is.null(input$variavel_ex5_graf)) {
          return("Você não selecionou as respostas!")
        } else {
          if (identical(input$variavel_ex5x, 'dist_comun') && identical(input$variavel_ex5y, 'Não se aplica') && identical(input$variavel_ex5_graf, 'Barras')) {
            return("Resposta correta.")
          } else if (identical(input$variavel_ex5x, 'Não se aplica') && identical(input$variavel_ex5y, 'dist_comun') && identical(input$variavel_ex5_graf, 'Barras')) {
            return("Resposta correta.")
          } else {
            return("Há algo errado com sua seleção.")
          }
        }
      })
      shinyalert(
        title = "",
        text = mensagem(),
        type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
      )
    }, ignoreNULL = TRUE)
    
    
    
    ### 06UI para exercício 6 ----
    output$ex6 <- renderUI({
      enunciado <- questoes_paralisia[[6]]
      fluidRow(
        h3(''),
        column(10,
               wellPanel(
                 p(HTML(enunciado))
               ),
               column(9, offset = 1,
                      pickerInput(ns('variavel_ex6x'), 'Eixo x',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput(ns('variavel_ex6y'), 'Eixo y',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput(ns('variavel_ex6_graf'), 'Tipo de gráfico',
                                  choices = c('Barras', 'Boxplot', 'Dispersão'),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      actionButton(ns('graf_ex6'), 'Gerar gráfico'),
                      align = 'center'
               ),
               column(9, offset = 1,
                      plotOutput(ns('plot_ex6')),
                      align = 'center')
        )
      )
    })
    
    # Ex6 - Validação das respostas
    observeEvent(input$graf_ex6, {
      output$plot_ex6 <- renderPlot({
        req(input$variavel_ex6x != input$variavel_ex6y,
            input$variavel_ex6x == 'td_liquido' || input$variavel_ex6x == 'Não se aplica',
            input$variavel_ex6y == 'td_liquido' || input$variavel_ex6y == 'Não se aplica',
            input$variavel_ex6_graf == 'Boxplot')
        
        x_label <- if(input$variavel_ex6x == 'td_liquido'){
          'Tempo líquido'
        } else{
          ''
        }
        
        y_label <- if(input$variavel_ex6y == 'td_liquido'){
          'Tempo líquido'
        } else{
          ''
        }
        
        if(input$variavel_ex6x == 'td_liquido'){
          plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_ex6x, fill = as.factor(input$variavel_ex6x))) +
            geom_boxplot() +
            theme_minimal() +
            scale_fill_manual(values = c(colorful[1]), labels = 'Tempo') +
            ylab('Tempo Líquido') +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()) +
            labs(x = x_label, y = y_label, fill = 'Tempo Líquido')
          return(plot)
        }
        
        if (input$variavel_ex6y == 'td_liquido'){
          plot <- ggplot(dados_paralisia, aes_string(y = input$variavel_ex6y, fill = as.factor(input$variavel_ex6y))) +
            geom_boxplot() +
            theme_minimal() +
            scale_fill_manual(values = c(colorful[1]), labels = 'Tempo') +
            ylab('Tempo Líquido') +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()) +
            labs(x = x_label, y = y_label, fill = 'Tempo Líquido')
          return(plot)
        }
      })
      
      mensagem <- reactive({
        if (is.null(input$variavel_ex6x) && is.null(input$variavel_ex6y) && is.null(input$variavel_ex6_graf)) {
          return("Você não selecionou as respostas!")
        } else {
          if (identical(input$variavel_ex6x, 'Não se aplica') && identical(input$variavel_ex6y, 'td_liquido') && identical(input$variavel_ex6_graf, 'Boxplot')) {
            return("Resposta correta.")
          } else if (identical(input$variavel_ex6x, 'td_liquido') && identical(input$variavel_ex6y, 'Não se aplica') && identical(input$variavel_ex6_graf, 'Boxplot')) {
            return("Resposta correta.")
          } else {
            return("Há algo errado com sua seleção.")
          }
        }
      })
      shinyalert(
        title = "",
        text = mensagem(),
        type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
      )
    }, ignoreNULL = TRUE)
    
    
    ### 07UI para exercício 7 ----
    output$ex7 <- renderUI({
      enunciado <- questoes_paralisia[[7]]
      fluidRow(
        h3('Exercício 7'),
        column(10,
               wellPanel(
                 p(HTML(enunciado)),
                 column(9, 
                        br(),
                        h5('Tabela 1 - Distribuição de perda auditiva 
  pelos grupos de crianças com paralisia cerebral e as sem acometimentos 
  neurológicos'),
                        tags$style(HTML("
                      .gt_table {
                        font-size: 12px;
                      }
                    ")),
                        gt_output(outputId = ns('tabela_obs_ex7')),
                        br(),
                        h5('Tabela 2 - Frequências absolutas esperadas
                           sob hipótese de não associação entre as variáveis'),
                        gt_output(outputId = ns('tabela_esperada_ex7')),
                        align = 'center'
                 )
               )
        ),
        column(10,
               wellPanel(
                 p(HTML('<b>a)</b> Com um nível de significância de 5%, escolha 
                     o teste estatístico mais apropriado para verificar se há 
                     associação estatisticamente significante 
                     entre essas variáveis?<br>')
                 ),
                 column(9, 
                        br(),
                        br(),
                        pickerInput(ns('teste_ex7'), 'Escolha um teste:',
                                    choices = c('Qui-Quadrado', 'Qui-Quadrado via simulação de Monte Carlo'),
                                    options = list(noneSelectedText = 'Nada selecionado')),
                        actionButton(ns('botao_teste_ex7'), 'Verificar'),
                        br(),
                        br(),
                        verbatimTextOutput(ns('resultado_teste_ex7')),
                        align = 'center'
                 )
               )
        ),
        column(10,
               wellPanel(
                 p(HTML('<b>b)</b> De acordo com o resultado do teste,
                     é correto afirmar que as variáveis estão associadas ao nível 
                     de significância de 5%?<br>')),
                 column(9,
                        br(),
                        br(),
                        uiOutput(ns('ex7_parteb')),
                        align = 'center'
                 )
               )
        )
      )
    })
    
    # Ex7 - Validação das respostas
    output$tabela_obs_ex7 <- render_gt({
      dados_paralisia %>%
        select(perda_audit, grupo) %>%
        tbl_summary(by=grupo,
                    missing = "no", 
                    label = list(perda_audit = "Perda Auditiva"),
                    statistic = list(all_categorical() ~ "{n} ({p}%)"),
                    percent = "col",
                    digits = list(all_categorical() ~ c(0,0))) %>% 
        modify_header(label = "**Variável**") %>% 
        modify_footnote(update = list(
          starts_with("stat_") ~ "n (%): frequência absoluta e porcentagem")) %>% 
        modify_spanning_header(all_stat_cols() ~ "**Grupo de crianças**") %>%  
        as_gt() %>%
        tab_options(table.width = pct(100))
    })
    
    output$tabela_esperada_ex7 <- render_gt({
      qui_quad_ex7 <- chisq.test(dados_paralisia$perda_audit, 
                                 dados_paralisia$grupo, 
                                 simulate.p.value = TRUE,
                                 B = 10000)
      tabela_esperada_ex7 <- as.data.frame(qui_quad_ex7$expected)
      colnames(tabela_esperada_ex7) <- c("PC", "SAN")
      rownames(tabela_esperada_ex7) <- c("Não", "Sim")
      
      tabela_esperada_ex7 %>%
        rownames_to_column(var = "Perda Auditiva") %>%
        mutate(PC = round(PC), SAN = round(SAN)) %>%
        gt() %>%
        tab_options(table.width = pct(100))
    })
    
    observeEvent(input$botao_teste_ex7, {
      req(input$teste_ex7)
      if(input$teste_ex7 == 'Qui-Quadrado via simulação de Monte Carlo'){
        output$resultado_teste_ex7 <- renderPrint({
          qui_quad_7 <- chisq.test(dados_paralisia$perda_audit, dados_paralisia$grupo, simulate.p.value = TRUE, B = 10000)
          cat('p-valor do Teste Qui-Quadrado (Monte Carlo):', qui_quad_7$p.value)
        })
      }
      mensagem <- reactive({
        if(input$teste_ex7 == 'Qui-Quadrado via simulação de Monte Carlo'){
          return('Resposta correta.')
        } else {
          if(is.null(input$teste_ex7)){
            return('Você não selecionou as respostas')
          } else {
            return('Há algo errado com sua seleção.')
          }
        }
      })
      shinyalert(
        title = '',
        text = mensagem(),
        type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
      )
    }, ignoreNULL = TRUE)
    
    output$ex7_parteb <- renderUI({
      req(input$botao_teste_ex7)
      req(input$teste_ex7 == 'Qui-Quadrado via simulação de Monte Carlo')
      if(input$teste_ex7 == 'Qui-Quadrado via simulação de Monte Carlo'){
        fluidRow(
          pickerInput(ns('relacao'), 'Há relação entre as variáveis?',
                      choices = c('Sim', 'Não')),
          actionButton(ns('verificar_teste'), 'Verificar')
        )
      }
    })
    
    observeEvent(input$verificar_teste, {
      mensagem <- reactive({
        if(input$relacao == 'Não'){
          return('Resposta correta.')
        } else {
          if(is.null(input$relacao)){
            return('Você não selecionou as respostas')
          } else {
            return('Há algo errado com sua seleção.')
          }
        }
      })
      
      shinyalert(
        title = '',
        text = mensagem(),
        type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
      )
    })
    
    ### 08UI para exercício 8 ----
    output$ex8 <- renderUI({
      enunciado <- questoes_paralisia[[8]]
      fluidRow(
        h3(''),
        column(10,
               wellPanel(
                 p(HTML(enunciado)),
                 column(9, 
                        br(),
                        h5('Tabela 1 - Distribuição dos distúrbios de 
                        comunicação pelos grupos de crianças com paralisia 
                        cerebral e as sem acometimentos neurológicos'),
                        tags$style(HTML("
                      .gt_table {
                        font-size: 12px;
                      }
                    ")),
                        gt_output(outputId = ns('tabela_ex8_pc')),
                        br(),
                        h5('Tabela 2 - Frequências absolutas esperadas
                           sob hipótese de não associação entre as variáveis'),
                        gt_output(outputId = ns('tabela_esperada_ex8')),
                        align = 'center'
                 )
               )
        ),
        column(10,
               wellPanel(
                 p(HTML('<b>a)</b> Com um nível de significância de 5%,
                        qual é o teste estatístico mais apropriado para 
                        verificar se há associação estatisticamente 
                        significante entre essas variáveis?')),
                 column(9,
                        br(),
                        br(),
                        pickerInput(ns('teste_ex8'), 
                                    'Escolha o teste:',
                                    choices = c('Qui-Quadrado', 
                                                'Qui-Quadrado via simulação de Monte Carlo'),
                                    options = list(noneSelectedText = 'Nada
                                                   selecionado')),
                        actionButton(ns('botao_teste_ex8'), 'Verificar'),
                        br(),
                        br(),
                        verbatimTextOutput(ns('resultado_teste_ex8')),
                        align = 'center'
                 )
               )
        ),
        column(10,
               wellPanel(
                 p(HTML('<b>b)</b> De acordo com o resultado do teste,
                        é correto afirmar que as variáveis estão associadas ao
                        nível de significância de 5%?')),
                 column(9,
                        br(),
                        br(),
                        uiOutput(ns('ex8_parteb')),
                        align = 'center'
                 )
               )
        )
      )
    })
    
    # Ex8 - Validação das respostas
    output$tabela_ex8_pc <- render_gt({
      dados_paralisia %>%
        select(dist_comun, grupo) %>%
        tbl_summary(by = grupo,
                    missing = "no",
                    label = list(dist_comun = "Distúrbio de Comunicação"),
                    statistic = list(all_categorical() ~ "{n} ({p}%)"),
                    percent = "col",
                    digits = list(all_categorical() ~ c(0, 0))) %>%
        modify_header(label = "**Variável**") %>%
        modify_footnote(update = list(starts_with("stat_") ~ "n (%)")) %>%
        modify_footnote(list(starts_with("stat_") ~ "n (%): frequência absoluta e porcentagem")) %>%
        modify_spanning_header(all_stat_cols() ~ "**Grupo de crianças**") %>%
        as_gt() %>%
        tab_options(table.width = pct(100))
    })
    
    output$tabela_esperada_ex8 <- render_gt({
      qui_quad_ex8 <- chisq.test(dados_paralisia$dist_comun, 
                                 dados_paralisia$grupo,
                                 simulate.p.value = FALSE,
                                 B = 10000)
      tabela_esperada_ex8 <- as.data.frame(qui_quad_ex8$expected)
      colnames(tabela_esperada_ex8) <- c("PC", "SAN")
      rownames(tabela_esperada_ex8) <- c("Não", "Sim")
      
      tabela_esperada_ex8 %>%
        tibble::rownames_to_column(var = "Distúrbio de Comunicação") %>%
        mutate(PC = round(PC), SAN = round(SAN)) %>% 
        gt() 
    })
    
    observeEvent(input$botao_teste_ex8, {
      req(input$teste_ex8)
      if(input$teste_ex8 == 'Qui-Quadrado'){
        output$resultado_teste_ex8 <- renderPrint({
          qui_quad_ex8 <- chisq.test(dados_paralisia$dist_comun, 
                                     dados_paralisia$grupo, 
                                     simulate.p.value = FALSE, 
                                     B = 10000)
          cat('p-valor do teste Qui-Quadrado:', qui_quad_ex8$p.value)
        })
      } else {
          output$resultado_teste_ex8 <- renderPrint({
            qui_quad_ex8 <- chisq.test(dados_paralisia$dist_comun, 
                                       dados_paralisia$grupo, 
                                       simulate.p.value = TRUE, 
                                       B = 10000)
            cat('p-valor do teste Qui-Quadrado (Monte Carlo):', qui_quad_ex8$p.value)
          })
        }

      mensagem <- reactive({
        if (input$teste_ex8 == 'Qui-Quadrado') {
          return('Resposta correta.')} 
        else {
          if (input$teste_ex8 == 'Qui-Quadrado via simulação de Monte Carlo') {
            return('Resposta correta.')} 
          else {
            if(is.null(input$teste_ex8)){
              return('Você não selecionou as respostas')
            } 
            else {
              return('Há algo errado com sua seleção.')
            }
          }
        }
      })
      
        shinyalert(
        title = '',
        text = mensagem(),
        type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
      )
    }, ignoreNULL = TRUE)
    
    output$ex8_parteb <- renderUI({
      req(input$botao_teste_ex8)
        fluidRow(
          pickerInput(ns('relacao_ex8'), 'Há relação entre as variáveis?',
                      choices = c('Sim', 'Não')),
          actionButton(ns('verificar_teste_ex8'), 'Verificar')
        )
    })
    
    observeEvent(input$verificar_teste_ex8, {
      mensagem <- reactive({
        if(input$relacao_ex8 == 'Não'){
          return('Resposta correta.')
        } else {
          if(is.null(input$relacao_ex8)){
            return('Você não selecionou as respostas')
          } else {
            return('Há algo errado com sua seleção.')
          }
        }
      })
      
      shinyalert(
        title = '',
        text = mensagem(),
        type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
      )
    })
    
    
    ### 09UI para exercício 9 ----
    output$ex9 <- renderUI({
      enunciado <- questoes_paralisia[[9]]
      fluidRow(
        h3(''),
        column(10,
               wellPanel(
                 p(HTML(enunciado)),
                 column(9,
                        pickerInput(ns('variavel_ex9x'), 'Eixo x',
                                    choices = c('Não se aplica', nomes_exibidos),
                                    options = list(noneSelectedText = 'Nada selecionado')),
                        pickerInput(ns('variavel_ex9y'), 'Eixo y',
                                    choices = c('Não se aplica', nomes_exibidos),
                                    options = list(noneSelectedText = 'Nada selecionado')),
                        pickerInput(ns('variavel_ex9_graf'), 'Tipo de gráfico',
                                    choices = c('Barras', 'Boxplot', 'Dispersão'),
                                    options = list(noneSelectedText = 'Nada selecionado')),
                        actionButton(ns('graf_ex9'), 'Gerar gráfico'),
                        plotOutput(ns('plot_ex9')),
                        align = 'center'
                 )
               )
        ),
        column(10,
               wellPanel(
                 h5('<b>Testes de normalidade da variável quantitativa pelos níveis
             da variável qualitativa:</b>'),
                 column(9,
                        verbatimTextOutput(ns('resultado_teste_norm_ex9')),
                        align = 'center'
                 )
               )
        ),
        column(10,
               wellPanel(
                 p(HTML('<b>a)</b> A partir dos resultados dos testes de normalidade
             exibidos, qual é o teste estatístico mais apropriado
      para verificar se há associação estatisticamente significante entre essas 
      variáveis a um nível de 5%?')),
                 column(9,
                        uiOutput(ns('ex9_parteb')),
                        uiOutput(ns('resultado_teste_ex9')),
                        align = 'center'
                 )
               )
        ),
        column(10,
               wellPanel(
                 p(HTML('<b>b)</b> De acordo com o resultado do teste,
           é correto afirmar que as variáveis estão 
                  associadas ao nível de significância de 5%?')
                 ),
                 column(9,
                        uiOutput(ns('ex9_partec')),
                        align = 'center'
                 )
               )
        )
      )
    })
    
    ### 10UI para exercício 10 ----
    output$ex10 <- renderUI({
      enunciado <- questoes_paralisia[[10]]
      fluidRow(
        h3(''),
        column(10,
               wellPanel(
                 p(HTML(enunciado)),
                 column(9,
                        br(),
                        br(),
                        pickerInput(ns('variavel_ex10x'), 'Eixo x',
                                    choices = c('Não se aplica', nomes_exibidos),
                                    options = list(noneSelectedText = 'Nada selecionado')),
                        pickerInput(ns('variavel_ex10y'), 'Eixo y',
                                    choices = c('Não se aplica', nomes_exibidos),
                                    options = list(noneSelectedText = 'Nada selecionado')),
                        pickerInput(ns('variavel_ex10_graf'), 'Tipo de gráfico',
                                    choices = c('Barras', 'Boxplot', 'Dispersão'),
                                    options = list(noneSelectedText = 'Nada selecionado')),
                        actionButton(ns('graf_ex10'), 'Gerar gráfico'),
                        br(),
                        br(),
                        plotOutput(ns('plot_ex10')),
                        br(),
                        align = 'center')
               )
        ),
        br(),
        br(),
        column(10,
               wellPanel(  
                 h5(HTML('<b>Testes de normalidade da variável quantitativa pelos níveis
             da variável qualitativa: </b>')),
                 column(9,
                        br(),
                        br(),
                        verbatimTextOutput(ns('resultado_teste_norm_ex10')),
                        align = 'center'
                 )
               )
        ),
        column(10,
               wellPanel(  
                 p(HTML('<b>a)</b> A partir dos resultados dos testes de normalidade
             exibidos, qual é o teste estatístico mais apropriado 
             para verificar se há relação estatisticamente significante
             entre elas a um nível de 5%?')),
                 column(9, 
                        uiOutput(ns('ex10_parteb')),
                        br(),
                        verbatimTextOutput(ns('resultado_teste_ex10')),
                        align = 'center')
               )
        ),
        column(10,
               wellPanel(  
                 p(HTML('<b>b)</b> De acordo com o resultado do teste, 
                  é correto afirmar que as variáveis estão
                         relacionadas ao nível de significância de 5%?')
                 ),
                 column(9,
                        uiOutput(ns('ex10_partec')),
                        align = 'center'
                 )
               )
        )
      )
    })
    
    # Ex10 - Validação das respostas
    observeEvent(input$graf_ex10, {
      output$plot_ex10 <- renderPlot({
        req(input$variavel_ex10x != input$variavel_ex10y, 
            (input$variavel_ex10x == 'td_liquido' || input$variavel_ex10x == 'td_solido'),
            (input$variavel_ex10y == 'td_liquido' || input$variavel_ex10y == 'td_solido'),
            input$variavel_ex10_graf == 'Dispersão')
        
        x_label <- if (input$variavel_ex10x == 'td_liquido') {
          'Tempo líquido'
        } else {
          'Tempo sólido'
        }
        
        y_label <- if (input$variavel_ex10y == 'td_liquido') {
          'Tempo líquido'
        } else {
          'Tempo sólido'
        }
        
        plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_ex10x, y = input$variavel_ex10y)) +
          geom_point() +
          labs(x = x_label, y = y_label) +
          theme_minimal()
        
        return(plot)
      })
      mensagem <- reactive({
        if (is.null(input$variavel_ex10x) && is.null(input$variavel_ex10y) && is.null(input$variavel_ex10_graf)) {
          return("Você não selecionou as respostas!")
        } else {
          if (identical(input$variavel_ex10x, 'td_liquido') && identical(input$variavel_ex10y, 'td_solido') && identical(input$variavel_ex10_graf, 'Dispersão')) {
            return("Resposta correta.")
          } else if (identical(input$variavel_ex10x, 'td_solido') && identical(input$variavel_ex10y, 'td_liquido') && identical(input$variavel_ex10_graf, 'Dispersão')) {
            return("Resposta correta.")
          } else {
            return("Há algo errado com sua seleção.")
          }
        }
      })
      
      shinyalert(
        title = "",
        text = mensagem(),
        type = ifelse(mensagem() == "Resposta correta.", "success", "warning")
      )
    }, ignoreNULL = TRUE)
    
    output$ex10_parteb <- renderUI({
      req(input$variavel_ex10x != input$variavel_ex10y, 
          (input$variavel_ex10x == 'td_liquido' || input$variavel_ex10x == 'td_solido'),
          (input$variavel_ex10y == 'td_liquido' || input$variavel_ex10y == 'td_solido'),
          input$variavel_ex10_graf == 'Dispersão')
      req(input$graf_ex10)
      if(input$variavel_ex10x == 'td_liquido' || input$variavel_ex10x == 'td_solido' && input$variavel_ex10y == 'td_liquido' || input$variavel_ex10y == 'td_solido' && input$variavel_ex10_graf =='Dispersão' && input$variavel_ex10x != input$variavel_ex10y){
        fluidRow(
          br(),
          br(),
          pickerInput(ns('teste_ex10'), 'Escolha o teste:',
                      choices = c('Teste de Correlação de Spearman', 'Qui-Quadrado', 't de Student')),
          actionButton(ns('verificar_teste_ex10'), 'Verificar')
        )
      }
    })
    
    observeEvent(input$graf_ex10, {
      if(input$variavel_ex10x == 'td_liquido' || input$variavel_ex10x == 'td_solido' && input$variavel_ex10y == 'td_liquido' || input$variavel_ex10y == 'td_solido' && input$variavel_ex10_graf =='Dispersão' && input$variavel_ex10x != input$variavel_ex10y){
        output$resultado_teste_norm_ex10 <- renderPrint({
          shapiro_tdliq <- shapiro.test(dados_paralisia$td_liquido)
          shapiro_tdsol <- shapiro.test(dados_paralisia$td_solido)
          
          cat('p-valor do teste de Shapiro-Wilk para tempo líquido',
              shapiro_tdliq$p.value, '\n',
              'p-valor do teste de Shapiro-Wilk para tempo sólido',
              shapiro_tdsol$p.value)
        })
      }
    })
    
    observeEvent(input$verificar_teste_ex10, {
      if(input$teste_ex10 == 'Teste de Correlação de Spearman'){
        output$resultado_teste_ex10 <- renderPrint({
          teste_cor_ex10 <- cor.test(dados_paralisia$td_liquido, dados_paralisia$td_solido,
                                     exact = FALSE, method = 'spearman')
          cat('p-valor do Teste de Correlação de Spearman', teste_cor_ex10$p.value)
        })
      }
      mensagem <- reactive({
        if(input$teste_ex10 == 'Teste de Correlação de Spearman'){
          return('Resposta correta.')
        } else {
          if(is.null(input$teste_ex10)){
            return('Você não selecionou as respostas')
          } else {
            return('Há algo errado com sua seleção.')
          }
        }
      })
      
      shinyalert(
        title = '',
        text = mensagem(),
        type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
      )
    })
    
    output$ex10_partec <- renderUI({
      req(input$verificar_teste_ex10)
      if(input$teste_ex10 == 'Teste de Correlação de Spearman'){
        fluidRow(
          br(),
          br(),
          pickerInput(ns('relacao_ex10'), 'Escolha sua resposta:',
                      choices = c('Sim', 'Não')),
          actionButton(ns('verificar_relacao_ex10'), 'Verificar')
        )
      }
    })
    
    observeEvent(input$verificar_relacao_ex10, {
      mensagem <- reactive({
        if(input$relacao_ex10 == 'Sim'){
          return('Resposta correta.')
        } else {
          return('Há algo errado com sua seleção')
        }
      })
      shinyalert(
        title = '',
        text = mensagem(),
        type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
      )
    })
    
  })
}