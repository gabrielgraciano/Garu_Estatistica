library(shiny)

ui <- 
  
  dashboardPage(
    dashboardHeader(title = 'Garu Estatística'),
    dashboardSidebar(
      sidebarMenu(
        
        menuItem('Exercícios Práticos',
                 menuSubItem('Dados - Paralisia Cerebral', tabName = 'paralisia'))
        
      )
    ),
    
    dashboardBody(
      
      tabItems(
        
        paralisia
      )
    )
  )



paralisia <-
  tabItem(tabName = 'paralisia',
          fluidPage(
            fluidRow(
              wellPanel(titlePanel(h3("Exercícios Práticos - Paralisia Cerebral")),
                        HTML("<p>Segundo as <a href= 'https://bvsms.saude.gov.br/bvs/publicacoes/diretrizes_atencao_paralisia_cerebral.pdf'> Diretrizes de Atenção à
              Pessoa com Paralisia Cerebral do Ministério da Saúde (2014)</a>,
             a paralisia cerebral (PC) descreve um grupo de desordens da desenvolução do movimento e postura atribuído
             à distúrbio não progressivo durante o desenvolvimento do cérebro fetal ou infantil.
             <br>
             <p>Estima-se que nos países em desenvolvimento, 7 a cada 1000 nascidos vivos
             sejam acometidos por PC, em diferentes graus de comprometimento dos movimentos e postura.
             <br>
             <p>Para fins didáticos, algumas informações fornecidas pelo Ministério da Saúde e achados de <a href= 'https://www.scielo.br/j/rboto/a/hvQv9C6JY9h7MbrrSY98WpB/abstract/?lang=pt'> Aurélio et al.
             (2002)</a> na comparação do padrão de deglutição de alimentos entre crianças com PC e crianças sem acometimentos neurológicos
             (SAN), Curitiba/PR, encontram-se simulados no banco de dados tratado neste problema.
             <br>
             <br>
             <p>Existem 9 variáveis tratadas besse banco de dados, sendo elas:
             <br>
             <p><b>Sexo<b>: menino ou menina
             <p>Idade: idade em anos completos
             <p>Grupo: grupo das crianças por condição de saúde (SAN ou PC)
             <p>Perda auditiva: existência ou não de perda auditiva
             <p>Distúrbio de Comunicação: existência ou não de distúrbio de comunicação
             <p>DMO: grau de DMO (disfunção motora oral, compreendido entre 0 e 4)
             <p>Tempo líquido: tempo, em segundos, para deglutição de 100 ml de suco de laranja
             <p>Tempo pastoso: tempo, em segundos, para deglutição de 140 g de iogurte de morango homogêneo
             e sem pedaços de fruta
             <p>Tempo sólido: tempo, em segundos, para deglutição de 12 g de bolacha recheada de chocolate"
                        )
              )
              #"column(4,
              #selectInput('exercicio_pc','Escolha o exercício a ser resolvido', choices = 1:12 ))"
            ),
            
            
            mainPanel(
              tabsetPanel(
                id = "tabs",
                tabPanel("alo", uiOutput('banana')),
                tabPanel("Ex2", uiOutput('ex2'))
              )
            )
            
            
            #mainPanel(
            # uiOutput('opcoes_exercicio_pc'),
            #actionButton('gerar_pc', 'Gerar')
            #uiOutput('grafico_pc') -> testar por esse caminho depois
            
            #)
            
          )
  )


server <- function(input, output) {
 
  output$ex2 <- renderUI({
    fluidRow(
      column(12, h2("Conteúdo do Ex2"))
    )
  })
  
  numero_exercicios <- reactiveVal(0)
  
  output$banana <- renderUI({
    enunciado <- enunciados_pc[[1]]
    
    numero_exercicio_atual <- numero_exercicios() + 1
    numero_exercicios(numero_exercicio_atual)
    
    fluidRow(
      column(4,
             p(HTML(enunciado)),
             
             h3('Seleção de variáveis'),
             
             pickerInput('variavel_quali', 'Quais variáveis são qualitativas?',
                         choices = nomes_exibidos, multiple = TRUE),
             pickerInput('variavel_quanti', 'Quais variáveis são quantitativas?',
                         choices = nomes_exibidos, multiple = TRUE),
             pickerInput('medidas_resumo_quali', 'Qual é uma medida resumo adequada para variáveis qualitativas?',
                         choices = c('Média', 'Porcentagem', 'Mediana')),
             pickerInput('medidas_resumo_quanti', 'Qual é uma medida resumo adequada para variáveis quantitativas?',
                         choices = c('Porcentagem', 'Frequência absoluta', 'Média'))
             
      ),
      column(8,
             DT::dataTableOutput('tabela_exs'),
             actionButton('gerar_pc', 'Gerar')
      )
      
      
    )
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)






















output$ex1 <- renderUI({
  enunciado <- enunciados_pc_novo[[1]]
  numero_do_exercicio <- 1
  
  
  fluidRow(
    column(4,
           p(HTML(enunciado)),
           
           h3('Seleção de variáveis'),
           
           pickerInput('variavel_quali', 'Quais variáveis são qualitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('variavel_quanti', 'Quais variáveis são quantitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('medidas_resumo_quali', 'Qual é uma medida resumo adequada para variáveis qualitativas?',
                       choices = c('Média', 'Porcentagem', 'Mediana')),
           pickerInput('medidas_resumo_quanti', 'Qual é uma medida resumo adequada para variáveis quantitativas?',
                       choices = c('Porcentagem', 'Frequência absoluta', 'Média'))
           
    ),
    column(8,
           DT::dataTableOutput('tabela_exs'),
           actionButton('gerando', 'Gerar')
    ),
    print(numero_do_exercicio)
    
  )
  
})

output$ex2 <- renderUI({
  enunciado <- enunciados_pc_novo[[2]]
  numero_do_exercicio <- 2
  
  
  fluidRow(
    column(4,
           p(HTML(enunciado)),
           
           h3('Seleção de variáveis'),
           
           pickerInput('variavel_quali', 'Quais variáveis são qualitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('variavel_quanti', 'Quais variáveis são quantitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('medidas_resumo_quali', 'Qual é uma medida resumo adequada para variáveis qualitativas?',
                       choices = c('Média', 'Porcentagem', 'Mediana')),
           pickerInput('medidas_resumo_quanti', 'Qual é uma medida resumo adequada para variáveis quantitativas?',
                       choices = c('Porcentagem', 'Frequência absoluta', 'Média'))
           
    ),
    column(8,
           DT::dataTableOutput('tabela_exs'),
           actionButton('gerando', 'Gerar')
    ),
    print(numero_do_exercicio)
    
  )
  
})

output$ex3 <- renderUI({
  enunciado <- enunciados_pc_novo[[3]]
  numero_do_exercicio <- 3
  
  
  fluidRow(
    column(4,
           p(HTML(enunciado)),
           
           h3('Seleção de variáveis'),
           
           pickerInput('variavel_quali', 'Quais variáveis são qualitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('variavel_quanti', 'Quais variáveis são quantitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('medidas_resumo_quali', 'Qual é uma medida resumo adequada para variáveis qualitativas?',
                       choices = c('Média', 'Porcentagem', 'Mediana')),
           pickerInput('medidas_resumo_quanti', 'Qual é uma medida resumo adequada para variáveis quantitativas?',
                       choices = c('Porcentagem', 'Frequência absoluta', 'Média'))
           
    ),
    column(8,
           DT::dataTableOutput('tabela_exs'),
           actionButton('gerando', 'Gerar')
    )
    
    
  )
  
})

output$ex4 <- renderUI({
  enunciado <- enunciados_pc_novo[[4]]
  numero_do_exercicio <- 5
  
  
  fluidRow(
    column(4,
           p(HTML(enunciado)),
           
           h3('Seleção de variáveis'),
           
           pickerInput('variavel_quali', 'Quais variáveis são qualitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('variavel_quanti', 'Quais variáveis são quantitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('medidas_resumo_quali', 'Qual é uma medida resumo adequada para variáveis qualitativas?',
                       choices = c('Média', 'Porcentagem', 'Mediana')),
           pickerInput('medidas_resumo_quanti', 'Qual é uma medida resumo adequada para variáveis quantitativas?',
                       choices = c('Porcentagem', 'Frequência absoluta', 'Média'))
           
    ),
    column(8,
           DT::dataTableOutput('tabela_exs'),
           actionButton('gerando', 'Gerar')
    )
    
    
  )
  
})

output$ex5 <- renderUI({
  enunciado <- enunciados_pc_novo[[5]]
  numero_do_exercicio <- 5
  
  
  fluidRow(
    column(4,
           p(HTML(enunciado)),
           
           h3('Seleção de variáveis'),
           
           pickerInput('variavel_quali', 'Quais variáveis são qualitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('variavel_quanti', 'Quais variáveis são quantitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('medidas_resumo_quali', 'Qual é uma medida resumo adequada para variáveis qualitativas?',
                       choices = c('Média', 'Porcentagem', 'Mediana')),
           pickerInput('medidas_resumo_quanti', 'Qual é uma medida resumo adequada para variáveis quantitativas?',
                       choices = c('Porcentagem', 'Frequência absoluta', 'Média'))
           
    ),
    column(8,
           DT::dataTableOutput('tabela_exs'),
           actionButton('gerando', 'Gerar')
    )
    
    
  )
  
})

output$ex6 <- renderUI({
  enunciado <- enunciados_pc_novo[[6]]
  numero_do_exercicio <- 6
  
  
  fluidRow(
    column(4,
           p(HTML(enunciado)),
           
           h3('Seleção de variáveis'),
           
           pickerInput('variavel_quali', 'Quais variáveis são qualitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('variavel_quanti', 'Quais variáveis são quantitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('medidas_resumo_quali', 'Qual é uma medida resumo adequada para variáveis qualitativas?',
                       choices = c('Média', 'Porcentagem', 'Mediana')),
           pickerInput('medidas_resumo_quanti', 'Qual é uma medida resumo adequada para variáveis quantitativas?',
                       choices = c('Porcentagem', 'Frequência absoluta', 'Média'))
           
    ),
    column(8,
           DT::dataTableOutput('tabela_exs'),
           actionButton('gerando', 'Gerar')
    )
    
    
  )
  
})

output$ex7 <- renderUI({
  enunciado <- enunciados_pc_novo[[7]]
  numero_do_exercicio <- 7
  
  
  fluidRow(
    column(4,
           p(HTML(enunciado)),
           
           h3('Seleção de variáveis'),
           
           pickerInput('variavel_quali', 'Quais variáveis são qualitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('variavel_quanti', 'Quais variáveis são quantitativas?',
                       choices = nomes_exibidos, multiple = TRUE),
           pickerInput('medidas_resumo_quali', 'Qual é uma medida resumo adequada para variáveis qualitativas?',
                       choices = c('Média', 'Porcentagem', 'Mediana')),
           pickerInput('medidas_resumo_quanti', 'Qual é uma medida resumo adequada para variáveis quantitativas?',
                       choices = c('Porcentagem', 'Frequência absoluta', 'Média'))
           
    ),
    column(8,
           DT::dataTableOutput('tabela_exs'),
           actionButton('gerando', 'Gerar')
    )
    
    
  )
  
})



validando_exercicios <- reactive({
  mensagem_de_erro <- character(0)
  req(numero_do_exercicio > 0)
  print(inpu t$exercicio_pc)
  req(!is.null(input$exercicio_pc))
  
  if(input$exercicio_pc == 1){
    ex <- 1
    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
    
    print(resposta_esperada)
    print(input$variavel_quali)
    print(input$variavel_quanti)
    print(input$medidas_resumo_quali)
    print(input$medidas_resumo_quanti)
    
    
    if (!identical(input$variavel_quali, resposta_esperada$variavel_quali)) {
      mensagem_de_erro <- c(mensagem_de_erro, "Não foram selecionadas todas as variáveis categóricas")
    }
    
    
    if (!identical(input$variavel_quanti, resposta_esperada$variavel_quanti)) {
      mensagem_de_erro <- c(mensagem_de_erro, "Não foram selecionadas todas as variáveis contínuas")
    }
    
    # Verificar se as medidas resumo selecionadas correspondem às esperadas
    
    if (input$medidas_resumo_quali != resposta_esperada$medidas_resumo_quali) {
      mensagem_de_erro <- c(mensagem_de_erro, "Essa não é a medida resumo adequada para variáveis categóricas")
    }
    
    
    if (input$medidas_resumo_quanti != resposta_esperada$medidas_resumo_quanti) {
      mensagem_de_erro <- c(mensagem_de_erro, "Essa não é a medida resumo adequada para variáveis contínuas")
    }
    
  }
  
  else if (length(mensagem_de_erro) > 0) {
    return(mensagem_de_erro)
  } else {
    return(NULL)
  }
  
})

observeEvent(input$gerando, {
  mensagens_erro <- validando_exercicios()
  print(mensagens_erro)  # Exibe as mensagens de erro no console
  if (length(mensagens_erro) > 0) {
    shinyalert(title = "Erro!", text = mensagens_erro, type = "error")
  }
})






criar_ui_exercicio <- function(numero_exercicio) {
  enunciado <- enunciados_pc[[numero_exercicio]]
  
  if (!is.null(enunciado)) {
    if (numero_exercicio %in% c(2, 3, 4, 8, 9, 10, 11, 12)) {
      resposta_esperada <- respostas_esperadas_pc[[paste0("ex", numero_exercicio)]]
      fluidRow(
        column(4,
               p(HTML(enunciado)),
               
               h3('Seleção de variáveis e Tipo de Gráfico'),
               
               pickerInput('variavel_pc_x', 'Escolha a variável para o eixo x:',
                           choices = c("Não se aplica", nomes_exibidos)),
               pickerInput('variavel_pc_y', 'Escolha a variável para o eixo y:',
                           choices = c("Não se aplica", nomes_exibidos)),
               pickerInput('tipo_grafico_pc', 'Escolha o tipo de gráfico:',
                           choices = c("Não se aplica", 'Boxplot', 'Dispersão', 'Barras'))
               
        ),
        
        column(8,
               plotOutput('grafico_pc')
        )
      )
    }
    
    
    
    #retirei exercicio 1 daqui
    
    else if (numero_exercicio == 5) {
      fluidRow(
        column(4,
               p(HTML(enunciado))
        ),
        column(8,
               h3('Seleção de variáveis'),
               
               pickerInput('medidas_resumo_ex5', 'Quais são as medidas-resumo adequadas?',
                           choices = c('Média', 'Desvio-Padrão','Moda', 'Frequência relativa'), multiple = TRUE),
               DT::dataTableOutput('tabela_exs')
               
               
               
        )
      )
    }
    
    else if (numero_exercicio == 6) {
      fluidRow(
        column(4,
               p(HTML(enunciado)),
               gt_output('plot_ex6'),
               render_gt(plot_ex6)
        ),
        column(8,
               h3('Seleção de variáveis'),
               
               pickerInput('teste_ex6', 'Qual seria o  teste estatístico mais adequado para verificação da hipótese?',
                           choices = c('Teste t de Student', 'Teste Qui-quadrado de Pearson', 'ANOVA')),
               uiOutput("hipotese_ui_6"),
               verbatimTextOutput('resultado_teste_ex_6'),
               actionButton('calcular_teste', 'Calcular')
               
               
        )
      )
    }
    
    else if (numero_exercicio == 7) {
      fluidRow(
        column(4,
               p(HTML(enunciado)),
               render_gt(plot_ex7)),
        column(8,
               h3('Seleção de variáveis'),
               
               pickerInput('teste_ex7', 'Qual seria o  teste estatístico mais adequado para verificação da hipótese?',
                           choices = c('Teste t de Student', 'Teste Qui-quadrado de Pearson', 'ANOVA')),
               uiOutput("hipotese_ui_7"),
               verbatimTextOutput('resultado_teste_ex_7'),
               actionButton('calcular_teste', 'Calcular')
               
               
        )
      )
    }
    
    else {
      fluidRow(
        column(12,
               p(enunciado)
        )
      )
    }
  } else {
    NULL
  }
}

output$hipotese_ui_6 <- renderUI({
  if(input$teste_ex6 == 'Teste Qui-quadrado de Pearson' && input$calcular_teste > 0 && length(calculo_teste()) == 0) {
    pickerInput('hipotese_ex6', 'A perda auditiva está associada à Paralisia Cerebral?',
                choices = c('Sim', 'Não'), selected = 'Sim')
  }
  
  else {
    NULL
  }
})

output$hipotese_ui_7 <- renderUI({
  if(input$teste_ex7 == 'Teste Qui-quadrado de Pearson' && input$calcular_teste > 0 && length(calculo_teste()) == 0) {
    pickerInput('hipotese_ex7', 'A perda auditiva está associada à Paralisia Cerebral?',
                choices = c('Sim', 'Não'), selected = 'Sim')
  }
  else {
    NULL
  }
})


output$resultado_teste_ex_6 <- renderPrint({
  if(input$teste_ex6 == 'Teste Qui-quadrado de Pearson' && input$calcular_teste > 0 && length(calculo_teste()) == 0) {
    chisq.test(dados_paralisia$perda_audit, dados_paralisia$grupo)
  }
})

output$resultado_teste_ex_7 <- renderPrint({
  if(input$teste_ex7 == 'Teste Qui-quadrado de Pearson' && input$calcular_teste > 0 && length(calculo_teste()) == 0) {
    chisq.test(dados_paralisia$dist_comun, dados_paralisia$grupo)
  }
})







validar_opcoes_ex <- reactive({
  msgs_erro <- character(0)
  
  req(!is.null(input$exercicio_pc))  
  
  # Verificar se as variáveis estão sendo definidas corretamente
  
  
  if (!is.null(input$variavel_pc_x) && !is.null(input$variavel_pc_y) && !is.null(input$tipo_grafico_pc) && input$exercicio_pc %in% c(2, 3, 4, 8, 9, 10, 11, 12)) {
    ex <- input$exercicio_pc
    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
    
    # Verificar se resposta_esperada está sendo definido corretamente
    
    if (input$variavel_pc_x != resposta_esperada$variavel_pc_x) {
      msgs_erro <- c(msgs_erro, "Essa não é a variável correta para o eixo x")
    }
    
    else if (input$variavel_pc_y != resposta_esperada$variavel_pc_y) {
      msgs_erro <- c(msgs_erro, "Essa não é a variável correta para o eixo y")
    }
    
    else if (input$tipo_grafico_pc != resposta_esperada$tipo_grafico_pc) {
      msgs_erro <- c(msgs_erro, "Esse não é o tipo de gráfico correto")
    }
    
    else if (input$variavel_pc_x == input$variavel_pc_y) {
      msgs_erro <- c(msgs_erro, 'As variáveis não podem ser iguais')
    }
  }
  
  
  
  else if (input$exercicio_pc == 5){
    ex <- input$exercicio_pc
    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
    
    
    if (!identical(input$medidas_resumo_ex5, resposta_esperada$medidas_resumo_ex5)) {
      msgs_erro <- c(msgs_erro, "Não foram selecionadas somente as medidas-resumo adequadas")
    }
    
    
    
  }
  
  else if (input$exercicio_pc == 6){
    ex <- input$exercicio_pc
    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
    
    if(!identical(input$hipotese_ex6, resposta_esperada$hipotese_ex6)){
      msgs_erro <- c(msgs_erro, 'Essa não é a conclusão correta')
    }
  }
  
  else if (input$exercicio_pc == 7){
    ex <- input$exercicio_pc
    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
    
    if(!identical(input$hipotese_ex7, resposta_esperada$hipotese_ex7)){
      msgs_erro <- c(msgs_erro, 'Essa não é a conclusão correta')
    }
  }
  
  
  else if (length(msgs_erro) > 0) {
    return(msgs_erro)
  } else {
    return(NULL)
  }
})


# Renderizar shinyalert
observeEvent(input$gerar_pc, {
  if (!is.null(validar_opcoes_ex())) {
    shinyalert(title = "Erro!", text = validar_opcoes_ex(), type = "error")
  }
})


# Renderizar testes estatísticos
calculo_teste <- reactive({
  msgs_erro_teste <- character(0)
  req(!is.null(input$exercicio_pc))
  
  if(input$exercicio_pc == 6){
    ex <- input$exercicio_pc
    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
    if (!identical(input$teste_ex6, resposta_esperada$teste_ex6)) {
      msgs_erro <- c(msgs_erro_teste, "Este não é o teste adequado")
    }
  }
  
  if(input$exercicio_pc == 7){
    ex <- input$exercicio_pc
    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
    if (!identical(input$teste_ex7, resposta_esperada$teste_ex7)) {
      msgs_erro <- c(msgs_erro_teste, "Este não é o teste adequado")
    }
  }
  
})

observeEvent(input$calcular_teste, {
  if(!is.null(calculo_teste())){
    shinyalert(title = 'Erro!', text = calculo_teste(), type = 'error')
  }
})



output$grafico_pc <- renderPlot({
  
  if(input$exercicio_pc %in% c(2, 3, 4, 8, 9, 10, 11, 12)){
    
    if ( is.null(validar_opcoes_ex())) {
      exercicio_selecionado <- input$exercicio_pc
      resposta_esperada <- respostas_esperadas_pc[[paste0("ex", exercicio_selecionado)]]
      
      
      
      
      if (exercicio_selecionado == 2){
        plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_pc_x, fill = input$variavel_pc_x ))+
          geom_bar(stat = 'count')
        req(input$gerar_pc)
        return(plot)
      }
      
      if (exercicio_selecionado == 3){
        plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_pc_x, fill = input$variavel_pc_x ))+
          geom_bar(stat = 'count')
        req(input$gerar_pc)
        return(plot)
      }
      
      if (exercicio_selecionado == 4){
        plot <- ggplot(dados_paralisia, aes_string(x = 1, y = input$variavel_pc_y))+
          geom_boxplot()
        req(input$gerar_pc)
        return(plot)
      }
      
      
      if (resposta_esperada$tipo_grafico_pc == 'Boxplot') {
        plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_pc_x, y = input$variavel_pc_y)) +
          geom_boxplot()
        req(input$gerar_pc)
        return(plot)
        
      }
      
      else if (resposta_esperada$tipo_grafico_pc == 'Dispersão') {
        plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_pc_x, y = input$variavel_pc_y)) +
          geom_point()
        req(input$gerar_pc)
        return(plot)
        
      } else if (resposta_esperada$tipo_grafico_pc == 'Barras') {
        plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_pc_x, y = input$variavel_pc_y)) +
          geom_bar()
        req(input$gerar_pc)
        return(plot)
        
      } else {
        plot <- NULL
        
        
      }
      
      return(NULL)
      
    } 
  }
})



output$tabela_exs <- DT::renderDataTable({
  
  if ( is.null(validar_opcoes_ex())) {
    exercicio_selecionado <- input$exercicio_pc
    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", exercicio_selecionado)]]
    
    if (exercicio_selecionado == 1) {
      
      plot <- dados_paralisia %>%
        tbl_summary(
          missing = 'no',
          digits = 
            list(all_continuous() ~ 1,
                 all_categorical() ~ c(0, 2)),
          statistic = list(all_continuous() ~ "{mean} ({sd})",
                           all_categorical() ~ "{n} ({p}%)")
        ) %>%
        modify_header(label = '**Variável') %>%
        modify_footnote(update = starts_with('stat_') ~ 'Média (desvio-padrão) para variáveis numéricas; n (%) para variáveis categóricas')
      
      # Converter para formato DT
      plot_dt <- datatable(
        plot$table_body,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE
        )
      )
      
      return(plot_dt)
    }
    
    if (exercicio_selecionado == 5) {
      
      plot <- dados_paralisia %>%
        select(td_liquido, td_pastoso, td_solido, grupo) %>%
        tbl_summary(
          by = 'grupo',
          missing = 'no',
          digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 2)),
          statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p})%")
        ) %>%
        modify_header(label = '**Variável**') %>%
        modify_footnote(update = starts_with('stat_') ~ 'Média (desvio-padrão) para')
      
      # Mantendo apenas as colunas desejadas
      plot_dt <- datatable(
        plot$table_body,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE
        )
      )
      
      return(plot_dt)
      
    }
    
  }
  
})















}







counter_ex9 <- 0
numero_do_exercicio <- 0
ex <- 0

useShinyjs()
paralisia <-
  tabItem(tabName = 'paralisia',
          fluidPage(
            fluidRow(
              wellPanel(titlePanel(h3("Exercícios Práticos - Paralisia Cerebral")),
                        HTML("<p>Segundo as <a href= 'https://bvsms.saude.gov.br/bvs/publicacoes/diretrizes_atencao_paralisia_cerebral.pdf'> Diretrizes de Atenção à
              Pessoa com Paralisia Cerebral do Ministério da Saúde (2014)</a>,
             a paralisia cerebral (PC) descreve um grupo de desordens da desenvolução do movimento e postura atribuído
             à distúrbio não progressivo durante o desenvolvimento do cérebro fetal ou infantil.
             <br>
             <p>Estima-se que nos países em desenvolvimento, 7 a cada 1000 nascidos vivos
             sejam acometidos por PC, em diferentes graus de comprometimento dos movimentos e postura.
             <br>
             <p>Para fins didáticos, algumas informações fornecidas pelo Ministério da Saúde e achados de <a href= 'https://www.scielo.br/j/rboto/a/hvQv9C6JY9h7MbrrSY98WpB/abstract/?lang=pt'> Aurélio et al.
             (2002)</a> na comparação do padrão de deglutição de alimentos entre crianças com PC e crianças sem acometimentos neurológicos
             (SAN), Curitiba/PR, encontram-se simulados no banco de dados tratado neste problema.
             <br>
             <br>
             <p>Existem 9 variáveis tratadas besse banco de dados, sendo elas:
             <br>
             <p><b>Sexo<b>: menino ou menina
             <p>Idade: idade em anos completos
             <p>Grupo: grupo das crianças por condição de saúde (SAN ou PC)
             <p>Perda auditiva: existência ou não de perda auditiva
             <p>Distúrbio de Comunicação: existência ou não de distúrbio de comunicação
             <p>DMO: grau de DMO (disfunção motora oral, compreendido entre 0 e 4)
             <p>Tempo líquido: tempo, em segundos, para deglutição de 100 ml de suco de laranja
             <p>Tempo pastoso: tempo, em segundos, para deglutição de 140 g de iogurte de morango homogêneo
             e sem pedaços de fruta
             <p>Tempo sólido: tempo, em segundos, para deglutição de 12 g de bolacha recheada de chocolate"
                        ),
                        selectInput('exercicio_pc','Escolha o exercício a ser resolvido', choices = 1:12 )
                        
              )
            ),
            
            
            mainPanel(
              tabsetPanel(
                tabPanel('Ex1', uiOutput('ex1')),
                tabPanel("Ex2", uiOutput('ex2')),
                tabPanel('Ex3', uiOutput('ex3')),
                tabPanel('Ex4', uiOutput('ex4')),
                tabPanel('Ex5', uiOutput('ex5')),
                tabPanel('Ex6', uiOutput('ex6')),
                tabPanel('Ex7', uiOutput('ex7'))
              ),
              
            )
            
            
            
            
          )
  )







counter_ex9 <- 0
useShinyjs()
paralisia <-
  tabItem(tabName = 'paralisia',
          fluidPage(
            fluidRow(
              HTML('<center><img src="images/garu_3.png"></center>')),
            sidebarLayout(
              sidebarPanel(
                selectInput('exercicio_pc','Escolha o exercício a ser resolvido', choices = 1:12 ),
                titlePanel(h3("Exercícios Práticos - Paralisia Cerebral")),
                HTML("<p>Segundo as <a href= 'https://bvsms.saude.gov.br/bvs/publicacoes/diretrizes_atencao_paralisia_cerebral.pdf'> Diretrizes de Atenção à
              Pessoa com Paralisia Cerebral do Ministério da Saúde (2014)</a>,
             a paralisia cerebral (PC) descreve um grupo de desordens da desenvolução do movimento e postura atribuído
             à distúrbio não progressivo durante o desenvolvimento do cérebro fetal ou infantil.
             <br>
             <p>Estima-se que nos países em desenvolvimento, 7 a cada 1000 nascidos vivos
             sejam acometidos por PC, em diferentes graus de comprometimento dos movimentos e postura.
             <br>
             <p>Para fins didáticos, algumas informações fornecidas pelo Ministério da Saúde e achados de <a href= 'https://www.scielo.br/j/rboto/a/hvQv9C6JY9h7MbrrSY98WpB/abstract/?lang=pt'> Aurélio et al.
             (2002)</a> na comparação do padrão de deglutição de alimentos entre crianças com PC e crianças sem acometimentos neurológicos
             (SAN), Curitiba/PR, encontram-se simulados no banco de dados tratado neste problema.
             <br>
             <br>
             <p>Existem 9 variáveis tratadas besse banco de dados, sendo elas:
             <br>
             <p><b>Sexo<b>: menino ou menina
             <p>Idade: idade em anos completos
             <p>Grupo: grupo das crianças por condição de saúde (SAN ou PC)
             <p>Perda auditiva: existência ou não de perda auditiva
             <p>Distúrbio de Comunicação: existência ou não de distúrbio de comunicação
             <p>DMO: grau de DMO (disfunção motora oral, compreendido entre 0 e 4)
             <p>Tempo líquido: tempo, em segundos, para deglutição de 100 ml de suco de laranja
             <p>Tempo pastoso: tempo, em segundos, para deglutição de 140 g de iogurte de morango homogêneo
             e sem pedaços de fruta
             <p>Tempo sólido: tempo, em segundos, para deglutição de 12 g de bolacha recheada de chocolate"
                     
                )
              ),
              mainPanel(
                uiOutput('opcoes_exercicio_pc'),
                actionButton('gerar_pc', 'Gerar'),
                plotOutput('grafico_pc'),
                gt_output('tabela_ex1'),
                gt_output('plot_ex6')
                
              )
            )
          )
  )











