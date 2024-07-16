####09UI para exercício 9 ----
"output$ex9 <- renderUI({
    
    enunciado <- questoes_paralisia[[9]]
    
    fluidRow(
      h3(''),
      column(10,
             wellPanel(
               p(HTML(enunciado)),
               column(8,
                      pickerInput('variavel_ex9x', 'Eixo x',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput('variavel_ex9y', 'Eixo y',
                                  choices = c('Não se aplica', nomes_exibidos),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      pickerInput('variavel_ex9_graf', 'Tipo de gráfico',
                                  choices = c('Barras', 'Boxplot', 'Dispersão'),
                                  options = list(noneSelectedText = 'Nada selecionado')),
                      actionButton('graf_ex9', 'Gerar gráfico'),
                      plotOutput('plot_ex9'),
                      align = 'center'
               )
             )
      ),
      column(10,
             wellPanel(
               h5('<b>Testes de normalidade da variável quantitativa pelos níveis
             da variável qualitativa:</b>'),
               column(8,
                      verbatimTextOutput('resultado_teste_norm_ex9'),
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
               column(8,
                      uiOutput('ex9_parteb'),
                      uiOutput('resultado_teste_ex9'),
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
               column(8,
                      uiOutput('ex9_partec'),
                      align = 'center'
               )
             )
      )
    )
    
  })"

#Esse é o server do ex 9
observeEvent(input$graf_ex9, {
  output$plot_ex9 <- renderPlot({
    req(input$variavel_ex9x != input$variavel_ex9y, 
        (input$variavel_ex9y == 'grupo' || input$variavel_ex9y == 'td_liquido'),
        (input$variavel_ex9x == 'grupo' || input$variavel_ex9x == 'td_liquido'),
        input$variavel_ex9_graf == 'Boxplot')
    
    # Determine the labels based on the selected variables
    x_label <- if (input$variavel_ex9x == 'grupo') {
      'Grupo'
    } else {
      'Tempo Líquido'
    }
    
    y_label <- if (input$variavel_ex9y == 'td_liquido') {
      'Tempo Líquido'
    } else {
      'Grupo'
    }
    
    # Determine the fill aesthetic based on whether 'grupo' is in x or y
    fill_var <- if (input$variavel_ex9x == 'grupo') {
      input$variavel_ex9x
    } else if (input$variavel_ex9y == 'grupo') {
      input$variavel_ex9y
    } else {
      NULL
    }
    
    # Create the plot using ggplot2
    plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_ex9x, y = input$variavel_ex9y, fill = fill_var)) +
      geom_boxplot() +
      scale_fill_manual(values = c('#ffc20a','#0c7bdc')) +
      labs(x = x_label, y = y_label, fill = 'Grupo') +
      theme_minimal()
    
    return(plot)
  })
  
  
  
  
  mensagem <- reactive({
    if (is.null(input$variavel_ex9x) && is.null(input$variavel_ex9y) && is.null(input$variavel_ex9_graf)) {
      return('Você não selecionou as respostas!')}
    else {
      if (identical(input$variavel_ex9x, 'grupo') && identical(input$variavel_ex9y, 'td_liquido') && identical(input$variavel_ex9_graf, 'Boxplot')) {
        return('Resposta correta.')}
      if (identical(input$variavel_ex9x, 'td_liquido') && identical(input$variavel_ex9y, 'grupo') && identical(input$variavel_ex9_graf, 'Boxplot')) {
        return('Resposta correta.')}
      else {
        return('Há algo errado com sua seleção.')}}
  })
  
  shinyalert(
    title = "",
    text = mensagem(),
    type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
  )},
  ignoreNULL = T)

output$ex9_parteb <- renderUI({
  req(input$variavel_ex9x == 'grupo' || input$variavel_ex9x == 'td_liquido')
  req(input$variavel_ex9y == 'grupo' || input$variavel_ex9y == 'td_liquido')
  req(input$variavel_ex9_graf == 'Boxplot')
  req(input$variavel_ex9x != input$variavel_ex9y)
  req(input$graf_ex9)
  fluidRow(
    pickerInput('teste_ex9', 'Escolha o teste:',
                choices = c('Mann-Whitney', 'Qui-Quadrado', 't de Student')),
    actionButton('verificar_teste_ex9', 'Verificar')
  )
  
  
  
  
  
})

observeEvent(input$graf_ex9, {
  req(input$variavel_ex9x == 'grupo' || input$variavel_ex9x == 'td_liquido')
  req(input$variavel_ex9y == 'grupo' || input$variavel_ex9y == 'td_liquido')
  req(input$variavel_ex9_graf == 'Boxplot')
  req(input$variavel_ex9x != input$variavel_ex9y)
  req(input$graf_ex9)
  output$resultado_teste_norm_ex9 <- renderPrint({
    shapiro_san <- shapiro.test(SAN$td_liquido)
    shapiro_pc <- shapiro.test(PC$td_liquido)
    
    # Using cat() to ensure line breaks are correctly interpreted
    cat('p-valor do teste de Shapiro-Wilk para o grupo SAN:\n', shapiro_san$p.value, '\n',
        'p-valor do teste de Shapiro-Wilk para o grupo PC:\n', shapiro_pc$p.value)
  })
  
})

SAN <- filter(dados_paralisia, grupo == 'SAN')
PC <- filter(dados_paralisia, grupo == 'PC')


observeEvent(input$verificar_teste_ex9, {
  
  if(input$teste_ex9 == 'Mann-Whitney'){
    output$resultado_teste_ex9 <- renderPrint({
      wilcox_ex9 <- wilcox.test(td_liquido ~ grupo, alternative = 'two.sided',
                                conf.level = 0.95, data = dados_paralisia)
      
      cat('p-valor do teste:', wilcox_ex9$p.value)
      
    })
  }
  
  mensagem <- reactive({
    if(input$teste_ex9 == 'Mann-Whitney'){
      return('Resposta correta.')
    }
    else{
      if(is.null(input$teste_ex9)){
        return('Você não selecionou as respostas')}
      else{
        return('Há algo errado com sua seleção.')}
    }
  })
  shinyalert(
    title = '',
    text = mensagem(),
    type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
  )}
  
)

output$ex9_partec <- renderUI({
  req(input$verificar_teste_ex9)
  if(input$teste_ex9 == 'Mann-Whitney'){
    fluidRow(
      pickerInput('relacao_ex9', 'Escolha sua resposta:',
                  choices = c('Sim', 'Não')),
      actionButton('verificar_relacao_ex9', 'Verificar')
      
    )
  }
})

observeEvent(input$verificar_relacao_ex9, {
  mensagem <- reactive({
    if(input$relacao_ex9 == 'Sim'){
      return('Resposta correta.')
    }
    else{
      return('Há algo errado com sua seleção')
    }
  })
  shinyalert(
    title = '',
    text = mensagem(),
    type = ifelse(mensagem() == 'Resposta correta.', 'success', 'warning')
  )
})