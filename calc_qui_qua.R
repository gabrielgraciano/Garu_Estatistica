library(shiny)
library(rhandsontable)

#ui
calc_qui_qua_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$head(
      tags$style(HTML("
        .handsontable-container {
          width: 100% !important;
          height: auto !important;
          overflow: hidden !important; /* Oculta a barra de rolagem */
        }
        .rhandsontable {
          width: 100% !important;
          overflow: hidden !important; /* Oculta a barra de rolagem */
        }
      "))
    ),
    titlePanel("Calculadora de Qui-Quadrado"),
    
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("ncol"), "Número de colunas:", 2, min = 2, max = 10),
        numericInput(ns("nrow"), "Número de linhas:", 2, min = 2, max = 10),
        numericInput(ns("conf_level"), "Nível de confiança (%):", 
                     95, min = 90, max = 99),
        actionButton(ns("calculate"), "Calcular"),
        uiOutput(ns("montecarlo_options")) 
      ),
      
      mainPanel(
        h4("Tabela Observada"),
        h5("Clique nas células da tabela observada para editar os valores."),
        rHandsontableOutput(ns("obs_table_ht"), height = "auto"),
        br(),  
        h4("Tabela Esperada"),
        rHandsontableOutput(ns("exp_table_ht"), height = "auto"),
        br(),  
        h4("Resultado do Teste Qui-Quadrado"),
        verbatimTextOutput(ns("test_result"))
      )
    )
  )
}

#server
calc_qui_qua_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(data = NULL, 
                         rownames = NULL, 
                         colnames = NULL, 
                         test_result = NULL, 
                         montecarlo = FALSE, 
                         num_simulations = NULL)
    
   #Fornecendo dados relativos ao número de linhas e colunas da tabela
    observe({
      req(input$nrow, input$ncol)
      rv$rownames <- paste0("Linha ", 1:input$nrow)
      rv$colnames <- paste0("Coluna ", 1:input$ncol)
      rv$data <- matrix(0, 
                        nrow = input$nrow, 
                        ncol = input$ncol,
                        dimnames = list(rv$rownames, rv$colnames))
    })
    
    #utiliza o handsontable para renderizar a tabela, oculta a barra de rolagem e ajusta o tamanho das células dinamicamente
    output$obs_table_ht <- renderRHandsontable({
      req(rv$data, rv$rownames, rv$colnames)
      num_cells <- input$nrow * input$ncol
      cell_size <- ifelse(num_cells < 20, "150px", "100px") 
      rhandsontable(rv$data, rowHeaders = rv$rownames, colHeaders = rv$colnames) %>%
        hot_cols(colWidths = cell_size) %>%
        hot_context_menu(allowColEdit = TRUE, allowRowEdit = TRUE)
    })
    
    #atualização da tabela a partir da edição de uma célula
    observeEvent(input$obs_table_ht, {
      hot_data <- hot_to_r(input$obs_table_ht)
      rv$data <- hot_data
    })
    
    #cálculo do teste qui-quadrado
    observeEvent(input$calculate, {
      req(rv$data)
      data <- rv$data
      
      rownames(data) <- rv$rownames
      colnames(data) <- rv$colnames
      
      #observa as frequencias para definir se utilizará mcmc ou não
      freq_menor_que_5 <- any(data < 5)
      rv$montecarlo <- freq_menor_que_5 
      
      if (freq_menor_que_5) {
        rv$test_result <- "Uma ou mais frequências observadas são menores que 5. Recomenda-se o uso da simulação de Monte Carlo para obter resultados mais precisos."
        
        #ui do monte carlo
        output$montecarlo_options <- renderUI({
          tagList(
            numericInput(ns("num_simulations"), 
                         "Número de Simulações:", 
                         2000, 
                         min = 100,
                         max = 10000),
            actionButton(ns("apply_montecarlo_btn"), "Aplicar Monte Carlo")
          )
        })
        
      } else {
        #execução do teste
        test <- tryCatch({
          chisq.test(data,
                     correct = FALSE,
                     simulate.p.value = FALSE)
        }, warning = function(w) {
          w
        }, error = function(e) {
          e
        })
        
        rv$test_result <- test
        
        #renderização da tabela esperada (não editável) e com tamanho de células atualizado e igual às frequências observadas
        output$exp_table_ht <- renderRHandsontable({
          if (inherits(test, "htest")) {
            num_cells <- input$nrow * input$ncol
            cell_size <- ifelse(num_cells < 20, "150px", "100px") 
            rhandsontable(test$expected, 
                          rowHeaders = rv$rownames, 
                          colHeaders = rv$colnames, 
                          readOnly = TRUE) %>%
              hot_cols(colWidths = cell_size)
          } else {
            showNotification("Erro ao calcular a tabela esperada.", type = "error")
            NULL
          }
        })
        
        #Resultado do teste com saída personalizada para facilitar leitura
        output$test_result <- renderPrint({
          if (inherits(test, "htest")) {
            cat("Teste Qui-quadrado de Pearson\n")
            cat("Valor do teste (estatística do teste):", format(test$statistic, digits = 4, nsmall = 4), "\n")
            cat("Graus de liberdade (df):", test$parameter, "\n")
            cat("p-valor:", format(test$p.value, digits = 4, nsmall = 4), "\n")
          } else {
            "Erro ao realizar o teste qui-quadrado. Verifique se os dados inseridos são adequados."
          }
        })
        
        #Esconde a ui do monte carlo
        output$montecarlo_options <- renderUI(NULL)
      }
    })
    
    #teste qui-quadrado com simulação de monte carlo
    observeEvent(input$apply_montecarlo_btn, {
      req(rv$test_result)
      data <- rv$data
      
      rownames(data) <- rv$rownames
      colnames(data) <- rv$colnames
      rv$num_simulations <- input$num_simulations 
      
      test <- tryCatch({
        chisq.test(data, 
                   correct = FALSE,
                   simulate.p.value = TRUE,
                   B = rv$num_simulations)
      }, warning = function(w) {
        w
      }, error = function(e) {
        e
      })
      
      rv$test_result <- test
      
      #Renderização da tabela esperada
      output$exp_table_ht <- renderRHandsontable({
        if (inherits(test, "htest")) {
          num_cells <- input$nrow * input$ncol
          cell_size <- ifelse(num_cells < 20, "150px", "100px")
          rhandsontable(test$expected, 
                        rowHeaders = rv$rownames, 
                        colHeaders = rv$colnames, 
                        readOnly = TRUE) %>%
            hot_cols(colWidths = cell_size)
        } else {
          showNotification("Erro ao calcular a tabela esperada.", type = "error")
          NULL
        }
      })
      
      #saída do teste com simulação de monte carlo
      output$test_result <- renderPrint({
        if (inherits(test, "htest")) {
          cat("Teste Qui-quadrado de Pearson com simulação de Monte Carlo (", rv$num_simulations, " vezes)\n", sep = "")
          cat("Valor do teste (estatística do teste):", format((test$statistic), digits = 4, nsmall = 4), "\n")
          cat("Graus de liberdade (df):", "NA (Monte Carlo)", "\n")
          cat("p-valor:", format(test$p.value, digits = 4, nsmall = 4), "\n")
        } else {
          "Erro ao realizar o teste qui-quadrado com Monte Carlo. Verifique se os dados inseridos são adequados."
        }
      })
    })
  })
}


