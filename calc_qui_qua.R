calc_qui_qua_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Calculadora de Qui-Quadrado"),
    
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("ncol"), "Número de colunas:", 2, min = 2, max = 10),
        numericInput(ns("nrow"), "Número de linhas:", 2, min = 2, max = 10),
        numericInput(ns("conf_level"), "Nível de confiança (%):", 
                     95, min = 90, max = 99),
        
        uiOutput(ns("table_setup")),
        
        actionButton(ns("calculate"), "Calcular"),
        uiOutput(ns("montecarlo_options"))
      ),
      
      mainPanel(
        h4("Tabela Observada"),
        h5("Após selecionar número de colunas e linhas, clique nas células da 
        tabela observada para incluir os valores."),
        DTOutput(ns("obs_table_dt")),
        
        h4("Tabela Esperada"),
        tableOutput(ns("exp_table")),
        
        h4("Resultado do Teste Qui-Quadrado"),
        verbatimTextOutput(ns("test_result"))
      )
    )
  )
}

calc_qui_qua_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$table_setup <- renderUI({
      req(input$nrow, input$ncol)
      tagList(
        textInput(ns("rownames"),
                  HTML("Nomes das linhas<br>(separados por vírgulas):"), 
                  paste0("Linha ", 1:input$nrow, collapse = ",")),
        textInput(ns("colnames"), 
                  HTML("Nomes das colunas <br>(separados por vírgulas):"), 
                  paste0("Coluna ", 1:input$ncol, collapse = ","))
      )
    })
    
    rv <- reactiveValues(data = NULL, 
                         rownames = NULL, 
                         colnames = NULL, 
                         test_result = NULL)
    
    observe({
      req(input$nrow, input$ncol)
      rv$rownames <- paste0("Linha ", 1:input$nrow)
      rv$colnames <- c(NULL, paste0("Coluna ", 1:input$ncol))
      rv$data <- matrix(0, 
                        nrow = input$nrow, 
                        ncol = input$ncol,
                        dimnames = list(rv$rownames, rv$colnames)
                        )
    })
    
    observe({
      req(input$rownames, input$colnames)
      rv$rownames <- unlist(strsplit(input$rownames, ","))
      rv$colnames <- unlist(strsplit(input$colnames, ","))
    })
    
    output$obs_table_dt <- renderDT({
      req(rv$data, rv$rownames, rv$colnames)
      datatable(rv$data, 
                editable = TRUE, 
                rownames = rv$rownames,
                colnames = c(NULL, rv$colnames),
                options = list(dom = 't',
                               pageLength = 10, 
                               scrollX = TRUE,
                               autoWidth = TRUE,
                               columnDefs = list(
                                 list(className = 'dt-center', targets = "_all"),
                                 list(width = '100px', targets = "_all"))
                               )
                )
    })
    
    observeEvent(input$obs_table_dt_cell_edit, {
      info <- input$obs_table_dt_cell_edit
      i <- info$row
      j <- info$col
      
      v <- as.numeric(info$value)
      
      if (!is.na(v) & i <= nrow(rv$data) & j <= ncol(rv$data)) {
        rv$data[i, j] <- v
      } else {
        showNotification("Por favor, insira um valor numérico válido.", 
                         type = "error")
      }
    })
    
    observeEvent(input$calculate, {
      req(rv$data)
      data <- rv$data
      
      rownames(data) <- rv$rownames
      colnames(data) <- rv$colnames
      
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
      
      output$exp_table <- renderTable({
        if (inherits(test, "htest")) {
          test$expected
        } else {
          "Tabela de valores esperados não pôde ser calculada devido a um erro."
        }
      }, rownames = TRUE)
      
      output$test_result <- renderPrint({
        if (inherits(test, "htest")) {
          test
        } else {
          "Erro ao realizar o teste qui-quadrado. Verifique se os dados inseridos são adequados."
        }
      })
      
      if (!inherits(test, "htest")) {
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
        output$montecarlo_options <- renderUI(NULL)
      }
      
      # Força a atualização da tabela
      outputOptions(output, "obs_table_dt", suspendWhenHidden = FALSE)
    })
    
    observeEvent(input$apply_montecarlo_btn, {
      req(rv$test_result)
      data <- rv$data
      
      rownames(data) <- rv$rownames
      colnames(data) <- rv$colnames
      
      test <- tryCatch({
        chisq.test(data, 
                   correct = FALSE,
                   simulate.p.value = TRUE,
                   B = input$num_simulations)
      }, warning = function(w) {
        w
      }, error = function(e) {
        e
      })
      
      rv$test_result <- test
      
      output$exp_table <- renderTable({
        if (inherits(test, "htest")) {
          test$expected
        } else {
          "Tabela de valores esperados não pôde ser calculada devido a um erro."
        }
      }, rownames = TRUE)
      
      output$test_result <- renderPrint({
        if (inherits(test, "htest")) {
          test
        } else {
          "Erro ao realizar o teste qui-quadrado. Verifique se os dados inseridos são adequados."
        }
      })
    })
  })
}

