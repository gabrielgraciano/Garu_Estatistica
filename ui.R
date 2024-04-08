library(shiny)
library(shinyjs)
library(gt)
library(gtsummary)
library(cowplot)
library(shinydashboard)
library(DT)
library(ggokabeito)
library(ggthemes)

source('data_cleaning_paralisia.R')
source("data_handling.R")
source("inicio.R")
source("tipos_variaveis.R")
source("tabela_frequencias.R")
source("medidas_resumo.R")
source('graf_qualitativa.R')
source('graf_quantitativa.R')
source('graf_bidimensional.R')
source("inferencia.R") 
source('questionario.R')
source('perguntaspibiti.R')
source('expraticos.R')
source('enunciados_pc.R')

useShinyalert(force=TRUE)

#Comcecei aqui
#substitui graficos.R por outras três sources
#substitui inferencia por outras quatro sources (ainda não troquei, talvez não precise)


dashboardPage(
    
  dashboardHeader(title = 'Garu Estatística'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Início', tabName = 'inicio', icon = icon("home", lib = "font-awesome")),
      
      menuItem('Descritiva', icon = icon("table", lib = "font-awesome"),
               menuSubItem('Tipos de Variáveis', tabName = 'tipos_variaveis'),
               menuSubItem('Tabela de Frequências', tabName = 'tabela_frequencias'),
               menuSubItem('Medidas Resumo', tabName = 'medidas_resumo')),
      
      menuItem('Gráficos', icon = icon("pie-chart", lib="font-awesome"),
               menuSubItem('Variáveis Qualitativas', tabName = 'graf_qualitativa'),
               menuSubItem('Variáveis Quantitativas', tabName = 'graf_quantitativa'),
               menuSubItem('Gráficos Bidimensionais', tabName = 'graf_bidimensional')),
      
      menuItem('Estatística Inferencial', icon = icon("magnifying-glass", lib = "font-awesome"),
               menuSubItem('Teste T para uma amostra', tabName = "teste_t_1"), 
               menuSubItem("Teste T para duas amostras (dep.)", tabName = "teste_t_2"),
               menuSubItem("Teste qui quadrado", tabName ="teste_qui"),
               menuSubItem("Teste de Correlação", tabName = "teste_corr")),
      
      menuItem('Exercícios Teóricos', icon = icon("pencil", lib="font-awesome"),
               menuSubItem('Exercícios', tabName = 'questionario')),
      
      menuItem('Exercícios práticos', icon = icon("magnifying-glass-chart", lib="font-awesome"),
               menuSubItem('Paralisia Cerebral', tabName = 'paralisia'))
      ),
      
      tags$div(
          style = "position: absolute; bottom: 0; width: 100%; background-color: #000; padding: 10px;",
          h5(HTML("Versão 1.0.6 <br> Última atualização: 08/04/2024"))
       )
    ),

  
  dashboardBody(
        
    tags$head(tags$script(HTML('
      var fakeClick = function(tabName) {
        var dropdownList = document.getElementsByTagName("a");
        for (var i = 0; i < dropdownList.length; i++) {
          var link = dropdownList[i];
          if(link.getAttribute("data-value") == tabName) {
            link.click();
          };
        }
      };
    '))),
    
    tabItems(
      inicio,
      tipos_variaveis,
      tabela_frequencias,
      medidas_resumo,
      graf_quantitativa,
      graf_qualitativa,
      graf_bidimensional,
      teste_t_1,
      teste_t_2,
      teste_qui,
      teste_corr,
      questionario,
      paralisia
    )
  )
  
)