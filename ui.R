source("inicio_projeto.R")
source("inicio_equipe.R")
source("inicio_contato.R")
source('conjunto_dados.R')
source("tipos_variaveis.R")
source("tabela_frequencias.R")
source("medidas_resumo.R")
source('graficos.R')
source('distr_prob.R')
source("inferencia.R") 
source("calc_qui_qua.R")
source('exerc_teoricos.R')
source('exerc_paralisia.R')
source('ref_biblio.R')
options(OutDec = ",")

dashboardPage(
  
  dashboardHeader(title = HTML('Garu Estatística'),
                  tags$li(class = "dropdown", 
                          tags$a(href = "javascript:fakeClick('inicio_equipe')", 
                                 icon("users", lib = "font-awesome"), 
                                 "Equipe")),
                  tags$li(class = "dropdown", 
                          tags$a(href = "javascript:fakeClick('inicio_contato')", 
                                 icon("envelope", lib = "font-awesome"), 
                                 "Contato"))
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem('O projeto', 
               tabName = 'inicio_projeto', 
               icon = icon("project-diagram", 
                           lib = "font-awesome")),
      menuItem('Equipe', 
               tabName = 'inicio_equipe', 
               icon = icon("users", lib = "font-awesome")),
      menuItem('Contato', 
               tabName = 'inicio_contato', 
               icon = icon("envelope", lib = "font-awesome")),
      
      menuItem('Conjuntos de dados', 
               tabName = 'conjunto_dados', 
               icon = icon('database', 
                           lib = 'font-awesome')),
      
      menuItem('Descritiva', icon = icon("table", 
                                         lib = "font-awesome"),
               menuSubItem('Tipos de Variáveis', 
                           tabName = 'tipos_variaveis'),
               menuSubItem('Tabela de Frequências',
                           tabName = 'tabela_frequencias'),
               menuSubItem('Medidas Resumo', 
                           tabName = 'medidas_resumo')),
      
      menuItem('Gráficos', icon = icon("pie-chart", 
                                       lib="font-awesome"),
               menuSubItem('Variáveis Qualitativas',
                           tabName = 'graf_qualitativa'),
               menuSubItem('Variáveis Quantitativas', 
                           tabName = 'graf_quantitativa'),
               menuSubItem('Gráficos Bidimensionais', 
                           tabName = 'graf_bidimensional')),
      
      menuItem('Probabilidade', icon = icon("dice", 
                                            lib="font-awesome"),
               menuSubItem('Distribuições', tabName = 'distr_prob')),
      
      menuItem('Inferência', icon = icon('chart-area', 
                                         lib= 'font-awesome'),
               menuSubItem('Teste T para uma amostra', 
                           tabName = 'teste_t_1'),
               menuSubItem('Teste Qui-quadrado',
                           tabName = 'teste_qui'),
               menuSubItem('Teste de Correlação', 
                           tabName= 'teste_corr'),
               menuSubItem('Calculadora Qui-quadrado', 
                           tabName= 'calc_qui_qua')),
      
      
      menuItem('Exercícios', icon = icon("pencil", 
                                         lib="font-awesome"),
               menuSubItem('Exercícios Teóricos', 
                           tabName = 'exerc_teoricos'),
               menuSubItem('Exercícios Práticos',
                           tabName = 'exerc_paralisia')),
      
      menuItem('Referências', 
               tabName = 'ref_biblio', 
               icon = icon("book", 
                           lib = "font-awesome"))
      
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      ')),
      tags$meta(property = "og:title", 
                content = "Garu Estatística"),
      tags$meta(property = "og:description", 
      content = "Uma ferramenta para ensino de Estatística"),
      tags$meta(property = "og:image", 
                content = "https://garuestatistica.unifesp.br/images/garu_3.png"),
      tags$meta(property = "og:url", 
                content = "https://garuestatistica.unifesp.br/"),
      tags$meta(property = "og:type", 
                content = "website")
    ),
    tabItems(
      tabItem(tabName = 'inicio_projeto', inicio_projeto_ui("inicio_projeto")),
      tabItem(tabName = 'inicio_equipe', inicio_equipe_ui("inicio_equipe")),
      tabItem(tabName = 'inicio_contato', inicio_contato_ui("inicio_contato")),
      conjunto_dados,
      tipos_variaveis,
      tabela_frequencias,
      medidas_resumo,
      graf_qualitativa,
      graf_quantitativa,
      graf_bidimensional,
      distr_prob,
      teste_t_1,
      teste_qui,
      teste_corr,
      tabItem(tabName = 'calc_qui_qua', calc_qui_qua_ui("calc_qui_qua")),
      tabItem(tabName = 'exerc_teoricos', exerc_teoricos_ui("exerc_teoricos")),
      tabItem(tabName = 'exerc_paralisia', exerc_paralisia_ui("exerc_paralisia")),
      tabItem(tabName = 'ref_biblio', ref_biblio_ui("ref"))
    )
  )
)
