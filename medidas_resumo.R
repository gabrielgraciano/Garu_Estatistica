medidas_resumo <-
  
  tabItem(tabName = 'medidas_resumo',
          
          fluidPage(
            tags$head(
              tags$style(HTML("
                .btn-primary {
                  background-color: white;
                  border-color: #007BFF;
                }
                p, h4 {
                  text-align: justify;
                }
              "))
            ),
            h3(strong('Medidas Resumo')),
            
            column(6,
                   h4(strong("Gerador de amostra de números")),
                   p("Gere uma amostra de números através dos botões 
                     'Valores entre' e 'Quantidade de elementos'. Depois, 
                     clique em 'Gerar amostra de números' e acompanhe como as medidas
                     resumo são calculadas para a amostra gerada."),
                   fluidRow(
                     sliderInput("slider_min_max_valor", 
                                 label = "Valores entre:", 
                                 min = 1, 
                                 max = 1000, 
                                 sep = ".",
                                 value = c(250, 750)),
                     align = 'center'
                   ),
                   
                   fluidRow(
                     sliderInput("slider_qtd_elementos", 
                                 label = "Quantidade de elementos", 
                                 min = 4, 
                                 max = 100, 
                                 value = 50),
                     align = 'center'
                   ),
                   
                   fluidRow(
                     actionButton("geraAmostras", 
                                  "Gerar amostra de números",
                                  class = "btn-primary"),
                     align = 'center'
                   ),
                   fluidRow(
                     h4(strong("Amostra gerada")),
                     tags$div(
                       style = "overflow-y: auto; height: 200px; 
                       border: 1px solid #ddd; padding: 15px; width: 100%;",
                       htmlOutput("printElementos")
                     )
                   )
                   
            ),
            column(6,
                   h4(strong("Medidas resumo da amostra gerada")),
                   
                   fluidRow(
                     column(6, 
                            wellPanel(
                              htmlOutput("minimoTitle"),
                              actionLink("minimoMostrarMais", "Mostrar mais"),
                              hidden(actionLink("minimoMostrarMenos", "Mostrar menos")),
                              hidden(p("Observação de menor valor.", id = "minimoTexto")),
                              hidden(helpText(id="htMinimo", "Exemplo com os elementos gerados:"),
                                     verbatimTextOutput("exMinimo"))
                            )
                     ),
                     column(6, 
                            wellPanel(
                              htmlOutput("maximoTitle"),
                              actionLink("maximoMostrarMais", "Mostrar mais"),
                              hidden(actionLink("maximoMostrarMenos", "Mostrar menos")),
                              hidden(p("Observação de maior valor.", id = "maximoTexto")),
                              hidden(helpText(id="htMaximo", "Exemplo com os elementos gerados:"),
                                     verbatimTextOutput("exMaximo"))
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(6, 
                            wellPanel(
                              htmlOutput("mediaTitle"),
                              actionLink("mediaMostrarMais", "Mostrar mais"),
                              hidden(actionLink("mediaMostrarMenos", "Mostrar menos")),
                              hidden(p("A média aritmética é a soma dos valores das 
                                      observações dividido pela quantidade de observações.", id = "mediaTexto")),
                              hidden(uiOutput("mediaExplain")),
                              hidden(helpText(id="htMedia",
                                              "Exemplo com os elementos gerados:"),
                                     verbatimTextOutput("exMedia"))
                            )
                     ),
                     column(6, 
                            wellPanel(
                              htmlOutput("quartilq1Title"),
                              actionLink("quartilq1MostrarMais",
                                         "Mostrar mais"),
                              hidden(actionLink("quartilq1MostrarMenos",
                                                "Mostrar menos")),
                              hidden(
                                p("Os quartis dividem o conjunto de observações 
                              em quatro partes iguais.
                                A observação que deixa 25% dos dados abaixo dela 
                                é chamada de primeiro quartil.",
                                  id = "quartilq1Texto")),
                              hidden(uiOutput("quartilq1Explain")),
                              hidden(helpText(id="htQuartilq1",
                                              "Exemplo com os elementos gerados:"),
                                     verbatimTextOutput("exQuartilq1"))
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(6, 
                            wellPanel(
                              htmlOutput("medianaTitle"),
                              actionLink("medianaMostrarMais",
                                         "Mostrar mais"),
                              hidden(actionLink("medianaMostrarMenos", 
                                                "Mostrar menos")),
                              hidden(
                                p("Os quartis dividem o conjunto de observações 
                                em quatro partes iguais.
                                A mediana (ou 2º quartil) representa a 
                                observação que ocupa a 
                                posição central da lista 
                                  de observações, quando essa está ordenada.
                                  Quando o conjunto de observações
                                   possui um número ímpar de observações, então
                                   a mediana é 
                                o valor central dessa lista ordenada. 
                                Caso contrário, um modo de obtê-la é pela média 
                                dos dois valores centrais.",
                                  id = "medianaTexto")),
                              hidden(uiOutput("medianaExplain")),
                              hidden(helpText(id="htMediana",
                                              "Exemplo com os elementos gerados:"),
                                     verbatimTextOutput("exMediana"))
                            )
                     ),
                     column(6, 
                            wellPanel(
                              htmlOutput("quartilq3Title"),
                              actionLink("quartilq3MostrarMais",
                                         "Mostrar mais"),
                              hidden(actionLink("quartilq3MostrarMenos",
                                                "Mostrar menos")),
                              hidden(
                                p("Os quartis dividem o conjunto de observações 
                              em quatro partes iguais.
                                A observação que deixa 75% dos dados abaixo dela 
                                é chamada de terceiro quartil.",
                                  id = "quartilq3Texto")),
                              hidden(uiOutput("quartilq3Explain")),
                              hidden(helpText(id="htQuartilq3",
                                              "Exemplo com os elementos gerados:"),
                                     verbatimTextOutput("exQuartilq3"))
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(6, 
                            wellPanel(
                              htmlOutput("dist_quantilTitle"),
                              actionLink("dist_quantilMostrarMais", 
                                         "Mostrar mais"),
                              hidden(actionLink("dist_quantilMostrarMenos", 
                                                "Mostrar menos")),
                              hidden(
                              p("Como os quartis são divididos em quatro partes 
                              iguais, a distância entre qualquer quartil e seu 
                              sucessor ou antecessor terá o mesmo valor
                                               absoluto. Por exemplo: 
                                q2 - q1 = q3 - q2 (a distância entre o segundo 
                                quartil e o primeiro é a mesma que entre o terceiro 
                                quartil e o segundo.",
                                       id = "dist_quantilTexto")),
                              hidden(uiOutput("dist_quantilExplain")),
                              hidden(helpText(id="dist_htQuantil", 
                                              "Exemplo com os elementos gerados:"),
                                     verbatimTextOutput("dist_exQuantil"))
                            )
                     ),
                     column(6, 
                            wellPanel(
                              htmlOutput("varianciaTitle"),
                              actionLink("varianciaMostrarMais", "Mostrar mais"),
                              hidden(actionLink("varianciaMostrarMenos", "Mostrar menos")),
                              hidden(p("Medidas como média e mediana podem não nos trazer 
                                        informações suficientes sobre o conjunto de observações,
                                        pois são medidas de posição. Nestes casos,
                                        é interessente analisarmos medidas de dispersão,
                                        como a variância. Esta representa
                                        o quão distantes os dados estão de sua média. 
                                        Para isso, precisamos
                                        calcular a distância de cada elemento da média, 
                                        somar o quadrado dessas
                                        distâncias e dividir pelo número de observações.",
                                       id = "varianciaTexto")),
                              hidden(uiOutput("varianciaExplain")),
                              hidden(helpText(id="htVariancia", 
                                              "Exemplo com os elementos gerados:"),
                                     verbatimTextOutput("exVariancia"))
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(6, 
                            wellPanel(
                              htmlOutput("dpTitle"),
                              actionLink("dpMostrarMais",
                                         "Mostrar mais"),
                              hidden(actionLink("dpMostrarMenos", 
                                                "Mostrar menos")),
                              hidden(p("O desvio padrão também é uma medida clássica de dispersão. 
                                                 Em termos de cálculo, ele se dá pela raiz quadrada da variância do 
                                                 conjunto. A vantagem que ele apresenta sobre a variância é a possibilidade 
                                                 de uma interpretação direta, uma vez que ele está na mesma unidade que a variável 
                                                 (kg, m, cm, etc.).", 
                                       id = "dpTexto")),
                              hidden(uiOutput("dpExplain")),
                              hidden(helpText(id="htDp", "Exemplo com os elementos gerados:"),
                                     verbatimTextOutput("exDp"))
                            )
                     )
                   )
            ),
            fluidRow(
              h4(strong("Gráfico das medidas resumo da amostra gerada")),
              plotOutput("graficoElementos")
            )
          )
  )