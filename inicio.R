inicio <-
    tabItem(tabName = 'inicio',
            fluidPage(
                fluidRow(
                    HTML('<center><img src="images/garu_3.png"></center>')),
                fluidRow(column(6,
                                h3(strong('Funcionalidades')), br(),
        
                                strong("1. Estatística Descritiva"),
                                h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"tipos_variaveis\")'>1.1 Tipos de Variáveis</a>")),
                                h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"tabela_frequencias\")'>1.2 Distribuição de Frequências</a>")),
                                h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"medidas_resumo\")'>1.3 Medidas Resumo </a>")),  br(),
                                
                                strong("2. Gráficos"),
                                h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"graf_qualitativa\")'>2.1. Gráficos para variáveis qualitativas</a>")),
                                h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"graf_quantitativa\")'>2.2. Gráficos para variáveis quantitativas</a>")), 
                                h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"graf_bidimensional\")'>2.3 Gráficos Bidimensionais</a>")),  br(),
                                
                                strong("3. Exercícios Teóricos"),
                                h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"questionario\")'>3.1 Exercícios</a>")),  br(),
                                
                                strong("4. Exercícios Práticos"), 
                                h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"paralisia\")'>4.1 Paralisia Cerebral</a>")),  br(),
                                
                ),
                
                column(6, 
                       h3(strong('Equipe')),  br(),
                       
                       strong("Alunos participantes"), 
                       h5(HTML("<p>João Henrique de Araujo Morais e Gabriel Graciano Dias</p>")), br(),
                       
                    strong("Orientadores"), 
                       h5(HTML("<p> Profa. Dra. Camila Bertini Martins e Téc. Dra. Alessandra A. S. Menezes</p>")), br(),
                       
                          strong("Contato"), 
                       h5(HTML("<p><a> joao.morais@unifesp.br, gabriel.graciano@unifesp.br </a></p>")), br(),
                )
                )
            )
    )