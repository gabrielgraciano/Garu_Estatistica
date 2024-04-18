
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
             <p><b>Sexo: menino ou menina
             <p>Idade: idade em anos completos
             <p>Grupo: grupo das crianças por condição de saúde (SAN ou PC)
             <p>Perda auditiva: existência ou não de perda auditiva
             <p>Distúrbio de Comunicação: existência ou não de distúrbio de comunicação
             <p>DMO: grau de DMO (disfunção motora oral, compreendido entre 0 e 4)
             <p>Tempo líquido: tempo, em segundos, para deglutição de 100 ml de suco de laranja
             <p>Tempo pastoso: tempo, em segundos, para deglutição de 140 g de iogurte de morango homogêneo
             e sem pedaços de fruta
             <p>Tempo sólido: tempo, em segundos, para deglutição de 12 g de bolacha recheada de chocolate
                             </b>"
                        )
                        
                        
              )
            ),
            
            
            
              tabsetPanel(
                tabPanel('Ex1', uiOutput('ex1')),
                tabPanel("Ex2", uiOutput('ex2')),
                tabPanel('Ex3', uiOutput('ex3')),
                tabPanel('Ex4', uiOutput('ex4')),
                tabPanel('Ex5', uiOutput('ex5')),
                tabPanel('Ex6', uiOutput('ex6')),
                tabPanel('Ex7', uiOutput('ex7')),
                tabPanel('Ex8', uiOutput('ex8')),
                tabPanel('Ex9', uiOutput('ex9')),
                tabPanel('Ex10', uiOutput('ex10'))
              )
              
            
            
            
            
            
          )
  )

