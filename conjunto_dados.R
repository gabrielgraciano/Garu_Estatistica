conjunto_dados <- 
  tabItem(tabName = 'conjunto_dados',
<<<<<<< Updated upstream
  fluidPage(
    fluidRow(
      h3(strong('Conjunto de Dados')),
      wellPanel(
        h4(strong("Dados nativos")),
        HTML("<p>Nosso aplicativo tem um conjunto de dados nativo para que você possa acompanhar as definições junto aos 
dados, e aplicar os métodos estatísticos sobre ele. Esse conjunto se chama 'Food choices', é de domínio público, e foi extraído do <a href='https://www.kaggle.com'> Kaggle</a>. 
Ele consiste em nada mais do que um questionário aplicado na Universidade de Mercyhurst, Pennsilvânia, EUA. A base inclui informações de preferências gastronômicas, 
      nutrição, e relação com a saúde dos alunos. Ela pode ser visualizada <a href='https://www.kaggle.com/borapajo/food-choices'> aqui</a>. Logo abaixo temos uma breve 
explicação das variáveis que vocês podem encontrar e usar no aplicativo. </p>
</p>"),
        #        h4(strong("Use seus dados!")),
        #         HTML("<p>Nesta versão do GARU incluímos suporte para você carregar seu próprio conjunto de dados e utilizar os métodos 
        #    do aplicativo! Baste alterar a 'Fonte de dados' abaixo, e você poderá desfrutar de gráficos, cálculos de probabilidade e 
        #    testes sobre seus dados. É importante ressaltar que seu conjunto de dados pode não conter variáveis qualitativas ou quantitativas suficientes 
        #   para alguns dos nossos métodos. Você pode sempre alterar a <strong>fonte de dados</strong> de volta e utilizar o conjunto nativo."),
        h5("Significado das variáveis"),
        tableOutput("tabelaInformativa"),
        helpText("Vale informar que a variável 'Altura' e dados relacionados a exames de laboratório (HDL, LDL, etc.) não existiam na base de dados original e foram estimados a partir de 
             métodos de regressão treinados com a base de dados 'weight-height' e 'lab-data', também do Kaggle.")
      )
    )
  )
  )



=======
          fluidPage(
            h3(strong('Conjuntos de Dados')),
            
            fluidRow(
              wellPanel(
                h4(strong("Alimentação")),
                HTML("<p> 'dados_saude_alimentação.csv' é uma versão didática
                do 'Food choices', banco de dados de domínio público disponível 
                em <a href='https://www.kaggle.com/borapajo/food-choices' target='_blank'>Kaggle</a>. 
                A base inclui informações de preferências gastronômicas,
                nutrição e de saúde de estudantes. A variável 'altura'
                e os dados relacionados aos exames laboratoriais (HDL, LDL etc.) 
                não existiam na base de dados original e foram acrescentados,
                de modo fictício, por questões didáticas. Esta base será 
                     utilizada na apresentação de alguns conceitos estatísticos.</p>"),
                fluidRow(
                  column(6, actionButton("botaoDicioAlimentacao", "Abrir Dicionário")),
                  column(6, downloadButton("botaoBaixarAlimentacao", "Download dos dados")),
                  align = 'center'
                )
              )
            ),
            fluidRow(
              wellPanel(
                h4(strong("Paralisia Cerebral")),
                HTML("<p>Para fins didáticos, algumas informações fornecidas pelas
                <a href= 'https://bvsms.saude.gov.br/bvs/publicacoes/diretrizes_atencao_paralisia_cerebral.pdf' 
                target='_blank' > Diretrizes de Atenção à
              Pessoa com Paralisia Cerebral do Ministério da Saúde (2014) </a> 
              e achados de 
                <a href= 'https://www.scielo.br/j/rboto/a/hvQv9C6JY9h7MbrrSY98WpB/abstract/?lang=pt' target='_blank'> 
             Aurélio et al. (2002)</a> 
                na comparação do padrão de deglutição de alimentos entre
                crianças com paralisia cerebral (PC) e crianças sem acometimentos 
                neurológicos (SAN), em Curitiba/PR, foram simulados e inseridos 
                na planilha 'dados_paralisia.csv' aqui fornecida. Este banco de dados será
                utilizado em alguns exercícios.<br></p>"),
                fluidRow(
                  column(6, actionButton("botaoDicioParalisia", "Abrir Dicionário")),
                  column(6, downloadButton("botaoBaixarParalisia", "Download dos dados")),
                  align = 'center'
                )
              )
            )
            
          )
  )
>>>>>>> Stashed changes
