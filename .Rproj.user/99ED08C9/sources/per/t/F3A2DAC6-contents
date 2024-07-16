conjunto_dados <- 
  tabItem(tabName = 'conjunto_dados',
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
    ),
    wellPanel(
      
      downloadButton("botaoBaixarDados", "Download dos dados"),
      align = 'center'
    )
  )
  )



