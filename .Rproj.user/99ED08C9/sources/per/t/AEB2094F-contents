library(ggplot2)


library(dplyr)
library(tidyverse)
library(shiny)
library(htmltools)
library(imager)


perguntas_respostas <- list(
  
  
  list(
    pergunta =  HTML('Qual das seguintes estatísticas descritivas é afetada pela adição de um valor discrepante a um conjunto de dados?'),
    respostas = c('Média',
                  'Mediana',
                  'Moda',
                  'Primeiro quartil'),
    resposta_correta = 1
  ),
  
  list(
    pergunta =  HTML('Qual dos seguintes conjuntos de dados tem uma Média de 15 e um desvio padrão de 0?'),
    respostas = c('0, 15, 30',
                  '15, 15, 15',
                  '0, 0, 0',
                  'Não existe um conjunto de dados com desvio padrão de 0'),
    resposta_correta = 2
  ),
  
  list(
    pergunta =  HTML('Qual das seguintes afirmações é verdadeira?'),
    respostas = c('50% dos valores de um conjunto de dados está entre o 1o e o 3o quartis.
',
                  '50% dos valores de um conjunto de dados está entre a mediana e o valor máximo
',
                  '50% dos valores de um conjunto de dados está entre a mediana e o valor mínimo
',
                  'Todas as anteriores
'),
    resposta_correta = 4
  ),
  
  list(
    pergunta =  HTML('Suponha que o conjunto de dados contém os pesos de uma amostra aleatória de 100 recém-nascidos, em libras. Qual das estatísticas descritivas a seguir não é medida em libras?
'),
    respostas = c('A média dos pesos',
                  'O desvio padrão dos pesos',
                  'A variância dos pesos',
                  'A amplitude dos pesos'),
    resposta_correta = 3
  ),
  
  list(
    pergunta =  HTML('O seguinte gráfico de barras representa os planos de pós-graduação dos formandos de ensino médio. Suponha que cada aluno escolha uma dessas cinco opções. (Nota: Um ano sabático significa que o aluno ficará de folga por um ano antes de decidir o que fazer.)
<br>
<br>
                     <center><img src="images/questionario/q5_imagem.png"></center>
                     <br>
                     <br>
                     Qual é o plano de pós-graduação mais comum para esses formandos?
'),
    respostas = c('Emprego',
                  'Faculdade Comunitária',
                  'Universidade',
                  'Exército'),
    resposta_correta = 3
  ),
  
  list(
    pergunta =  HTML('O seguinte gráfico de barras representa os planos de pós-graduação dos formandos de ensino médio. Suponha que cada aluno escolha uma dessas cinco opções. (Nota: Um ano sabático significa que o aluno ficará de folga por um ano antes de decidir o que fazer.)
<br>
<br>
                     <center><img src="images/questionario/q5_imagem.png"></center>
                     <br>
                     <br>
                     Qual é o plano de pós-graduação menos comum para esses formandos?
'),
    respostas = c('Emprego',
                  'Faculdade Comunitária',
                  'Universidade',
                  'Exército'),
    resposta_correta = 4
  ),
  
  list(
    pergunta =  HTML('O seguinte gráfico de barras representa os planos de pós-graduação dos formandos de ensino médio. Suponha que cada aluno escolha uma dessas cinco opções. (Nota: Um ano sabático significa que o aluno ficará de folga por um ano antes de decidir o que fazer.)
<br>
<br>
                     <center><img src="images/questionario/q5_imagem.png"></center>
                     <br>
                     <br>
                     Presumindo que cada aluno escolheu apenas uma das cinco possibilidades, cerca de quantos alunos planejam tirar um ano sabático ou ir para a universidade? 

'),
    respostas = c('20',
                  '120',
                  '100',
                  '140'),
    resposta_correta = 4
  ),
  
  list(
    pergunta =  HTML('O seguinte gráfico de barras representa os planos de pós-graduação dos formandos de ensino médio. Suponha que cada aluno escolha uma dessas cinco opções. (Nota: Um ano sabático significa que o aluno ficará de folga por um ano antes de decidir o que fazer.)
<br>
<br>
                     <center><img src="images/questionario/q5_imagem.png"></center>
                     <br>
                     <br>
                     Qual porcentagem da turma de graduandos está planejando frequentar a faculdade comunitária? 


'),
    respostas = c('90/330',
                  '90/240',
                  '240/330',
                  '120/330'),
    resposta_correta = 1
  ),
  
  list(
    pergunta =  HTML(' O seguinte gráfico de pizza mostra a proporção de alunos matriculados em diferentes cursos de uma universidade.
<br>
<br>
<center><img src="images/questionario/q9_imagem.png"></center>
<br>
<br>
Se alguns alunos estiverem matriculados em mais de um curso, qual tipo de gráfico seria mais apropriado para exibir a porcentagem em cada curso?

                     '),
    respostas = c('O mesmo gráfico de pizza',
                  'Um gráfico de pizza separado para cada curso mostrando qual porcentagem está matriculada e qual porcentagem não está.
',
                  'Um gráfico de barras onde cada barra representa um curso e a altura mostra qual o percentual de alunos matriculados.
',
                  'Alternativas (b) e (c)'),
    resposta_correta = 4
  ),
  
  list(
    pergunta =  HTML('Internações por doenças respiratórias em idosos e a intervenção vacinal contra influenza no Estado de São Paulo (Francisco, et al 2004).
<br>
<br>
<center><img src="images/questionario/q10_imagem.png"></center>
<br>
<br>
Da figura acima é correto afirmar que:

                     '),
    respostas = c('A proporção de internações por doenças respiratórias é maior nas mulheres idosas.
',
                  'Pode-se dizer que parece não haver efeito de intervenção da vacina pois os percentuais de internação mantiveram-se os mesmos.',
                  'Não há sazonalidade na proporção de internações.',
                  'Pode-se dizer que parece haver efeito da vacinação, pois os percentuais de internação diminuíram entre os idosos.'),
    resposta_correta = 4
  ),
  
  list(
    pergunta =  HTML('Foi realizado um estudo para avaliar a relação entre o índice de massa corporal segundo padrão internacional e indicadores de adiposidade no diagnóstico de sobrepeso e obesidade em 528 escolares, entre 6 e 10 anos, de ambos os sexos (Giugliano, R e Melo, ALP 2004). A seguir tem-se o gráfico de dispersão entre o índice de massa corporal e o percentual de gordura corporal em escolares, na faixa etária de 6 a 10 anos.
<br>
<br>
<center><img src="images/questionario/q11_imagem.png" width="671" height="368"></center>
<br>
<br>
Com relação aos gráficos acima, é correto afirmar:

                     '),
    respostas = c('Há indícios de correlação negativa entre IMC e percentual de gordura corporal das meninas.
',
                  'Há indícios de correlação negativa entre IMC e percentual de gordura corporal dos meninos.
',
                  'Não há indícios de correlação positiva entre IMC e percentual de gordura corporal das meninas.',
                  'Há indícios de correlação positiva entre IMC e percentual de gordura corporal das meninas.
'),
    resposta_correta = 4
  ),
  
  list(
    pergunta =  HTML('Abaixo, são dadas medidas resumo do salário mensal e da carga horária semanal (em horas) de 100 funcionários de um determinado hospital:

                     <br>
                     <br>
                     <center><img src="images/questionario/q12_imagem.png"></center>
<br>
<br>
Com base nessas informações, assinale a alternativa CORRETA.
                     '),
    respostas = c('O salário possui menor variabilidade que a carga horária semanal, pois o coeficiente de variação é menor.
',
                  'O salário possui menor variabilidade que a carga horária semanal, pois tem maior desvio padrão.
',
                  'A carga horária semanal possui menor variabilidade que o salário, pois o desvio padrão é menor.',
                  'A carga horária semanal possui menor variabilidade que o salário, pois o coeficiente de variação é menor.
'),
    resposta_correta = 1
  ),
  
  
  list(
    pergunta =  HTML('A faixa etária é uma variável ___________; um tipo de gráfico apropriado para representar sua distribuição é o ________. A pressão arterial é uma variável __________; um tipo de gráfico apropriado para representar sua distribuição é o _________________. 
Assinale a alternativa que preenche corretamente os espaços em branco acima. 
'),
    respostas = c('Qualitativa ordinal; de barras; quantitativa discreta; de boxplot.

',
                  'Quantitativa discreta; de barras; quantitativa contínua; histograma.

',
                  'Qualitativa ordinal; de pizza; quantitativa contínua; boxplot.
',
                  'Qualitativa ordinal; de barras; quantitativa contínua; histograma.

'),
    resposta_correta = 4
  ),
  
  list(
    pergunta =  HTML(' Para visualizar a relação entre idade gestacional da mãe (em semanas) e peso ao nascer (em quilogramas) de recém nascidos, qual os gráficos abaixo é mais apropriado?
'),
    respostas = c('Gráfico de barras
',
                  'Gráfico de linhas
',
                  'Gráfico de dispersão bivariado',
                  'Boxplot
'),
    resposta_correta = 3
  )
  
  
  
  )
  
  
  