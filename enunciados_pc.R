#Enunciados adaptados do exercício de paralisia cerebral
library(shinyWidgets)

#Esses são os enunciados antigos

enunciados_pc <- list(
  "1" = "Calcule as medidas-resumos adequadas às variáveis e apresente os resultados
  em uma única tabela.",
  
  "2" = "Construa um gráfico para visualizar o grau de disfunção motora oral (DMO).",
  
  "3" = "Construa um gráfico para visualizar os distúrbios de comunicação.",
  
  "4" = "Construa um gráfico para visualizar o tempo de deglutição de alimentos líquidos.",
  
  "5" = "Calcule medidas-resumos adequadas aos tempos de deglutição de alimentos
  (líquidos, pastosos e sólidos) segundo o grupo de crianças. Apresente os resultados
  em uma única tabela.",
  
  "6" = "Observe a tabela de perda auditiva por grupo de crianças.
  Há indícios de que a perda auditiva esteja associada à PC? Qual é o teste estatístico
  mais apropriado para verificar essa associação?",

  "7" = "Cruze os distúrbios de comunicação com o grupo de crianças e dê os resultados
  em uma tabela. Há indícios de que os distúrbios de comunicação estejam associados à PC? 
Verifique sua observação com um teste estatístico apropriado e nível de significância de 5%.",
  
  "8" = "Faça um gráfico para o tempo de deglutição de alimentos líquidos pelo grupo de crianças. 
Há indícios de que o tempo de deglutição de alimentos líquidos esteja associado à PC?
Verifique sua observação com um teste estatístico apropriado e nível de significância de 5%.",
  
"9" = "Faça um gráfico para o tempo de deglutição de alimentos pastosos pelo grupo de crianças. 
Há indícios de que o tempo de deglutição de alimentos pastosos esteja associado à PC? 
Verifique sua observação com um teste estatístico apropriado e nível de significância de 5%.",

"10" = "Faça um gráfico para o tempo de deglutição de alimentos sólidos pelo grupo de crianças. 
Há indícios de que o tempo de deglutição de alimentos sólidos esteja associado à PC? 
Verifique sua observação com um teste estatístico apropriado e nível de significância de 5%.",

"11" = "Faça um gráfico para visualizar o tempo de deglutição de alimentos líquidos com
o tempo de deglutição de alimentos sólidos. Há indícios de que as variáveis estejam
correlacionadas? Verifique sua observação com um teste estatístico apropriado e nível
de significância de 5%.",

"12" = "Faça um gráfico para visualizar o tempo de deglutição de alimentos pastosos
com o tempo de deglutição de alimentos sólidos. Há indícios de que as variáveis estejam
correlacionadas? Verifique sua observação com um teste estatístico apropriado e nível
de significância de 5%."
)

variaveis_fator <- names(dados_paralisia)[sapply(dados_paralisia, is.factor)]
variaveis_numeric <- names(dados_paralisia)[sapply(dados_paralisia, is.numeric)]




respostas_esperadas_pc <- list(
  ex1 = list(variavel_quali = variaveis_fator, variavel_quanti = variaveis_numeric),
  ex2 = list(variavel_pc_x = 'dmo', variavel_pc_y = "Não se aplica", tipo_grafico_pc = 'Barras'),
  ex3 = list(variavel_pc_x = 'dist_comun', variavel_pc_y = "Não se aplica", tipo_grafico_pc = 'Barras'),
  ex4 = list(variavel_pc_x = "Não se aplica", variavel_pc_y = 'td_liquido', tipo_grafico_pc = 'Boxplot'),
  ex5 = list(medidas_resumo_ex5 = c('Média', 'Desvio-Padrão')),
  ex6 = list(hipotese_ex6 = 'Sim', teste_ex6 = 'Teste Qui-quadrado de Pearson'),
  ex7 = list(hipotese_ex7 = 'Sim', teste_ex7 = 'Teste Qui-quadrado de Pearson'),
  ex8 = list(variavel_pc_x = 'grupo', variavel_pc_y = 'td_liquido', tipo_grafico_pc = 'Boxplot'),
  ex9 = list(variavel_pc_x = 'grupo', variavel_pc_y = 'td_pastoso', tipo_grafico_pc = 'Boxplot'),
  ex10 = list(variavel_pc_x = 'grupo', variavel_pc_y = 'td_solido', tipo_grafico_pc = 'Boxplot')
)

library(tidyverse)









#############################
# Esses são os enunciados novos

enunciados_pc_novo <- list(
  "1" = "<left>
  <p><b>1)</b> Utilize as caixas de seleção “Classificação de variáveis” 
  para agrupar as variáveis em <b>qualitativas e quantitativas</b>.<p>
  </left>",
  
  "2" = "<left>
  <p><b>2)</b> Utilize a caixa de seleção “Medidas-resumo” para selecionar <b>todas</b> as medidas-resumo
  adequadas aos <b>tempos de deglutição de alimentos (líquidos, pastosos e sólidos)</b>.<p>
  </left>",
  
  "3" = "<left>
  <p><b>3)</b> Utilize a caixa de seleção “Medidas-resumo”
  para selecionar <b>todas</b>  as medidas-resumo adequadas ao <b>distúrbio de comunicação</b>.<p>
  </left>",
  
  "4" = "<left>
  <p><b>4)</b> Utilize as caixas de seleção para construir
  um gráfico adequado para visualizar o <b>grau de disfunção motora oral (DMO)</b>.<p>
  </left>",
  
  "5" = "<left>
  <p><b>5)</b> Utilize a caixa de seleção “Elementos gráficos” para construir um gráfico
  adequado para visualizar os <b>distúrbios de comunicação</b>.<p>
  </left>",
  
  "6" = "<left>
  <p><b>6)</b> Utilize a caixa de seleção “Elementos gráficos” 
  para construir um gráfico adequado para visualizar o <b>tempo de deglutição
  de alimentos líquidos</b>.<p>
  </left>",
  
  "7" = "<left>
  <p><b>7)</b> A tabela abaixo apresenta a distribuição de perda auditiva pelos grupos de crianças SAN e PC.<p>
  
  <p>Para você, <b>há indícios de que a perda auditiva esteja associada à PC?</b> Utilize a caixa de seleção
  “Selecione suas respostas” para verificar sua intuição:<p>
  <br>
  <b>a)</b> Com um nível de significância de 5%, qual é o teste estatístico mais apropriado
  para verificar se há associação estatisticamente significante entre essas variáveis?
  <br>
  <br>
  <b>b)</b> De acordo com o resultado do teste, é correto afirmar que as variáveis estão 
  associadas ao nível de significância de 5%?
  </left>",
  
  "8" = "<left>
  <p><b>8)</b> A tabela abaixo apresenta os distúrbios de comunicação pelos grupos de
   crianças SAN e PC.<p>
  <p>Para você, <b>há indícios de que os distúrbios de comunicação estejam
  associados à PC?</b> Utilize a caixa de seleção “Selecione suas respostas” para verificar
  sua intuição:<p>
  <br>
  <b>a)</b> Com um nível de significância de 5%, qual é o teste estatístico mais apropriado
  para verificar se há associação estatisticamente significante entre essas variáveis?
  <br>
  <br>
  <b>b)</b> De acordo com o resultado do teste, é correto afirmar que as variáveis estão
  associadas ao nível de significância de 5%?
  </left>",
  
  "9" = "<left>
  <p><b>9)</b> Utilize a caixa de seleção “Elementos gráficos” para construir uma visualização
  para a relação do <b>tempo de deglutição de alimentos líquidos pelos grupos de crianças 
  PC e SAN</b>.<p>
  <p>Para você, há indícios de que o tempo de deglutição de alimentos líquidos
  esteja associado à PC? Utilize a caixa de seleção “Selecione suas respostas” para verificar
  sua intuição:<p>
  <br>
  <b>a)</b> Sabendo que um dos grupos (PC) <b>não possui tempo de deglutição de alimentos líquidos com distribuição normal</b>, qual é o teste estatístico mais apropriado
  para verificar se há associação estatisticamente significante entre essas variáveis a um nível de 5%?
  <br>
  <br>
  <b>b)</b> De acordo com o resultado do teste, é correto afirmar que as variáveis estão
  associadas ao nível de significância de 5%?
  </left>",
  
  "10" = "<left>
  <p><b>10)</b> Utilize a caixa de seleção “Elementos gráficos” para construir uma visualização
  para a relação do <b>tempo de deglutição de alimentos líquidos com o tempo de deglutição
  de alimentos sólidos.</b><p>
  
  <p>Para você, há indícios de que esses tempos estão relacionados?
  Utilize a caixa de seleção “Selecione suas respostas” para verificar sua intuição:<p>
  
  <b>a)</b> Sabendo que <b>ambas as variáveis não possuem distribuição normal</b>, qual é o teste estatístico mais apropriado
  para verificar se há relação estatisticamente significante entre elas a um nível de 5%?
  <br> 
  <br>
  <b>b)</b> De acordo com o resultado do teste, é correto afirmar que as variáveis estão
  relacionadas ao nível de significância de 5%?
  </left>"
)


