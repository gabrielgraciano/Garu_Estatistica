#Enunciados adaptados do exercício de paralisia cerebral
library(shinyWidgets)

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


#############################
# Esses são os enunciados novos

enunciados_pc_novo <- list(
  "1" = "Utilize as caixas de seleção para agrupar as variáveis em qualitativas e quantitativas.",
  
  "2" = "Utilize a caixa de seleção “Medidas-resumo” para selecionar todas as medidas-resumo
  adequadas aos tempos de deglutição de alimentos (líquidos, pastosos e sólidos).",
  
  "3" = "Utilize a caixa de seleção “Medidas-resumo”
  para selecionar todas  as medidas-resumo adequadas ao distúrbio de comunicação.",
  
  "4" = "Utilize a caixa de seleção “Elementos gráficos” para construir
  um gráfico adequado para visualizar o grau de disfunção motora oral (DMO).",
  
  "5" = "Utilize a caixa de seleção “Elementos gráficos” para construir um gráfico
  adequado para visualizar os distúrbios de comunicação.",
  
  "6" = "Utilize a caixa de seleção “Elementos gráficos” 
  para construir um gráfico adequado para visualizar o tempo de deglutição
  de alimentos líquidos.",
  
  "7" = "A tabela abaixo apresenta a distribuição de perda auditiva pelos grupos de crianças SAN e PC.
  Para você, há indícios de que a perda auditiva esteja associada à PC? Utilize a caixa de seleção
  “Selecione suas respostas” para verificar sua intuição:
  <br>
  a) Com um nível de significância de 5%, qual é o teste estatístico mais apropriado
  para verificar se há associação estatisticamente significante entre essas variáveis?
  <br>
  b) De acordo com o resultado do teste, é correto afirmar que as variáveis estão 
  associadas ao nível de significância de 5%?",
  
  "8" = "A tabela abaixo apresenta os distúrbios de comunicação pelos grupos de
  crianças SAN e PC. Para você, há indícios de que os distúrbios de comunicação estejam
  associados à PC? Utilize a caixa de seleção “Selecione suas respostas” para verificar
  sua intuição:
  <br>
  a) Com um nível de significância de 5%, qual é o teste estatístico mais apropriado
  para verificar se há associação estatisticamente significante entre essas variáveis?
  <br>
  b) De acordo com o resultado do teste, é correto afirmar que as variáveis estão
  associadas ao nível de significância de 5%?",
  
  "9" = "Utilize a caixa de seleção “Elementos gráficos” para construir uma visualização
  para a relação do tempo de deglutição de alimentos líquidos pelos grupos de crianças 
  PC e SAN. Para você, há indícios de que o tempo de deglutição de alimentos líquidos
  esteja associado à PC? Utilize a caixa de seleção “Selecione suas respostas” para verificar
  sua intuição:
  <br>
  a) Com um nível de significância de 5%, qual é o teste estatístico mais apropriado
  para verificar se há associação estatisticamente significante entre essas variáveis?
  <br>
  b) De acordo com o resultado do teste, é correto afirmar que as variáveis estão
  associadas ao nível de significância de 5%?",
  
  "10" = "Utilize a caixa de seleção “Elementos gráficos” para construir uma visualização
  para a relação do tempo de deglutição de alimentos líquidos com o tempo de deglutição
  de alimentos sólidos. Para você, há indícios de que esses tempos estão relacionados?
  Utilize a caixa de seleção “Selecione suas respostas” para verificar sua intuição:
  <br>
  a) Com um nível de significância de 5%, qual é o teste estatístico mais apropriado
  para verificar se há relação estatisticamente significante entre essas variáveis?
  <br> 
  b) De acordo com o resultado do teste, é correto afirmar que as variáveis estão
  relacionadas ao nível de significância de 5%?"
)