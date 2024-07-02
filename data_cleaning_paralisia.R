library(gtsummary)
library(dplyr)
library(ggplot2)
dados_paralisia <- read.csv('data/dados_paralisia_clean.csv')

#Dicionário de mudanças nos nomes
nomes_exibidos <- c(
  'Sexo' = 'sexo',
  'Grupo' = 'grupo',
  'Idade' = 'idade',
  'Perda Auditiva' = 'perda_audit',
  'Distúrbio de Comunicação' = 'dist_comun',
  'Grau de Disfunção Motora Oral' = 'dmo',
  'Tempo líquido' = 'td_liquido',
  'Tempo pastoso' = 'td_pastoso',
  'Tempo sólido' = 'td_solido'
)


# Configurar restante das variáveis
dados_paralisia$sexo <- factor(dados_paralisia$sexo)
dados_paralisia$grupo <- factor(dados_paralisia$grupo)
dados_paralisia$idade <- as.numeric(dados_paralisia$idade)
dados_paralisia$perda_audit <- factor(dados_paralisia$perda_audit)
dados_paralisia$dist_comun <- factor(dados_paralisia$dist_comun)
dados_paralisia$dmo <- factor(dados_paralisia$dmo, ordered = TRUE)
dados_paralisia$td_liquido <- as.numeric(dados_paralisia$td_liquido)
dados_paralisia$td_pastoso <- as.numeric(dados_paralisia$td_pastoso)
dados_paralisia$td_solido <- as.numeric(dados_paralisia$td_solido)



dados_paralisia <- dados_paralisia[,-1]


'entradas <- c(
  0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0
)' 


'# Criar um vetor vazio para armazenar os dados recuperados
dados_paralisia$perda_audit <- rep(NA, length(entradas))

# Preencher o vetor perda_audit com base nas entradas fornecidas
dados_paralisia$perda_audit[entradas == 0] <- 0
dados_paralisia$perda_audit[entradas == 1] <- 1
'



