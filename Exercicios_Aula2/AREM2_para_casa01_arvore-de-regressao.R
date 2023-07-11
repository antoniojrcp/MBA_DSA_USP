####################################################
# Tarefa para casa:
# carregar a base tips conforme abaixo
# ajustar uma árvore de regressão para classificar
# o valor do percentual da gorjeta conforme
# as variáveis disponíveis


library("reshape2")
library("tidyverse")
library("rpart")
library('esquisse')

data(tips)

tips %>% head

# 1) construir o percentual de gorjeta

tips <- tips %>% mutate(
  gorj_perc = tip/total_bill
)

# 1.1) Separar os dados de treino e de teste.

set.seed(123)
bool_treino <- stats::runif(dim(tips)[1])>.25 # coloca o >.25 para fazer a comparação e gerar o T/F

treino <- tips[bool_treino,]
teste <- tips[!bool_treino,]

# 2) treinar a árvore (não incluir o valor da gorjeta como explicativa)

arvore <- rpart::rpart(
  gorj_perc ~ total_bill + sex + smoker + day + time + size,
  data = treino,
  xval =5,
  control = rpart.control(cp = 0))

#valores preditos

teste['pred'] <- stats::predict(arvore,teste)

#Ver a árvore

rpart.plot::rpart.plot(arvore2)

#2.1 tunar a árvore

tab_cp <- rpart::printcp(arvore)
rpart::plotcp(arvore)

tab_cp[which.min(tab_cp[,'xerror']),]

cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

arvore2<- rpart::rpart(
  gorj_perc ~ total_bill + sex + smoker + day + time + size,
  data = treino,
  xval =5,
  control = rpart.control(cp = cp_min))
# 3) Avaliar a árvore SQE

treino['pred'] <- stats::predict(arvore2,treino)
teste['pred'] <- stats::predict(arvore2,teste)

#função para avaliar a árvore
base <- treino
n <-  dim(base)[1]
SQE <- sum((base$gorj_perc - base$pred)**2)
QME <- SQE/n
#cálculo do SSE (Sum of Squates Total)
SST <- sum((base$gorj_perc - mean(base$gorj_perc))**2)
QMT <- SST/n
# Cálculo do R-quadrado
R_squared <- 1 - SQE/SST

# Imprimindo os resultados
cat("SQE: ", SQE, "QME : ", QME, "\n")
cat("SST: ", SST, "QMT: ", QMT, "\n")
cat("R-quadrado: ", R_squared, "\n")

esquisser(teste)



