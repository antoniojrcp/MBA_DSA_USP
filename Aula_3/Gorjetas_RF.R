
# 1 -Importando os dados -----------------------------------------------------
data(tips)


# 2- Criando a função para avaliar o modelo -------------------------------

metricas <- function(p_var, y_var){
  SQE <- sum((y_var - p_var)**2)
  
  # Cálculo do SSE (Sum of Squares Total)
  SST <- sum((y_var - mean(y_var))**2)
  
  # Cálculo do R-quadrado
  R_squared <- 1 - SQE/SST
  
  # Imprimindo os resultados
  cat("SQE: ", SQE, "QME : ", SQE/length(y_var), "\n")
  cat("SST: ", SST, "QMT: ", SST/length(y_var), "\n")
  cat("R-quadrado: ", R_squared, "\n")}
  

# 3-Criando a variável em percentual --------------------------------------

tips$pct_tip = (tips$tip /(tips$total_bill - tips$tip))


# Identificando e filtrando outlaiers -------------------------------------

boxplot(tips$pct_tip)
# Definindo limite superior para outliers
limites <- mean(tips$pct_tip) + 2 * sd(tips$pct_tip)

#filtrando os valores acima do limete estabelecido

tips <- tips %>% filter(pct_tip <= limites)
# tips <- tips[,-2]
boxplot(tips$pct_tip)


# Construção da floresta aleatória ----------------------------------------

#Separando entre dos dados para treino e validação

set.seed(123)
divisao <- sample(c('treino', 'validacao'),size = nrow(tips)
                  ,replace = T,
                  prob = c(0.8, 0.2))

table(divisao)

treino <- tips[divisao =='treino',]
teste <- tips[divisao == 'validacao',]


rf <- randomForest::randomForest(
  pct_tip ~ total_bill +
    sex +
    smoker +
    day +
    time +
    size,
  data = treino,
  ntree = 50
)

pRF_treino <- predict(rf,treino)
pRF_teste <- predict(rf,teste)

metricas(pRF_teste, teste$pct_tip)
metricas(pRF_treino, treino$pct_tip)

#tunando a florests


#Para florestas os hyperparametros que usaremos são:
# mtry = número de variáveis para a árvore
# Cros validation= usando o método de k-fold, que separa em partes iguais
## os dados de treino e validação, realizado a selecionando o melhor modelo.
#Grid Search = Faz as combinação entre os hyperparâmetros realizando o melhor ajuste.

# Definir os hiperparâmetros para o grid search
# Criar o grid de hiperparâmetros
hyperparameters <- expand.grid(mtry = c(2,3,4,5))

# Definir a função de controle para validação cruzada
ctrl <- trainControl(method = "cv", # CV indica "k-fold cross validation"
                     number = 5)  # 5 é o número de "folds"

# Realizar o grid search com validação cruzada
set.seed(123)
gridsearch_kfold <- caret::train(pct_tip ~
                                   total_bill +
                                   sex +
                                   ., 
                          data = treino, 
                          method = "rf", 
                          trControl = ctrl, 
                          tuneGrid = hyperparameters)

print(gridsearch_kfold)
plot(gridsearch_kfold)

p_rftunada <- predict(gridsearch_kfold, teste)
metricas(p_rftunada, teste$pct_tip)

