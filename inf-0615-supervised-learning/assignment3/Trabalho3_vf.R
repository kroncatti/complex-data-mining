### ============ Trabalho 03 ============ ###
## Membros do grupo:
# - Kaleb Roncatti de Souza
# - Nelson Gomes Brasil Junior


################################################################################
# Algumas funções que serão utilizadas durante o trabalho
################################################################################
getHypothesis <- function(feature_names, degree){
  
  hypothesis_string <- "hypothesis <- formula(Resultado ~ "
  for(d in 1:degree){
    for(i in 1:length(feature_names)){
      hypothesis_string <- paste(hypothesis_string, 
                                 "I(", feature_names[i], "^", d, ") + ",
                                 sep = "")
    }
  }
  hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
  hypothesis_string <- paste(hypothesis_string, ")")
  hypothesis <- eval(parse(text=hypothesis_string))
  return(hypothesis)
}

# Calcula a matriz de confusão relativa 
calculaMatrizConfusaoRelativa <- function(cm){
  
  # Aplicamos a transposição para garantir que a referencia
  # fique nas linhas e a predicao nas colunas
  cm_absolute = t(cm$table)
  
  # SEMPRE construam e reportem a matriz de confusao relativa!
  cm_relative = cm_absolute
  
  cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
  cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
  
  return(cm_relative)  
}

################################################################################
# Carregando algumas bibliotecas
################################################################################

library(caret)
library(reshape2)
library(ggplot2)
library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)

set.seed(55)
###############################################################################
# O PROJETO COMEÇA AQUI, SEPARAMOS CADA ITEM POR MÚLTIPLOS HASHTAGS
###############################################################################
##### 1. Lendo e inspecionando o dataset
data <- read.csv("covid_analysis_train_val_sets.csv", 
                 header=TRUE,
                 sep=",", 
                 stringsAsFactors=TRUE)
dim(data)

# 1. Verificando se existe algum valor nulo
any(is.na(data))


# 1. Removendo duplicatas antes de separarmos os conjuntos
data <- unique(data)
summary(data)

# 1. Split train/validation 80% / Teste 20%
randomTrainIndexes <- sample(1:nrow(data), size=0.8*nrow(data))
dataTrain <- data[randomTrainIndexes, ]
dim(dataTrain)

dataVal  <- data[-randomTrainIndexes, ]
dim(dataVal)

# 1. Carregando os dados de Teste
dataTest <- read.csv("covid_analysis_test_set.csv", 
                     header=TRUE,
                     sep=",", 
                     stringsAsFactors=TRUE)
dim(dataTest)

###############################################################################
#### 2. Inspecionando os dados de treino
summary(dataTrain)

# 2. Os datasets estão desbalanceados
dataTrainNeg <- dataTrain[dataTrain$Resultado == "NEGATIVO", ]
dim(dataTrainNeg)
dataTrainPos <- dataTrain[dataTrain$Resultado == "POSITIVO", ] 
dim(dataTrainPos)

# 2. Lidando com o balanceamento através de oversampling
factorDiff <- nrow(dataTrainNeg) / nrow(dataTrainPos)

randomPosIdx <- sample(1:nrow(dataTrainPos), size=factorDiff*nrow(dataTrainPos), replace = TRUE)
oversamplingPos <- dataTrainPos[randomPosIdx,]

dataTrain <- rbind(oversamplingPos, dataTrainNeg)
dim(dataTrain)

###############################################################################
#### 3. Treinando um baseline
featureNames <- colnames(dataTrain)[1:(ncol(dataTrain)-1)]
fBaseline <- getHypothesis(featureNames, 1)

treeModel <- rpart(formula=fBaseline, 
                   data=dataTrain, 
                   method="class",
                   control=rpart.control(minsplit=4, cp=0.0, xval = 10),
                   parms= list(split="information"))
summary(treeModel)
prp(treeModel)

# 3. Rodando a importância das features para os itens subsequentes
importance_per_features <- treeModel$variable.importance
importance_per_features

relative_importance <- importance_per_features/sum(importance_per_features)
relative_importance

# 3. Performance nos conjuntos de treino, validação e teste
# 3. Treino
train_pred <- predict(treeModel, dataTrain, type="class")
train_cm <- confusionMatrix(data = as.factor(train_pred), 
                      reference = as.factor(dataTrain$Resultado), 
                      positive='POSITIVO')
train_cm_relative <- calculaMatrizConfusaoRelativa(train_cm); train_cm_relative


train_acc_bal <- (train_cm_relative[1,1] + train_cm_relative[2,2])/2; train_acc_bal



# 3. Validação
val_pred <- predict(treeModel, dataVal, type="class")
val_cm <- confusionMatrix(data = as.factor(val_pred), 
                            reference = as.factor(dataVal$Resultado), 
                            positive='POSITIVO')
val_cm_relative <- calculaMatrizConfusaoRelativa(val_cm); val_cm_relative


val_acc_bal <- (val_cm_relative[1,1] + val_cm_relative[2,2])/2; val_acc_bal


# 3. Testes
test_pred <- predict(treeModel, dataTest, type="class")
test_cm <- confusionMatrix(data = as.factor(test_pred), 
                          reference = as.factor(dataTest$Resultado), 
                          positive='POSITIVO')
test_cm_relative <- calculaMatrizConfusaoRelativa(test_cm); test_cm_relative


test_acc_bal <- (test_cm_relative[1,1] + test_cm_relative[2,2])/2; test_acc_bal

###############################################################################
#### 4. Variando-se a profundidade
accPerDepth <- data.frame(depth=numeric(29), accTrain=numeric(29), accVal=numeric(29))
for (maxDepth in 1:29){
  treeModel <- rpart(formula=fBaseline, 
                     data=dataTrain, method="class",
                     control=rpart.control(minsplit=4, cp=0.0, 
                                           maxdepth=maxDepth, xval = 0),
                     parms= list(split="information"))
  
  # Avaliando no conjunto de treinamento
  train_pred <- predict(treeModel, dataTrain, type="class")
  cm_train <- confusionMatrix(data = as.factor(train_pred), 
                              reference = as.factor(dataTrain$Resultado), 
                              positive='POSITIVO')
  
  cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
  acc_bal_train <- (cm_train_relative[1,1] + cm_train_relative[2,2])/2
  
  # Avaliando no conjunto de validacao
  val_pred <- predict(treeModel, dataVal, type="class")
  cm_val <- confusionMatrix(data = as.factor(val_pred), 
                            reference = as.factor(dataVal$Resultado), 
                            positive='NEGATIVO')
  
  cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
  acc_bal_val <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2
  
  accPerDepth[maxDepth,] = c(maxDepth, 
                             acc_bal_train, 
                             acc_bal_val)
}

accPerDepth <- melt(accPerDepth, id="depth")

# 4. Plotando a avaliação
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point()

# 4. Encontrando o melhor modelo de acordo com acurácia de validação
accValPerDepth <- accPerDepth[accPerDepth$variable == "accVal", ]
maxAccValPerDepth <- accValPerDepth[accValPerDepth$value == max(accValPerDepth$value),]

# 4. Pegando a melhor árvore, checando para conjunto de validação
treeModel <- rpart(formula=fBaseline, 
                   data=dataTrain, method="class",
                   control=rpart.control(minsplit=4, cp=0.0, 
                                         maxdepth=which.max(maxAccValPerDepth$depth), xval = 0),
                   parms= list(split="information"))

val_pred <- predict(treeModel, dataVal, type="class")
val_cm <- confusionMatrix(data = as.factor(val_pred), 
                          reference = as.factor(dataVal$Resultado), 
                          positive='POSITIVO')
val_cm_relative <- calculaMatrizConfusaoRelativa(val_cm); val_cm_relative


val_acc_bal <- (val_cm_relative[1,1] + val_cm_relative[2,2])/2; val_acc_bal

# Testes
test_pred <- predict(treeModel, dataTest, type="class")
test_cm <- confusionMatrix(data = as.factor(test_pred), 
                           reference = as.factor(dataTest$Resultado), 
                           positive='POSITIVO')
test_cm_relative <- calculaMatrizConfusaoRelativa(test_cm); test_cm_relative


test_acc_bal <- (test_cm_relative[1,1] + test_cm_relative[2,2])/2; test_acc_bal

###############################################################################
#### 5. Explorando os subconjuntos de features para treinar mais árvores de decisão

# 5. Escolhendo as 14 features mais importantes segundo a importância relativa em cima do baseline
treeModel <- rpart(formula = Resultado ~ (Dimeros.D..quantitativo + DHL + 
                                            Eosinofilos + ctO2.arterial +
                                            pO2.arterial + Plaquetas +
                                            Sat.O2.arterial + Leucocitos +
                                            VCM + TTPA + FHHb.arterial +
                                            Basofilos + Linfocitos + Eritrocitos ), 
                   data=dataTrain, 
                   method="class",
                   control=rpart.control(minsplit=4, cp=0.0, xval = 10, maxdepth=3),
                   parms= list(split="information"))

# 5. Performance nos conjuntos de treino, validação e teste
# 5. Treino
train_pred <- predict(treeModel, dataTrain, type="class")
train_cm <- confusionMatrix(data = as.factor(train_pred), 
                            reference = as.factor(dataTrain$Resultado), 
                            positive='POSITIVO')
train_cm_relative <- calculaMatrizConfusaoRelativa(train_cm); train_cm_relative


train_acc_bal <- (train_cm_relative[1,1] + train_cm_relative[2,2])/2; train_acc_bal



# 5. Validação
val_pred <- predict(treeModel, dataVal, type="class")
val_cm <- confusionMatrix(data = as.factor(val_pred), 
                          reference = as.factor(dataVal$Resultado), 
                          positive='POSITIVO')
val_cm_relative <- calculaMatrizConfusaoRelativa(val_cm); val_cm_relative


val_acc_bal <- (val_cm_relative[1,1] + val_cm_relative[2,2])/2; val_acc_bal

# 5. Teste
test_pred <- predict(treeModel, dataTest, type="class")
test_cm <- confusionMatrix(data = as.factor(test_pred), 
                           reference = as.factor(dataTest$Resultado), 
                           positive='POSITIVO')
test_cm_relative <- calculaMatrizConfusaoRelativa(test_cm); test_cm_relative


test_acc_bal <- (test_cm_relative[1,1] + test_cm_relative[2,2])/2; test_acc_bal

# 5. Escolhendo as 8 features mais importantes segundo a importância relativa em cima do baseline
treeModel <- rpart(formula = Resultado ~ (Dimeros.D..quantitativo + DHL + 
                                            Eosinofilos + ctO2.arterial +
                                            pO2.arterial + Plaquetas +
                                            Sat.O2.arterial + Leucocitos), 
                   data=dataTrain, 
                   method="class",
                   control=rpart.control(minsplit=4, cp=0.0, xval = 10, maxdepth=4),
                   parms= list(split="information"))

# 5. Performance nos conjuntos de treino, validação e teste
# 5. Treino
train_pred <- predict(treeModel, dataTrain, type="class")
train_cm <- confusionMatrix(data = as.factor(train_pred), 
                            reference = as.factor(dataTrain$Resultado), 
                            positive='POSITIVO')
train_cm_relative <- calculaMatrizConfusaoRelativa(train_cm); train_cm_relative


train_acc_bal <- (train_cm_relative[1,1] + train_cm_relative[2,2])/2; train_acc_bal



# 5. Validação
val_pred <- predict(treeModel, dataVal, type="class")
val_cm <- confusionMatrix(data = as.factor(val_pred), 
                          reference = as.factor(dataVal$Resultado), 
                          positive='POSITIVO')
val_cm_relative <- calculaMatrizConfusaoRelativa(val_cm); val_cm_relative


val_acc_bal <- (val_cm_relative[1,1] + val_cm_relative[2,2])/2; val_acc_bal

# Conclusão: O modelo com as 8 features mais importantes apresentou melhor acc balanceada
# 5. Teste
test_pred <- predict(treeModel, dataTest, type="class")
test_cm <- confusionMatrix(data = as.factor(test_pred), 
                           reference = as.factor(dataTest$Resultado), 
                           positive='POSITIVO')
test_cm_relative <- calculaMatrizConfusaoRelativa(test_cm); test_cm_relative


test_acc_bal <- (test_cm_relative[1,1] + test_cm_relative[2,2])/2; test_acc_bal

###############################################################################
#### 6. Treinando múltiplas random forests variando-se o número de árvores

# 6. Utilizando-se as mesmas configurações de fórmula do exercício anterior

# 6. 14 features
nTreeList = c(1, 5, 10, 25, 50, 100, 250, 500, 1000)
accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))

set.seed(55)
for (i in 1:length(nTreeList)){
  rfModel <- randomForest(formula=Resultado ~ (Dimeros.D..quantitativo + DHL + 
                                                 Eosinofilos + ctO2.arterial +
                                                 pO2.arterial + Plaquetas +
                                                 Sat.O2.arterial + Leucocitos +
                                                 VCM + TTPA + FHHb.arterial +
                                                 Basofilos + Linfocitos + Eritrocitos ), 
                          data= dataTrain, ntree=nTreeList[i], mtry=3)
  
  # Avaliando no conjunto de treinamento
  train_pred <- predict(rfModel, dataTrain, type="class")
  cm_train <- confusionMatrix(data = as.factor(train_pred), 
                              reference = as.factor(dataTrain$Resultado), 
                              positive='POSITIVO')
  
  cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
  acc_bal_train <- (cm_train_relative[1,1] + cm_train_relative[2,2])/2
  
  # Avaliando no conjunto de validacao
  val_pred <- predict(rfModel, dataVal, type="class")
  cm_val <- confusionMatrix(data = as.factor(val_pred), 
                            reference = as.factor(dataVal$Resultado), 
                            positive='POSITIVO')
  
  cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
  acc_bal_val <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2
  
  accPerNTrees[i,] = c(nTreeList[i], 
                       acc_bal_train, 
                       acc_bal_val)
}

accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line() + geom_point()

# 6. 8 features
nTreeList = c(1, 5, 10, 25, 50, 100, 250, 500, 1000)
accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))

set.seed(55)
for (i in 1:length(nTreeList)){
  rfModel <- randomForest(formula= Resultado ~ (Dimeros.D..quantitativo + DHL + 
                                                  Eosinofilos + ctO2.arterial +
                                                  pO2.arterial + Plaquetas +
                                                  Sat.O2.arterial + Leucocitos), 
                          data= dataTrain, ntree=nTreeList[i], mtry=3)
  
  # Avaliando no conjunto de treinamento
  train_pred <- predict(rfModel, dataTrain, type="class")
  cm_train <- confusionMatrix(data = as.factor(train_pred), 
                              reference = as.factor(dataTrain$Resultado), 
                              positive='POSITIVO')
  
  cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
  acc_bal_train <- (cm_train_relative[1,1] + cm_train_relative[2,2])/2
  
  # Avaliando no conjunto de validacao
  val_pred <- predict(rfModel, dataVal, type="class")
  cm_val <- confusionMatrix(data = as.factor(val_pred), 
                            reference = as.factor(dataVal$Resultado), 
                            positive='POSITIVO')
  
  cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
  acc_bal_val <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2
  
  accPerNTrees[i,] = c(nTreeList[i], 
                       acc_bal_train, 
                       acc_bal_val)
}

accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line() + geom_point()

# 6. Tomando 8 features e a solução com melhor número de árvores
set.seed(55)
rfModel <- randomForest(formula= Resultado ~ (Dimeros.D..quantitativo + DHL + 
                                                Eosinofilos + ctO2.arterial +
                                                pO2.arterial + Plaquetas +
                                                Sat.O2.arterial + Leucocitos), 
                        data= dataTrain, ntree=25, mtry=3)

# Avaliando no conjunto de treinamento
train_pred <- predict(rfModel, dataTrain, type="class")
cm_train <- confusionMatrix(data = as.factor(train_pred), 
                            reference = as.factor(dataTrain$Resultado), 
                            positive='POSITIVO')

cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
acc_bal_train <- (cm_train_relative[1,1] + cm_train_relative[2,2])/2; acc_bal_train

# Avaliando no conjunto de validacao
val_pred <- predict(rfModel, dataVal, type="class")
cm_val <- confusionMatrix(data = as.factor(val_pred), 
                          reference = as.factor(dataVal$Resultado), 
                          positive='POSITIVO')

cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
acc_bal_val <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2; acc_bal_val

# Avaliando no conjunto de teste
test_pred <- predict(rfModel, dataTest, type="class")
cm_test <- confusionMatrix(data = as.factor(test_pred), 
                          reference = as.factor(dataTest$Resultado), 
                          positive='POSITIVO')

cm_test_relative <- calculaMatrizConfusaoRelativa(cm_test)
acc_bal_test <- (cm_test_relative[1,1] + cm_test_relative[2,2])/2; acc_bal_test

