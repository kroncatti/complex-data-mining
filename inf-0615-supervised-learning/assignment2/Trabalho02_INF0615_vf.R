### ============ Trabalho 01 ============ ###
## Membros do grupo:
# - Kaleb Roncatti de Souza
# - Nelson Gomes Brasil Junior


################################################################################
# Algumas funções que serão utilizadas durante o trabalho
################################################################################
getHypothesis <- function(feature_names, degree){
  
  hypothesis_string <- "hypothesis <- formula(target ~ "
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

# Calcula a matriz de confus?o relativa 
calculaMatrizConfusaoRelativa <- function(cm){
  
  # Aplicamos a transposi??o para garantir que a referencia
  # fique nas linhas e a predicao nas colunas
  cm_absolute = t(cm$table)
  
  # SEMPRE construam e reportem a matriz de confusao relativa!
  cm_relative = cm_absolute
  
  cm_relative[1,1] = round(cm_absolute[1,1]/sum(cm_absolute[1,]), digits=2)
  cm_relative[1,2] = round(cm_absolute[1,2]/sum(cm_absolute[1,]), digits=2)
  cm_relative[2,1] = round(cm_absolute[2,1]/sum(cm_absolute[2,]), digits=2)
  cm_relative[2,2] = round(cm_absolute[2,2]/sum(cm_absolute[2,]), digits=2)
  
  return(cm_relative)  
}

################################################################################
# Carregando algumas bibliotecas
################################################################################

install.packages("glmnet")
install.packages("caret")
install.packages("pROC")

## Pacotes a serem utilizados
library(glmnet)
library(caret)
library(pROC)
library(corrplot)

set.seed(45)
################################################################################
# Análise Exploratória dos Dados
################################################################################

# Carregando as bases de dados
trainSet <- read.csv("proteins_training_set.csv")
valSet <- read.csv("proteins_validation_set.csv")
testSet <- read.csv("proteins_test_set.csv")
sarsSet <- read.csv("SARS_test_set.csv")

# Aalisando os conjuntos de treino,validação e teste
summary(trainSet)
summary(valSet)
summary(testSet)
summary(sarsSet)

dim(trainSet)
dim(valSet)
dim(testSet)
dim(sarsSet)

number_of_sample <- dim(trainSet)[1] + dim(valSet)[1] + dim(testSet)[1]

# Calculando a proporção dos itens
sprintf("proporção do conjunto de treinamento %s", round(dim(trainSet)[1]/number_of_sample, 2))
sprintf("proporção do conjunto de validação %s", round(dim(valSet)[1]/number_of_sample, 2))
sprintf("proporção do conjunto de teste %s", round(dim(testSet)[1]/number_of_sample, 2))

# Definindo o target como factor
trainSet$target <- as.factor(trainSet$target)
valSet$target <- as.factor(valSet$target)
testSet$target <- as.factor(testSet$target)
sarsSet$target <- as.factor(sarsSet$target)


# Verificando a existência de NA's
any(is.na(trainSet))
any(is.na(valSet))

# Verifica Frequencia de cada uma das classes
table(trainSet$target)
table(valSet$target)
table(testSet$target)
table(sarsSet$target)

### Fazendo o balanceamento (por ponderacao da funcao de erro)
classes_frequency = table(trainSet$target)

relative_classes_frequency = classes_frequency/sum(classes_frequency)

w_positive = 1 - relative_classes_frequency[2]
w_negative = 1 - relative_classes_frequency[1]

# Inicializando com zeros o vetor de pesos
weights <- rep(0.0, dim(trainSet)[1])

# Associando o peso dos positivos (w_positive) aos respectivos exemplos
weights[trainSet$target == 1] = w_positive 

# Associando o peso dos negativos (w_negative) aos respectivos exemplos
weights[trainSet$target == 0] = w_negative 

### Normalizacao Z-norma 
mean_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, mean)

sd_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, sd)

trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, mean_features, "-")
trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, sd_features, "/")

valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")

testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, mean_features, "-")
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, sd_features, "/")

sarsSet[,1:(ncol(sarsSet)-1)] <- sweep(sarsSet[,1:(ncol(sarsSet)-1)], 2, mean_features, "-")
sarsSet[,1:(ncol(sarsSet)-1)] <- sweep(sarsSet[,1:(ncol(sarsSet)-1)], 2, sd_features, "/")

################################################################################
# Baseline
################################################################################

feature_names <- colnames(trainSet)[1:(ncol(trainSet)-1)]

baseline_hypothesis <- getHypothesis(feature_names, 1)

# Treinando o modelo
x_train <- model.matrix(baseline_hypothesis, trainSet)
y_train <- trainSet$target

lr_model_baseline <- glmnet(x_train, y_train,  family="binomial",
                            weights = weights,
                            standardize = FALSE, alpha=0, lambda = 1e-6)


trainPred_baseline <- predict(lr_model_baseline, newx = x_train, type="response")

# Convertendo para classe
trainClassPred_baseline <- trainPred_baseline

# Threshold = 0.5 
trainClassPred_baseline[trainPred_baseline >= 0.5] <- 1
trainClassPred_baseline[trainPred_baseline < 0.5] <- 0

CMTrain_baseline <- confusionMatrix(data = as.factor(trainClassPred_baseline), 
                                   reference = as.factor(trainSet$target), 
                                   positive='1')

## Matriz de confusão
CMTrain_baseline$table

## Matriz de confusão relativa
CMRelativeTrain_baseline <- calculaMatrizConfusaoRelativa(CMTrain_baseline)
CMRelativeTrain_baseline

# Predicao no conjunto de validacao
x_val <- model.matrix(baseline_hypothesis, valSet)
y_val <- valSet$target
valPred <- predict(lr_model_baseline, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm_val <- confusionMatrix(data = as.factor(valClassPred), 
                          reference = as.factor(valSet$target), 
                          positive='1')


cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
cm_val_relative

acc_bal_baseline <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2


# Predicao no conjunto de teste
x_test <- model.matrix(baseline_hypothesis, testSet)
y_test <- testSet$target
testPred <- predict(lr_model_baseline, newx = x_test, type="response")

# Threshold = 0.5 
testClassPred <- testPred
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

cm_test <- confusionMatrix(data = as.factor(testClassPred), 
                           reference = as.factor(testSet$target), 
                           positive='1')

cm_test_relative <- calculaMatrizConfusaoRelativa(cm_test)
cm_test_relative


# Verificando a correlação entre as features
correlacao <- cor(trainSet[,feature_names])
corrplot(correlacao, method = "color", type = "upper")

# Removemos as features que possuem correlação alta (abs > 0.6)
feature_names <- c("start_position", "chou_fasman", "emini", 
                   "kolaskar_tongaonkar", "parker", "isoelectric_point", 
                   "aromaticity", "hydrophobicity", "stability")


################################################################################
# Análise via polinômios
################################################################################

acc_train <- c()
acc_val <- c()

# Escolhendo grau entre 1 e 15
for(i in 1:15){  
  
  print(i)
  hypothesis <- getHypothesis(feature_names, i)
  
  # Applying hypothesis and training the model
  x_train <- model.matrix(hypothesis, trainSet)
  y_train <- trainSet$target
  model <- glmnet(x_train, y_train,  family="binomial",
                  weights = weights,
                  standardize = FALSE, alpha=0, lambda = 1e-6)
  
  trainPred <- predict(model, newx = x_train, type="response")
  
  #converting to class
  trainClassPred <- trainPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  trainClassPred[trainPred >= 0.5] <- 1
  trainClassPred[trainPred < 0.5] <- 0
  #trainClassPred
  
  cm <- confusionMatrix(data = as.factor(trainClassPred), 
                        reference = as.factor(trainSet$target), 
                        positive='1')
  
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  # Validation
  x_val <- model.matrix(hypothesis, valSet)
  y_val <- valSet$target
  valPred <- predict(model, newx = x_val, type="response")
  
  #converting to class
  valClassPred <- valPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  
  cm <- confusionMatrix(data = as.factor(valClassPred), 
                        reference = as.factor(valSet$target), 
                        positive='1')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  acc_train[i] <- acc_bal_train 
  acc_val[i] <- acc_bal_val 
}

############ Ploting Acc Balanced ############
plot(acc_train, xlab="Complexity", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val)),
            max(c(acc_train, acc_val))))

axis(1, at=1:length(acc_train), labels=seq(from = 1, to = length(acc_train), by = 1), las=1)
points(acc_val, pch="*", col="blue")
points(rep(acc_bal_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_bal_baseline, length(acc_val)), col="green", lty=2)
legend('topleft', legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

#### Testing #### 
i<- which.max(acc_val)
i

hypothesis <- getHypothesis(feature_names, i)

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$target
model <- glmnet(x_train, y_train,  family="binomial",
                weights = weights,
                standardize = FALSE, alpha=0, lambda = 1e-6)

trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$target), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_test_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_test_bal

################################################################################
# Combinando as features
################################################################################

acc_train <- c()
acc_val <- c()

f01	<- formula(target ~ .)		
f02	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^2)
f03	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^3)
f04	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^4)
f05	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^5)
f06	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^6)
f07	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^7)
f08	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^8)
f09	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^9)
f10	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^10)
f11	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^11)
f12	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^12)
f13	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^13)
f14	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^14)
f15	<- formula(target ~ . +	(start_position + chou_fasman + emini + kolaskar_tongaonkar + isoelectric_point + aromaticity + hydrophobicity + stability)^15)

formulas <- c(f01,f02,f03,f04,
              f05,f06,f07,f08,
              f09,f10,f11,f12,
              f13,f14,f15)

i <- 1
for(f in formulas){  
  print(i)
  # Applying hypothesis and training the model
  x_train <- model.matrix(f, trainSet)
  y_train <- trainSet$target
  model <- glmnet(x_train, y_train,  family="binomial",
                  weights = weights,
                  standardize = FALSE, alpha=0, lambda = 1e-6)
  
  
  trainPred <- predict(model, newx = x_train, type="response")
  
  #converting to class
  trainClassPred <- trainPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  trainClassPred[trainPred >= 0.5] <- 1
  trainClassPred[trainPred < 0.5] <- 0
  #trainClassPred
  
  cm <- confusionMatrix(data = as.factor(trainClassPred), 
                        reference = as.factor(trainSet$target), 
                        positive='1')
  
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  
  # Validation
  x_val <- model.matrix(f, valSet)
  y_val <- valSet$target
  valPred <- predict(model, newx = x_val, type="response")
  
  #converting to class
  valClassPred <- valPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  
  cm <- confusionMatrix(data = as.factor(valClassPred), 
                        reference = as.factor(valSet$target), 
                        positive='1')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  acc_train[i] <- acc_bal_train 
  acc_val[i] <- acc_bal_val 
  
  i <- i + 1
}

############ Ploting Acc Balanced ############
plot(acc_train, xlab="Complexity", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val)),
            max(c(acc_train, acc_val))))

axis(1, at=1:15, labels=seq(from = 1, to = 15, by = 1), las=1)
points(acc_val, pch="*", col="blue")
points(rep(acc_bal_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_bal_baseline, length(acc_val)), col="green", lty=2)
legend('topleft', legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

# Testando
i<- which.max(acc_val)

x_train <- model.matrix(formulas[[i]], trainSet)
y_train <- trainSet$target
model <- glmnet(x_train, y_train,  family="binomial",
                weights = weights,
                standardize = FALSE, alpha=0, lambda = 1e-6)

trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(formulas[[i]], testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

#converting to target
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$target), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_test_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_test_bal

################################################################################
# Usando regularização
################################################################################

acc_train <- c()
acc_val <- c()

# Usaremos a combinação de features de grau 4, pois entendemos que foi a que,
# dentre os graus que apresentaram bons resultados no conjunto de validação, 
# é o que leva a menor esforço compuacional

f	<- formula(target ~ . +	(start_position + 
                           chou_fasman + 
                           emini +
                           kolaskar_tongaonkar + 
                           isoelectric_point + 
                           aromaticity + 
                           hydrophobicity + 
                           stability)^4)

# lambdas da regularização, de 1e-7 a 10
lambdas = 10^(1:-7)

for(i in 1:9){  
  print(i)
  # Applying hypothesis and training the model
  x_train <- model.matrix(f, trainSet)
  y_train <- trainSet$target
  model <- glmnet(x_train, y_train,  family="binomial",
                  weights = weights,
                  standardize = FALSE, alpha=0, lambda = lambdas[i])
  
  trainPred <- predict(model, newx = x_train, type="response")
  
  #converting to target
  trainClassPred <- trainPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  trainClassPred[trainPred >= 0.5] <- 1
  trainClassPred[trainPred < 0.5] <- 0
  #trainClassPred
  
  cm <- confusionMatrix(data = as.factor(trainClassPred), 
                        reference = as.factor(trainSet$target), 
                        positive='1')
  
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  # Validation
  x_val <- model.matrix(f, valSet)
  y_val <- valSet$target
  valPred <- predict(model, newx = x_val, type="response")
  
  #converting to class
  valClassPred <- valPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm <- confusionMatrix(data = as.factor(valClassPred), 
                        reference = as.factor(valSet$target), 
                        positive='1')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  acc_train[i] <- acc_bal_train 
  acc_val[i] <- acc_bal_val 
}


############ Ploting Acc Balanced ############
plot(lambdas, acc_train, xlab="regularization parameter", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val, acc_bal_baseline)),
            max(c(acc_train, acc_val, acc_bal_baseline))), log = 'x')

axis(1, at=lambdas, labels=lambdas, las=1)
points(lambdas, acc_val, pch="*", col="blue")
points(lambdas, rep(acc_bal_baseline, length(acc_val)), pch="o", col="green")

lines(lambdas, acc_train, col="red", lty=2)
lines(lambdas, acc_val, col="blue", lty=2)
lines(lambdas, rep(acc_bal_baseline, length(acc_val)), col="green", lty=2)
legend(1e-7, 0.65, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)


#### Testing #### 
i<- which.max(acc_val)

x_train <- model.matrix(f, trainSet)
y_train <- trainSet$target
model <- glmnet(x_train, y_train,  family="binomial",
                weights = weights,
                standardize = FALSE, alpha=0, lambda = lambdas[i])


x_test <- model.matrix(f, testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$target), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test

################################################################################
# Base SARS
################################################################################

best_model <- getHypothesis(feature_names, 15)

x_train <- model.matrix(best_model, trainSet)
y_train <- trainSet$target
model <- glmnet(x_train, y_train,  family="binomial",
                weights = weights,
                standardize = FALSE, alpha=0, lambda = 1e-6)

trainPred <- predict(model, newx = x_train, type="response")

x_sars <- model.matrix(best_model, sarsSet)
y_sars <- sarsSet$target
sarsPred <- predict(model, newx = x_sars, type="response")

#converting to class
sarsClassPred <- sarsPred

#### THRESHOLD ####
# Threshold = 0.5 
sarsClassPred[sarsPred >= 0.5] <- 1
sarsClassPred[sarsPred < 0.5] <- 0

cm_sars <- confusionMatrix(data = as.factor(sarsClassPred), 
                           reference = as.factor(sarsSet$target), 
                           positive='1')

cm_sars_relative <- calculaMatrizConfusaoRelativa(cm_sars)
cm_sars_relative

acc_sars_bal <- (cm_sars_relative[1,1] + cm_sars_relative[2,2])/2
acc_sars_bal

