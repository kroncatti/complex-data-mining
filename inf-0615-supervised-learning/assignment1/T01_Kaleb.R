### ============ Trabalho 01 ============ ###
## Membros do grupo:
# - Kaleb Roncatti de Souza
# - Nelson Gomes Brasil Junior

# Este eh o codigo a partir do qual voc?s devem desevolver o Trabalho 01


# A funcao abaixo auxilia na escrita dos modelos polinomiais. 
# Parametros:
#  "real_feature_names": conjunto de features continuas que sera considerado na
#                        criacao do modelo polinomial.
#
#  "categorical_feature_names": conjunto de features categoricas que sera 
#                               considerado na  criacao do modelo polinomial. Se
#                                voces desejarem um modelo sem variaveis categoricas
#                               basta nao passar nenhum valor para este parametro
#                               na chamada da funcao
#                       
# "degree": numero inteiro que indica ate qual grau polinomial as features continuas
#           em "real_feature_names" serao elevadas. 
#
# A funcao retorna a hipotese ja definida para realizar o treinamento do modelo. 
# Uma ilustracao de uma funcao similar aparece no Ex02.R na linha 490

getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
  
  hypothesis_string <- "hypothesis <- formula(target ~ "
  for(d in 1:degree){
    for(i in 1:length(real_feature_names)){
      hypothesis_string <- paste(hypothesis_string, 
                                 "I(", real_feature_names[i], "^", d, ") + ",
                                 sep = "")
    }
  }
  
  if(typeof(categorical_feature_names) != "logical"){
    for(i in 1:length(categorical_feature_names)){
      hypothesis_string <- paste(hypothesis_string, 
                                 categorical_feature_names[i], " + ",
                                 sep = "")
    } 
  }
  
  
  hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
  hypothesis_string <- paste(hypothesis_string, ")")
  hypothesis <- eval(parse(text=hypothesis_string))
  return(hypothesis)
}


###======= Desenvolvam o trabalho a partir daqui =======###

############## OBSERVAÇÃO #######################
# As respostas das Tarefas foram respondidas no relatório 


library(corrplot)

# Lendo os dados de treino e de validação
train_data <- read.csv("T01_train_set.csv", 
                       header=TRUE, 
                       stringsAsFactors=TRUE)
val_data <- read.csv("T01_val_set.csv", 
                     header=TRUE, 
                     stringsAsFactors=TRUE)
test_data <- read.csv("T01_test_set.csv", 
                      header=TRUE, 
                      stringsAsFactors=TRUE)

# Verificando a dimensão e o sumário para tirar características básicas do conjunto
dim(train_data)
summary(train_data)
head(train_data, 1)

dim(val_data)
summary(val_data)
head(val_data, 1)

dim(test_data)
summary(test_data)
head(test_data, 1)

# Verificando se temos NaNs
any(is.na(train_data))
any(is.na(val_data))

# Utilizando a metodologia one-hot encoding para lidar com as features que são categóricas
# No caso, temos apenas weekday
train_data$sunday <- as.numeric(train_data$weekday == "Sunday")
train_data$monday <- as.numeric(train_data$weekday == "Monday")
train_data$tuesday <- as.numeric(train_data$weekday == "Tuesday")
train_data$wednesday <- as.numeric(train_data$weekday == "Wednesday")
train_data$thursday <- as.numeric(train_data$weekday == "Thursday")
train_data$friday <- as.numeric(train_data$weekday == "Friday")
train_data$saturday <- as.numeric(train_data$weekday == "Saturday")
train_data$weekday <- NULL

val_data$sunday <- as.numeric(val_data$weekday == "Sunday")
val_data$monday <- as.numeric(val_data$weekday == "Monday")
val_data$tuesday <- as.numeric(val_data$weekday == "Tuesday")
val_data$wednesday <- as.numeric(val_data$weekday == "Wednesday")
val_data$thursday <- as.numeric(val_data$weekday == "Thursday")
val_data$friday <- as.numeric(val_data$weekday == "Friday")
val_data$saturday <- as.numeric(val_data$weekday == "Saturday")
val_data$weekday <- NULL

test_data$sunday <- as.numeric(test_data$weekday == "Sunday")
test_data$monday <- as.numeric(test_data$weekday == "Monday")
test_data$tuesday <- as.numeric(test_data$weekday == "Tuesday")
test_data$wednesday <- as.numeric(test_data$weekday == "Wednesday")
test_data$thursday <- as.numeric(test_data$weekday == "Thursday")
test_data$friday <- as.numeric(test_data$weekday == "Friday")
test_data$saturday <- as.numeric(test_data$weekday == "Saturday")
test_data$weekday <- NULL


# Checando novamente o dataset
summary(train_data)
dim(train_data)
head(train_data, 1)

# Verificando a correlação entre as features com valores numéricos (deixando o target de lado)
correl <- cor(train_data[, 1:17])
corrplot(correl, 
         method = "color", 
         type = "upper", 
         cl.cex = 0.7,
         tl.cex = 0.7)

## Utilizando Z-norm para a normalização das features
# Média das features (training set)
mean_features <- apply(train_data[,1:17], 2, mean)
mean_features
# Desvio padrão das features (training set)
sd_features <- apply(train_data[,1:17], 2, sd)
sd_features

train_data[, 1:17] <- sweep(train_data[, 1:17], 2, mean_features, "-")
train_data[,1:17] <- sweep(train_data[, 1:17], 2, sd_features, "/")
summary(train_data)

# Usando as mesmas médias e desvios padrões do conjunto de validação
val_data[, 1:17] <- sweep(val_data[, 1:17], 2, mean_features, "-")
val_data[, 1:17] <- sweep(val_data[, 1:17], 2, sd_features, "/")
summary(val_data)

# Usando as mesmas médias e desvios padrões do conjunto de testes
test_data[, 1:17] <- sweep(test_data[, 1:17], 2, mean_features, "-")
test_data[, 1:17] <- sweep(test_data[, 1:17], 2, sd_features, "/")
summary(test_data)

# Checando em forma de plota algumas das features normalizadas
plot(train_data[, "global_rate_positive_words"], ylab = "value", ylim=c(-5, 5))
points(train_data[,"global_rate_negative_words"], pch="*", col="blue")
points(train_data[, "n_tokens_title"], pch="+", col="green")

# Encontrando o nome das colunas das features (sem o target)
numeric_cols <- colnames(train_data[, c(1:17)])
categor_cols <- colnames(train_data[, c(19:25)])

# Criando o modelo polinomial de grau um
lr_hypot <- getHypothesis(real_feature_names = numeric_cols,
                          categorical_feature_names = categor_cols,
                          degree = 1)

# Aplicando o modelo de regressão linear
bsln_model <- lm(formula = lr_hypot,
                 data = train_data)
summary(bsln_model)

# Aplicando a predição baseado nos dados de treinamento e validação
prediction_train <- predict(bsln_model, train_data)
head(prediction_train)
prediction_val <- predict(bsln_model, val_data)
head(prediction_val)

# Fazendo o mesmo com o junto de testes
prediction_test_bsln <- predict(bsln_model, test_data)


###################################
####   Define MAE function     ####
MAE <- function(preds, labels){
  mae_values <- sum(abs(preds-labels))/length(preds)
  return(mae_values)
}

# Mean Absolute Error Baseline Train
mae_train_bsln <- MAE(prediction_train, train_data$target)
mae_train_bsln
# Mean Absolute Error Baseline Validation
mae_val_bsln <- MAE(prediction_val, val_data$target)
mae_val_bsln

# Mean Absolute Error Baseline Test
mae_test_bsln <- MAE(prediction_test_bsln, test_data$target)
mae_test_bsln


### Realizando a combinação de features
lr_hypot_01 <- formula(target ~ . +  (n_tokens_title + average_token_length + num_keywords + kw_avg_max + global_subjectivity + 
                                        global_sentiment_polarity + global_rate_positive_words + global_rate_negative_words + 
                                        avg_positive_polarity + avg_negative_polarity + rate_negative_words + log_n_tokens_content + 
                                        log_num_hrefs + root2_num_self_hrefs + log_self_reference_avg_sharess + log_self_reference_max_shares)^2)

lr_hypot_02 <- formula(target ~ . + (n_tokens_title + average_token_length + num_keywords + global_subjectivity + avg_positive_polarity)^2 +
                         (kw_avg_max + global_sentiment_polarity + log_self_reference_avg_sharess + avg_negative_polarity)^2 + 
                         (global_rate_positive_words + log_n_tokens_content + log_num_hrefs)^2 +
                         (global_rate_negative_words + root2_num_self_hrefs + log_self_reference_max_shares)^2)

lr_hypot_03 <- formula(target ~ . + (n_tokens_title + average_token_length + num_keywords + global_subjectivity + avg_positive_polarity)^3 +
                         (kw_avg_max + global_sentiment_polarity + log_self_reference_avg_sharess + avg_negative_polarity)^3 + 
                         (global_rate_positive_words + log_n_tokens_content + log_num_hrefs)^3 +
                         (global_rate_negative_words + root2_num_self_hrefs + log_self_reference_max_shares)^3)

lr_hypot_04 <- formula(target ~ . + (n_tokens_title + average_token_length + num_keywords + global_subjectivity + avg_positive_polarity)^4 +
                         (kw_avg_max + global_sentiment_polarity + log_self_reference_avg_sharess + avg_negative_polarity)^4 + 
                         (global_rate_positive_words + log_n_tokens_content + log_num_hrefs)^4 +
                         (global_rate_negative_words + root2_num_self_hrefs + log_self_reference_max_shares)^4)

lr_hypot_05 <- formula(target ~ . + (n_tokens_title + average_token_length + num_keywords + global_subjectivity + avg_positive_polarity )^5 +
                         (kw_avg_max + global_sentiment_polarity + log_self_reference_avg_sharess + avg_negative_polarity)^5 + 
                         (global_rate_positive_words + log_n_tokens_content + log_num_hrefs)^5 +
                         (global_rate_negative_words + root2_num_self_hrefs + log_self_reference_max_shares)^5)


models <- c(lr_hypot_01, lr_hypot_02, lr_hypot_03, lr_hypot_04, lr_hypot_05)
total_mae_train <- c(length(models))
total_mae_val <- c(length(models))

model_01 <- lm(formula=lr_hypot_01, data=train_data)
prediction_train_01 <- predict(model_01, train_data)

i <- 1
for(f in models){
  print("Creating linear model based on combination of features, this may take some time...")
  print(i)
  
  model <- lm(formula=f, data=train_data)
  
  predict_val_f <- predict(model, val_data)
  predict_train_f <- predict(model, train_data)
  
  mae_train <- MAE(predict_train_f, train_data$target)
  total_mae_train[i] <- mae_train
  
  mae_val <- MAE(predict_val_f, val_data$target)
  total_mae_val[i] <- mae_val
  i <- i + 1
}

plot(total_mae_val, xlab="Model", ylab="Error(MAE)", 
     ylim=c(705, 720), pch="+", col="blue",  xaxt="n")
axis(1, at=1:length(models), labels=seq(from = 1, to = 5, by = 1), las=1)

points(total_mae_train, pch="*", col="red")

lines(total_mae_train, col="red", lty=2)
lines(total_mae_val, col="blue", lty=2)
legend(2, 710, legend=c("Train", "Validation"), 
       col=c("red","blue"), lty=2, cex=0.8)
total_mae_val
total_mae_train

# Encontrando o mínimo MAE do conjunto de validação
best_lr_hypot <- models[[which.min(total_mae_val)]]

# Treinando novamente e aplicando os elementos restantes no conjunto de testes
model <- lm(formula = best_lr_hypot, data = train_data)

prediction_test <- predict(model, test_data)
mae_test <- MAE(prediction_test, test_data$target)
mae_test


### Observando a variação de polinômios de múltiplos graus
f01 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 1)
f02 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 2)
f03 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 3)
f04 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 4)
f05 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 5)
f06 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 6)
f07 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 7)
f08 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 8)
f09 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 9)
f10 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 10)
f11 <- getHypothesis(real_feature_names = numeric_cols, categorical_feature_names = categor_cols, degree = 11)

formulas <- list(f01, f02, f03, f04, f05, f06, f07, f08, f09, f10, f11)
total_mae_train_poly <- c(length(formulas))
total_mae_val_poly <- c(length(formulas))
i <- 1
for(i in 1:11){
  model <- lm(formula=formulas[[i]], data=train_data)
  
  prediction_val_pol <- predict(model, val_data)
  prediction_train_pol <- predict(model, train_data)
  
  mae_train <- MAE(prediction_train_pol, train_data$target)
  total_mae_train_poly[i] <- mae_train
  
  mae_val <- MAE(prediction_val_pol, val_data$target)
  total_mae_val_poly[i] <- mae_val
  i <- i + 1
  
}

summary(model)
plot(total_mae_val_poly, xlab="Polynomial Degree", ylab="Error MAE", 
     ylim=c(700, max(total_mae_val_poly)+15), pch="+", col="blue",  xaxt="n")
axis(1, at=1:length(formulas), labels=seq(from = 1, to = 11, by = 1), las=1)

points(total_mae_train_poly, pch="*", col="red")
points(rep(mae_val_bsln, length(total_mae_val_poly)), pch="o", col="green")

lines(total_mae_train_poly, col="red", lty=2)
lines(total_mae_val_poly, col="blue", lty=2)
lines(rep(mae_val_bsln, length(total_mae_val_poly)), col="green", lty=2)
legend(2, 735, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.7)

# Encontrando o mínimo MAE do conjunto de validação
best_lr_hypot_pol <- formulas[[which.min(total_mae_val_poly)]]

# Treinando novamente e aplicando os elementos restantes no conjunto de testes
model_pol <- lm(formula = best_lr_hypot_pol, data = train_data)

prediction_test_pol <- predict(model_pol, test_data)
mae_test_pol <- MAE(prediction_test_pol, test_data$target)
mae_test_pol
