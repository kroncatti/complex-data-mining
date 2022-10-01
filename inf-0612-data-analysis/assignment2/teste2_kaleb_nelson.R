########################################
# Teste 2         
# Nome(s): Kaleb Roncatti de Souza & Nelson Gomes Brasil Junior
########################################

## 1 - Agrupamento

groupsum <- function(df, colgroup, colsum){
  return(tapply(df[[colsum]], df[[colgroup]], sum))
}

# Exemplos no PDF:
dia <- c(01, 01, 02, 02, 03, 03, 04, 04, 05, 05)
cidade <- c('Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas')
chuva <- c(0.15, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45)
chuvas <- data.frame(cidade, dia, chuva); chuvas
groupsum(df = chuvas, colgroup = "cidade", colsum = "chuva")

## 2 - Binario para Decimal

binToDec <- function(...){
  result <- NULL
  for (entry in list(...)){
    max_pow <- length(entry) - 1
    min_pow <- 0
    seq_pow <- 2^(max_pow: min_pow)
    dec_entry <- sum(entry * seq_pow)
    result <- c(result, dec_entry)
  }
  return(result)
}

# Exemplos no PDF:
binToDec(c(1, 0))
binToDec(c(0, 0, 1), c(1, 1))
binToDec(rep(1, 3), rep(0, 2), rep(c(1,0), 2))

## 3 - Ocorrencia de Palavras

wordCount <- function(word, text){
  word <- tolower(word)
  
  # Lowercase
  text <- tolower(text)
  
  # Removendo pontuação
  text <- gsub('[[:punct:] ]+',' ', text)
  
  # Vetorizando
  vectorized_text <- unlist(strsplit(text, split = " "))
  
  counter <- 0
  for (w in vectorized_text){
    if (w == word){
      counter <- counter + 1
    }
  } 
  return(counter)
}

##### Exemplos no PDF:
text <- "O rAto roeu a roupa do Rei de Roma! RainhA raivosa rasgou o resto."
wordCount("rato", text)
wordCount("roma", text)
text <- "A vaca malHada foi molhADA por outra VACA, MOLhada e MALhaDa."
wordCount("outra", text)
wordCount("vaca", text)
wordCount("malhada", text)
text <- "Se a liga me ligasse, eu tambem ligava a liga. Mas a liga nao me liga, eu tambem nao ligo a liga."
wordCount("liga", text)
wordCount("ligasse", text)
