#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao                             #
#                                                                #
# Trabalho Avaliativo 2                                          #
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:                        #
# - Kaleb Roncatti de Souza                                      #
# - Nelson Gomes Brasil Júnior                                   #
# -                                                              #
#                                                                #
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares   
#----------------------------------------------------------------#
# configure o caminho antes de executar
# setwd("") 
options(warn=-1)
source("./ranking_metrics.R")
source("./trabalho2_base.R")

# caminho da pasta de imagens
path_plantas = './plantas'

#----------------------------------------------------------------#
# Leitura das imagens                 
#----------------------------------------------------------------#
imagens <- read_images(path = path_plantas)

#----------------------------------------------------------------#
# Obtem classe de cada imagem             
#----------------------------------------------------------------#
nome_classes <- get_classes(path = path_plantas)

#----------------------------------------------------------------#
# obtem ground_truth para cada classe 
#----------------------------------------------------------------#
ground_truth_biloba <- get_ground_truth(path = path_plantas,
                                        classes = nome_classes,
                                        classe_relevante = "biloba")
ground_truth_europaea <- get_ground_truth(path = path_plantas,
                                          classes = nome_classes,
                                          classe_relevante = "europaea")
ground_truth_ilex <- get_ground_truth(path = path_plantas,
                                      classes = nome_classes,
                                      classe_relevante = "ilex")
ground_truth_monogyna <- get_ground_truth(path = path_plantas,
                                          classes = nome_classes,
                                          classe_relevante = "monogyna")
ground_truth_regia <- get_ground_truth(path = path_plantas,
                                       classes = nome_classes,
                                       classe_relevante = "regia")



#----------------------------------------------------------------#
# Questao 1                               
#----------------------------------------------------------------#

# obtem caracteristicas de cor  
hist_cor_desc <- function(img){
  # The last channel represents the color, RED, GREEN and BLUE, respectively
  r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts
  g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts
  b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts
  # Create a vector with all the features together
  return (c(r, g, b))
}

# obtem caracteristicas de textura   
lbp_desc <- function(img){
  # "Removing" the color channel as requested and keeping gray
  print("Processing...")
  img_gray <- grayscale(img)
  
  min_v<-min(img_gray)
  max_v<-max(img_gray)
  img_gray<-((img_gray-min_v)/(max_v-min_v))*255
  
  # Applying LBP
  lbp_full <- lbp(img_gray[,,1,1], 1)
  # Taking only the uniform portion
  lbp_unif <- hist(lbp_full$lbp.u2, plot=FALSE, breaks=59)$counts
  return(c(lbp_unif))
}


# obtem caracteristicas de forma 
Momentos <-function(img){
  
  centroide <- function(M) {
    c(momento(M, 1, 0) / momento(M, 0, 0),
      momento(M, 0, 1) / momento(M, 0, 0))
  }
  
  momento <- function(M, p, q, central = FALSE) {
    r <- 0
    if (central) {
      c <- centroide(M)
      x <- c[1]
      y <- c[2]
    } else {
      x <- 0
      y <- 0
    }
    for (i in 1:nrow(M))
      for (j in 1:ncol(M))
        r <- r + (i - x)^p * (j - y)^q * M[i,j]  
    return(r)
  }
  print("Processing...")
  # "Removing" the color channel as requested and keeping gray
  img_gray <- grayscale(img)[,,1,1]
  
  min_v<-min(img_gray)
  max_v<-max(img_gray)
  img_gray<-((img_gray-min_v)/(max_v-min_v))*255
  
  features <- NULL
  for(i in 0:2){
    for(j in 0:2){
      # Each repetition adds a new momentum at a new column 
      features <- cbind(features, momento(img_gray, i,j, central=TRUE))
    }
  }
  return(features)
}

#----------------------------------------------------------------#
# obtem caracteristicas de cor, textura e forma para todas as imagens e 
# armazena em matrizes onde uma linha representa uma imagem 
features_c <- t(sapply(imagens, hist_cor_desc))
rownames(features_c) <- names(imagens)
features_t <- t(as.data.frame(lapply(imagens, lbp_desc)))
rownames(features_t) <- names(imagens)
features_s <- t(sapply(imagens, Momentos))
rownames(features_s) <- names(imagens)

#----------------------------------------------------------------#
# Questao 2                               
#----------------------------------------------------------------#

# definindo as consultas
# obs.:  use o caminho completo para a imagem
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
consulta_regia <- "./plantas/regia_07.jpg"

# visualizando as consultas
par(mfrow = c(3,3), mar = rep(2, 4))
mostrarImagemColorida(path_img = consulta_biloba, nome = "biloba_02.jpg")
mostrarImagemColorida(path_img = consulta_europaea, nome = "europaea_01.jpg")
mostrarImagemColorida(path_img = consulta_ilex, nome = "ilex_08.jpg")
mostrarImagemColorida(path_img = consulta_monogyna, nome = "monogyna_04.jpg")
mostrarImagemColorida(path_img = consulta_regia, nome = "regia_07.jpg")

#-----------------------------#
# construindo rankings  

# Function created to calculate the ranking between the query and features
# We used the euclidean distance as method for comparison between images
# all queries are using this same function
ranking_distancia <- function(features, consulta){
  distancias <- lapply(rownames(features),
                       function(x) dist(features[c(consulta, x),],
                                        method = "euclidean"))
  distancias <- unlist(distancias)
  names(distancias) <- names(imagens)
  return(names(sort(distancias)))
}

# para cada uma das 5 consultas, construa um ranking com base na cor
ranking_c_biloba <- ranking_distancia(features = features_c,
                                      consulta = consulta_biloba)
ranking_c_europaea <- ranking_distancia(features = features_c,
                                        consulta = consulta_europaea)
ranking_c_ilex <- ranking_distancia(features = features_c,
                                    consulta = consulta_ilex)
ranking_c_monogyna <- ranking_distancia(features = features_c,
                                        consulta = consulta_monogyna)
ranking_c_regia <- ranking_distancia(features = features_c,
                                     consulta = consulta_regia)

# para cada uma das 5 consultas, construa um ranking com base na textura
ranking_t_biloba <- ranking_distancia(features = features_t,
                                      consulta = consulta_biloba)
ranking_t_europaea <- ranking_distancia(features = features_t,
                                        consulta = consulta_europaea)
ranking_t_ilex <- ranking_distancia(features = features_t,
                                    consulta = consulta_ilex)
ranking_t_monogyna <- ranking_distancia(features = features_t,
                                        consulta = consulta_monogyna)
ranking_t_regia <- ranking_distancia(features = features_t,
                                     consulta = consulta_regia)

# para cada uma das 5 consultas, construa um ranking com base na forma
ranking_s_biloba <- ranking_distancia(features = features_s,
                                      consulta = consulta_biloba)
ranking_s_europaea <- ranking_distancia(features = features_s,
                                        consulta = consulta_europaea)
ranking_s_ilex <- ranking_distancia(features = features_s,
                                    consulta = consulta_ilex)
ranking_s_monogyna <- ranking_distancia(features = features_s,
                                        consulta = consulta_monogyna)
ranking_s_regia <- ranking_distancia(features = features_s,
                                     consulta = consulta_regia)

#-----------------------------#
# comparando  rankings                              

## utilize as funções do arquivo ranking_metrics.R para calcular 
# a precisão, revocação, taxa F1 e precisão média nos 
# top 5, 10, 15 e 20

analyse_rankings <- function(ranking, ground_truth) {
  top_vals = seq(from = 5, to = 20, by = 5)
  df <- NULL
  # Looping through all the desired values
  for (top_val in top_vals){
    precisao <- precision(gtruth = ground_truth, ranking = ranking, k = top_val)
    revocacao <- recall(gtruth = ground_truth, ranking = ranking, k = top_val)
    taxa_f1 <- f1_score(gtruth = ground_truth, ranking = ranking, k = top_val)
    precisao_media <- ap(gtruth = ground_truth, ranking = ranking, k = top_val)
    df <- rbind(df, c(precisao, revocacao, taxa_f1, precisao_media))
  }
  colnames(df) <- c("Precision", "Recall", "F1 Score", "Average Precision")
  rownames(df) <- top_vals
  return(as.data.frame(df))
}

# analisando rankings gerados com caracteristicas de cor
metrics_c_biloba <- analyse_rankings(ranking = ranking_c_biloba,
                                     ground_truth = ground_truth_biloba)
metrics_c_europaea <- analyse_rankings(ranking = ranking_c_europaea,
                                       ground_truth = ground_truth_europaea)
metrics_c_ilex <- analyse_rankings(ranking = ranking_c_ilex,
                                   ground_truth = ground_truth_ilex)
metrics_c_monogyna <- analyse_rankings(ranking = ranking_c_monogyna,
                                       ground_truth = ground_truth_monogyna)
metrics_c_regia <- analyse_rankings(ranking = ranking_c_regia,
                                    ground_truth = ground_truth_regia)


# analisando rankings gerados com caracteristicas de textura
metrics_t_biloba <- analyse_rankings(ranking = ranking_t_biloba,
                                     ground_truth = ground_truth_biloba)
metrics_t_europaea <- analyse_rankings(ranking = ranking_t_europaea,
                                       ground_truth = ground_truth_europaea)
metrics_t_ilex <- analyse_rankings(ranking = ranking_t_ilex,
                                   ground_truth = ground_truth_ilex)
metrics_t_monogyna <- analyse_rankings(ranking = ranking_t_monogyna,
                                       ground_truth = ground_truth_monogyna)
metrics_t_regia <- analyse_rankings(ranking = ranking_t_regia,
                                    ground_truth = ground_truth_regia)

# analisando rankings gerados com caracteristicas de forma
metrics_s_biloba <- analyse_rankings(ranking = ranking_s_biloba,
                                     ground_truth = ground_truth_biloba)
metrics_s_europaea <- analyse_rankings(ranking = ranking_s_europaea,
                                       ground_truth = ground_truth_europaea)
metrics_s_ilex <- analyse_rankings(ranking = ranking_s_ilex,
                                   ground_truth = ground_truth_ilex)
metrics_s_monogyna <- analyse_rankings(ranking = ranking_s_monogyna,
                                       ground_truth = ground_truth_monogyna)
metrics_s_regia <- analyse_rankings(ranking = ranking_s_regia,
                                    ground_truth = ground_truth_regia)


#----------------------------------------------------------------#
# Questao 2 - RESPONDA:                   
# (a) Escolha uma consulta para analisar mais detalhadamente e
# responda: Para essa consulta qual descritor retornou o melhor
# ranking? Lembre-se de analisar visualmente as imagens da classe,
# contextualizando o que foi extraído em cada descritor. Também
# aponte pontos fortes e fracos dos descritores usados que podem
# justificar esse comportamento.
#                                         
# Resposta: 
# Escolhendo-se a consulta 1 (biloba_02.jpg), é possível perceber que o                                         
# descritor de CORES se mostrou com o melhor ranking, recuperando todas as imagens relevantes nas primeiras 10 consultas.
# Isto também se evidencia através as métricas de precisão, recall e f1 score que, para todos os k que analisamos,                                       
# se mostraram superiores com relação à textura e forma. Observando-se visualmente                                         
# o conjunto de fotos, conseguimos perceber que a biloba apresenta um verde um pouco
# mais claro, que não se assemelha tanto com os outros tipos de plantas, se demonstrando
# como uma cor bem definida e um pouco mais isolada das outras plantas.
# No que diz respeito às outras features, analisando-se visualmente, tanto a textura quanto o shape se mostram
# relativamente bem definidos, porém, o resultado da análise de distâncias euclidianas através das métricas para essas
# características apresentou um resultado inferior ao de cores. Uma possível explicação
# para o desempenho inferior pode estar ligada à quantidade de features disponíveis de cada modalidade
# para cores, temos 765 features por amostra, para textura temos 58 e para forma temos apenas 9. Ou seja,
# essa quantidade baixa de features de textura/forma não foi capaz de capturar tão bem as especificidades
# das imagens tão bem quanto as features de cor, as quais são mais específicas.
#   Ademais, quando analisamos a métrica de precisão de maneira mais específica, obtivemos
# 100% de precisão para k entre 1 e 10 com a característica de cor, significando que, se olharmos
# para até o top 10 de elementos na busca, todos os elementos retornados são pertinentes (10 elementos do conjunto biloba).
# Já para características de forma e textura, mesmo quando puxamos o top 20 de elementos na busca não conseguimos 
# obter todas as 10 imagens de biloba, mostrando superioridade nas características de cor.
#   Por fim, também percebemos que o cálculo das características de forma e textura são bem mais lentos com relação
# à obtenção das características de cor.
#
# (b) Considerando as 5 consultas definidas, calcule a média das
# precisões médias em top 10. Avaliando essa medida, qual descritor
# obteve melhores rankings? Lembre-se que para justificar suas 
# resposta, você pode usar complementar sua análise usando também
# outras medidas de avaliação de ranking vistas na Aula 1.

color_average_ap_at_k_10 <- (metrics_c_biloba$`Average Precision`[2] + metrics_c_europaea$`Average Precision`[2] + metrics_c_ilex$`Average Precision`[2] + metrics_c_monogyna$`Average Precision`[2] + metrics_c_regia$`Average Precision`[2]) / 5; color_average_ap_at_k_10
color_average_precision_at_k_10 <- (metrics_c_biloba$`Precision`[2] + metrics_c_europaea$`Precision`[2] + metrics_c_ilex$`Precision`[2] + metrics_c_monogyna$`Precision`[2] + metrics_c_regia$`Precision`[2]) / 5; color_average_precision_at_k_10

texture_average_ap_at_k_10 <- (metrics_t_biloba$`Average Precision`[2] + metrics_t_europaea$`Average Precision`[2] + metrics_t_ilex$`Average Precision`[2] + metrics_t_monogyna$`Average Precision`[2] + metrics_t_regia$`Average Precision`[2]) / 5; texture_average_ap_at_k_10
texture_average_precision_at_k_10 <- (metrics_t_biloba$`Precision`[2] + metrics_t_europaea$`Precision`[2] + metrics_t_ilex$`Precision`[2] + metrics_t_monogyna$`Precision`[2] + metrics_t_regia$`Precision`[2]) / 5; texture_average_precision_at_k_10


shape_average_ap_at_k_10 <- (metrics_s_biloba$`Average Precision`[2] + metrics_s_europaea$`Average Precision`[2] + metrics_s_ilex$`Average Precision`[2] + metrics_s_monogyna$`Average Precision`[2] + metrics_s_regia$`Average Precision`[2]) / 5; shape_average_ap_at_k_10
shape_average_precision_at_k_10 <- (metrics_s_biloba$`Precision`[2] + metrics_s_europaea$`Precision`[2] + metrics_s_ilex$`Precision`[2] + metrics_s_monogyna$`Precision`[2] + metrics_s_regia$`Precision`[2]) / 5; shape_average_precision_at_k_10

# Resposta: 
# Através da métrica da média das precisões médias em top 10 calculada acima, 
# também chegamos no melhor descritor como sendo o descritor de cores. Também calculamos
# a média das precisões médias dentre as diferentes consultas (top 10) e o valor mais significativo
# também se mostrou como sendo as características de cores. É interessante ressaltar que,
# utilizando essas métricas específicas, estamos compondo os resultados de diferentes 
# consultas, cada uma para uma planta diferente, diminuindo o viés do resultado para a pesquisa de uma                                         
# planta específica como fizemos puramente para cada uma das consultas separadamente.

#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Questao 3
#----------------------------------------------------------------#
# concatenando caracteristicas                      

## obter vetores finais de caracteristicas pela concatenação de 
# cada tipo de caracteristica (cor, textura e forma):
features_concat = cbind(features_s, features_t, features_c)

# gerar novos rankings
ranking_concat_biloba <- ranking_distancia(features = features_concat,
                                           consulta = consulta_biloba)
ranking_concat_europaea <- ranking_distancia(features = features_concat,
                                             consulta = consulta_europaea)
ranking_concat_ilex <- ranking_distancia(features = features_concat,
                                         consulta = consulta_ilex)
ranking_concat_monogyna <- ranking_distancia(features = features_concat,
                                             consulta = consulta_monogyna)
ranking_concat_regia <- ranking_distancia(features = features_concat,
                                          consulta = consulta_regia)

# analisando rankings gerados com caracteristicas concatenadas
metrics_concat_biloba <- analyse_rankings(ranking = ranking_concat_biloba,
                                          ground_truth = ground_truth_biloba)
metrics_concat_europaea <- analyse_rankings(ranking = ranking_concat_europaea,
                                            ground_truth = ground_truth_europaea)
metrics_concat_ilex <- analyse_rankings(ranking = ranking_concat_ilex,
                                        ground_truth = ground_truth_ilex)
metrics_concat_monogyna <- analyse_rankings(ranking = ranking_concat_monogyna,
                                            ground_truth = ground_truth_monogyna)
metrics_concat_regia <- analyse_rankings(ranking = ranking_concat_regia,
                                         ground_truth = ground_truth_regia)


#----------------------------------------------------------------#
# Questao 3 - RESPONDA:  
# (a) Qual o impacto dessas alterações nas medidas de avaliação
# calculadas?

# Resposta: 
# Observando-se atentivamente as métricas calculadas em cima dos rankings, a concatenação 
# de todas as features fez com que os valores das métricas se aproximassem bastante das características de shape/forma. 
# ou seja, observa-se que, para consultas em que as features de shape tiveram bom resultado, a concatenação também
# apresentou um bom resultado.  
#
# (b) Os descritores combinados apresentaram melhores resultados?
# Justifique sua resposta.

# Resposta: 
# Não, como descrito no último item, o resultado com as features concatenadas
# se aproximou bastante às características de shape/forma. Usando como exemplo a consulta
# analisada no exercício 2 (consulta_biloba), no caso das features isoladas/separadas, 
# conseguimos ver que as características de cor se mostraram muito mais precisas
# para a consulta. Pegando a mesma consulta e usando-se as features concatenadas, o resultado
# foi inferior e muito próximo ao das features de shape/forma. 
# Ainda para outra consulta, a consulta_regia, observa-se o mesmo comportamento
# com relação aos resultados/métricas. As features concatenadas apresentaram resultados muito próximos
# dos resultados de feature de shape, sendo que as features de textura se mostraram com métricas/resultados
# melhores para tal consulta.
#
# (c) Você acredita que algum dos descritores apresentou maior
# influência na combinação? Justifique sua resposta.
# 
# Resposta:
# Sim, a princípio, imaginamos que os descritores de cor apresentariam uma maior influência
# na combinação, dado que possuímos MUITO mais features em tal descritor quando comparado
# com shape e texture no nosso exercício. Porém, após observamos os valores calculados 
# das features utilizando o comando summary() de maneira mais atentiva (análise de max/min), 
# percebemos que os valores das features de shape são bem mais elevados do que os outros
# descritores. Na ordem dos valores mais expressivos, observamos:
# features(shape) > features(texture) > features(color)
#   Por causa destes valores elevados e de grandes variações nestes descritores, o cálculo
# de rankings utilizando as distâncias euclidianas acaba "pesando" mais para os descritores
# de shape. Assim sendo, mesmo que tenhamos mais descritores de cor, o fato dos descritores de 
# shape/forma apresentarem valores mais expressivos acabam gerando um viés maior em cima
# dos descritores de forma, gerando uma maior influência neste elemento.
#   Uma possível maneira de atenuarmos este efeito seria usando técnicas de normalizalização,
# para que todas as features/descritores possuam o mesmo intervalo de valores antes de concatenarmos.
#----------------------------------------------------------------#
