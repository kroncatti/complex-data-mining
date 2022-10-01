#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao       
#                       
# Trabalho Avaliativo 3 
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# - Kaleb Roncatti de Souza                                        
# - Nelson Gomes Brasil Junior                                        
# -                                        
# 
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares 
#----------------------------------------------------------------#
# configure o caminho antes de executar
# setwd("") 
options(warn=-1)
source("./ranking_metrics.R")
source("./trabalho3_base.R")

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
# obtem características de cor, textura e forma  
# para todas as imagens e armazena em matrizes 
# onde uma linha e uma imagem 
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


# analisando rankings
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


# criando descritor concatenando 
desc_all <- cbind(features_s, features_t, features_c)
# criando rankings com descritor concatenado
ranking_concat_europaea <- names(sort(get_distance_vector(
  M = desc_all,
  query = consulta_europaea,
  method = "euclidean")))
ranking_concat_regia <- names(sort(get_distance_vector(
  M = desc_all,
  query = consulta_regia,
  method = "euclidean")))

# analisando os rankings 
metrics_concat_europaea <- analyse_rankings(ranking = ranking_concat_europaea,
                                            ground_truth = ground_truth_europaea)
metrics_concat_regia <- analyse_rankings(ranking = ranking_concat_regia,
                                         ground_truth = ground_truth_regia)



#----------------------------------------------------------------#
# Questao 3 
#----------------------------------------------------------------#

# calculando as distancias, descritor:  histograma de cor 
dist_hist_europaea <- get_distance_vector(M = features_c,
                                          query = consulta_europaea,
                                          method = "euclidean") 
dist_hist_regia <- get_distance_vector(M = features_c,
                                       query = consulta_regia,
                                       method = "euclidean") 

# calculando as distancias, descritor:  textura 
dist_text_europaea <- get_distance_vector(M = features_t,
                                          query = consulta_europaea,
                                          method = "euclidean") 
dist_text_regia <- get_distance_vector(M = features_t,
                                       query = consulta_regia,
                                       method = "euclidean") 

# calculando as distancias, descritor:  forma 
dist_forma_europaea <- get_distance_vector(M = features_s,
                                           query = consulta_europaea,
                                           method = "euclidean") 
dist_forma_regia <- get_distance_vector(M = features_s,
                                        query = consulta_regia,
                                        method = "euclidean") 

# calculando e analisando  rankings combmin
r_combmin_europaea <- names(imagens)[combmin(dist_hist_europaea,
                                             dist_text_europaea,
                                             dist_forma_europaea)]
r_combmin_regia <- names(imagens)[combmin(dist_hist_regia,
                                          dist_text_regia,
                                          dist_forma_regia)]

metrics_combmin_europaea <- analyse_rankings(ranking = r_combmin_europaea,
                                            ground_truth = ground_truth_europaea)
metrics_combmin_regia <- analyse_rankings(ranking = r_combmin_regia,
                                         ground_truth = ground_truth_regia)


# calculando e analisando  rankings combmax
r_combmax_europaea <- names(imagens)[combmax(dist_hist_europaea,
                                             dist_text_europaea,
                                             dist_forma_europaea)]
r_combmax_regia <- names(imagens)[combmax(dist_hist_regia,
                                          dist_text_regia,
                                          dist_forma_regia)]

metrics_combmax_europaea <- analyse_rankings(ranking = r_combmax_europaea,
                                             ground_truth = ground_truth_europaea)
metrics_combmax_regia <- analyse_rankings(ranking = r_combmax_regia,
                                          ground_truth = ground_truth_regia)

# calculando e analisando  rankings combsum
r_combsum_europaea <- names(imagens)[combsum(dist_hist_europaea,
                                             dist_text_europaea,
                                             dist_forma_europaea)]
r_combsum_regia <- names(imagens)[combsum(dist_hist_regia,
                                          dist_text_regia,
                                          dist_forma_regia)]

metrics_combsum_europaea <- analyse_rankings(ranking = r_combsum_europaea,
                                             ground_truth = ground_truth_europaea)
metrics_combsum_regia <- analyse_rankings(ranking = r_combsum_regia,
                                          ground_truth = ground_truth_regia)

# calculando e analisando  rankings borda
r_borda_europaea <- names(imagens)[bordacount(dist_hist_euroaea,
                                             dist_text_europaea,
                                             dist_forma_europaea)]
r_borda_regia <- names(imagens)[bordacount(dist_hist_regia,
                                          dist_text_regia,
                                          dist_forma_regia)]

metrics_borda_europaea <- analyse_rankings(ranking = r_borda_europaea,
                                             ground_truth = ground_truth_europaea)
metrics_borda_regia <- analyse_rankings(ranking = r_borda_regia,
                                          ground_truth = ground_truth_regia)

# calculando a média das precisões medias (APs)

combmin_av_ap_at_k_5 <- (metrics_combmin_europaea$`Average Precision`[1] + metrics_combmin_regia$`Average Precision`[1]) / 2
combmin_av_ap_at_k_10 <- (metrics_combmin_europaea$`Average Precision`[2] + metrics_combmin_regia$`Average Precision`[2]) / 2
combmin_av_ap_at_k_15 <- (metrics_combmin_europaea$`Average Precision`[3] + metrics_combmin_regia$`Average Precision`[3]) / 2
combmin_av_ap_at_k_20 <- (metrics_combmin_europaea$`Average Precision`[4] + metrics_combmin_regia$`Average Precision`[4]) / 2
combmin_av_ap <- rbind(combmin_av_ap_at_k_5, combmin_av_ap_at_k_10, combmin_av_ap_at_k_15, combmin_av_ap_at_k_20)

combmax_av_ap_at_k_5 <- (metrics_combmax_europaea$`Average Precision`[1] + metrics_combmax_regia$`Average Precision`[1]) / 2
combmax_av_ap_at_k_10 <- (metrics_combmax_europaea$`Average Precision`[2] + metrics_combmax_regia$`Average Precision`[2]) / 2
combmax_av_ap_at_k_15 <- (metrics_combmax_europaea$`Average Precision`[3] + metrics_combmax_regia$`Average Precision`[3]) / 2
combmax_av_ap_at_k_20 <- (metrics_combmax_europaea$`Average Precision`[4] + metrics_combmax_regia$`Average Precision`[4]) / 2
combmax_av_ap <- rbind(combmax_av_ap_at_k_5, combmax_av_ap_at_k_10, combmax_av_ap_at_k_15, combmax_av_ap_at_k_20)

combsum_av_ap_at_k_5 <- (metrics_combsum_europaea$`Average Precision`[1] + metrics_combsum_regia$`Average Precision`[1]) / 2
combsum_av_ap_at_k_10 <- (metrics_combsum_europaea$`Average Precision`[2] + metrics_combsum_regia$`Average Precision`[2]) / 2
combsum_av_ap_at_k_15 <- (metrics_combsum_europaea$`Average Precision`[3] + metrics_combsum_regia$`Average Precision`[3]) / 2
combsum_av_ap_at_k_20 <- (metrics_combsum_europaea$`Average Precision`[4] + metrics_combsum_regia$`Average Precision`[4]) / 2
combsum_av_ap <- rbind(combsum_av_ap_at_k_5, combsum_av_ap_at_k_10, combsum_av_ap_at_k_15, combsum_av_ap_at_k_20)

borda_av_ap_at_k_5 <- (metrics_borda_europaea$`Average Precision`[1] + metrics_borda_regia$`Average Precision`[1]) / 2
borda_av_ap_at_k_10 <- (metrics_borda_europaea$`Average Precision`[2] + metrics_borda_regia$`Average Precision`[2]) / 2
borda_av_ap_at_k_15 <- (metrics_borda_europaea$`Average Precision`[3] + metrics_borda_regia$`Average Precision`[3]) / 2
borda_av_ap_at_k_20 <- (metrics_borda_europaea$`Average Precision`[4] + metrics_borda_regia$`Average Precision`[4]) / 2
borda_av_ap <- rbind(borda_av_ap_at_k_5, borda_av_ap_at_k_10, borda_av_ap_at_k_15, borda_av_ap_at_k_20)

av_ap_df <- cbind(combmin_av_ap, combmax_av_ap, combsum_av_ap, borda_av_ap)
rownames(av_ap_df) <- seq(from = 5, to = 20, by = 5)
colnames(av_ap_df) <- c("COMBMIN", "COMBMAX", "COMBSUM", "BORDA")


#----------------------------------------------------------------#
# Questao 3 - RESPONDA:                   
# (i) Escolhendo-se "regia" como referência, vamos fazer uma breve análise visual e
# em seguida analisar o comportamento dos rankings de agregação variando-se o k. 
#   No que diz respeito à análise visual, as fotos de "regia" apresentam um 
# tom de verde bem escuro, e apresentam um fundo acinzentado, definindo texturas e formas
# relativamente bem definidos. Relembrando-se da tarefa anterior, as características 
# concatenadas demonstraram que a forma/shape tinham valores extremamente elevados,
# o que influenciava a concatenação a apresentar valores parecidos com o de forma.
#   No caso dos agregadores para os quais faremos a análise, a ideia é justamente compor
# os resultados de cada um dos rankings para balancear os resultados apresentados por múltiplos
# métodos de rankeamento.
#   Para k = 5, percebemos que o método COMBMAX apresenta os maiores valores de precisão, 
# recall, F1 score, atingindo também o melhor valor de precisão média (AP = 1), 
# mostrando-se como o melhor agregador neste valor de k.
#   Para k = 10, as melhores performances possuem métricas muito próximas (precisão, recall e f1score são idênticos) 
# para os agregadores de BORDA e COMBSUM, porém, BORDA se mostra um pouco superior 
# devido ao valor de average precision mais elevado. É interessante observar também 
# que COMBMAX possui um valor de average precision bastante elevado e significativo
# destoando dentre os outros rankings de agregação, mesmo não atingindo os mesmos níveis de precisão,
# recall e f1 score.
#   Para k = 15 e k = 20, os agregadores de BORDA, COMBSUM E COMBMAX apresentam performances
# muito próximas, com valores de precisão, recall e f1-score idênticos, no entanto, COMBMAX se sobrai
# novamente quando observamos o valor de average precision. 
#
# (j) Para k = 5, observamos que todas as técnicas possuem o valor máximo para a média
# das precisões médias.
# Considerando k = 10, COMBMIN também apresneta a melhor performance, mas tods os outros indicadores
# também possuem performances elevadas. Como já analisamos anteriormente, como a espécie europaea 
# possui textura e forma bem característica, sua precisão média em
# todas as técnicas possui valores bem altos i.e. bem próximos a 1. Então podemos basear a análise na espécie
# regia, o que nos leva a uma situação mais interessante quando k > 10, pois o valor de MAP para a técnica 
# combmin decresce de maneira muito mais acentuada do que as outras técnicas.
# Se para k <= 10 tínhamos combmin >= combmax >= combsum >= borda, para k >= 10 a relação se inverte, 
# ou seja borda >= combsum >= combmax >= combmin. Isto se dá pois a precisão média para a espécie regia
# para combmin piora com o aumento de k, dado que a recuperação da informação via esta técnica não apresenta 
# bons resultados -- apenas 6 dos 10 são recuperados até k = 20. Como recuperamos apenas 2 imagens relevantes
# nas dez primeiras e estas duas imagens são provavelmente as duas primeiras (e a informação AP = 1.0 
# nos mostra isso) e depois mais duas corretas entre k = 10 e k = 15, o valor de precisão nos valores 
# corretos vai decair muito o que vai influenciar a AP para baixo. Para a técnica borda, temos 4 relevantes 
# entre os 5 recuperados (os quatro primeiros, pela mesma lógica anterior) e temos 7 relevantes entre os 
# 10 primeiros. Assim sendo, a AP é mantida em valores mais altos, influenciando então na mudança da 
# MAP entre as duas técnicas.
# Podemos dizer então que o ranking de BORDA foi o melhor e mais consistente para as consultas, pois
# para k < 10 ele está próximo dos demais apesar de ter MAP mais baixo e para k > 10 é o que se mantém
# mais consistente e maior do que os outros rankings.
#----------------------------------------------------------------#


#----------------------------------------------------------------#
# Questao 4 - RESPONDA:                   
# (i) Mantendo-se em torno da análise das duas plantas escolhidas nos últimos itens, 
# podemos observar que:
#   Para o caso da regia, quando nos remetemos à tarefa anterior realizada, 
# devemos lembrar que os descritores de forma/shape mostraram valores extremamente
# elevados e, quando usamos a métrica euclidiana para o cálculo das distâncias, os descritores concatenados
# tiveram seus valores "puxados" para perto dos mesmos resultados do descritor de forma.
# Dado que, neste caso, o descritor de forma não apresentou um resultado  tão satisfatório e que,
# o descritor concatenado é "puxado" para valores altos quando utilizamos a métrica euclidiana,
# o descritor concatenado por consequência apresentou uma performance ruim. Quando observamos o COMBSUM, 
# observamos que a ideia de um agregador representa compor múltiplos rankings, o que levou a um resultado mais satisfatório
# Como a espécie europaea apresenta uma característica bem específica e diferente das demais espécies, 
# percebemos novamente que não houve grande diferença entre a concatenação e a combSUM. 
# Isto é interessante principalmente quando comparamos com a regia, que teve um desempenho tão desigual entre
# as duas técnicas. No caso da concatenação, vimos que as features de forma e textura são justamente as mais
# relevantes para a identificação, sendo que são as duas que justamente distinguem a europaea das demais.



# (ii) 
# Ranking concat
par(mfrow = c(3,2), mar = rep(2, 4))
for (i in c(1:6)){
  mostrarImagemColorida(path_img = ranking_concat_regia[i],
                        nome =paste(c("Position", i, "-", ranking_concat_regia[i]), 
                                    collapse = " "))
}

# Ranking agregado BORDA
par(mfrow = c(3,2), mar = rep(2, 4))
for (i in c(1:6)){
  mostrarImagemColorida(path_img = r_borda_regia[i],
                        nome =paste(c("Position", i, "-", r_borda_regia[i]), 
                                    collapse = " "))
}

# Obtivemos resultados superiores para o ranking agregado de BORDA. Observamos que,
# dos 6 resultados de busca, apenas 2 objetos relevantes foram retornados para o concatenado (precisão = 2/6 = 0.33%)
# e 5 objetos relevantes foram retornados para o ranking de BORDA (precisão = 5/6 = 83%). 
# Ademais, lembramos (da tarefa 2) que o ranking concatenado é "puxado" para valores próximos dos valores do ranking de forma,
# e, neste caso, o ranking de forma tinha apresentado uma performance ruim. Quando observamos os conjuntos de imagem, percebemos
# que o ranking agregado foi capaz de capturar bem as características de cor, forma e textura, enquanto o ranking concatenado
# parece ter focado no ranking de forma de maneira mais específica. Também visualmente, características de cor parecem 
#ter sido praticamente ignoradas no modelo concatenado.
# 
# (iii)
# Visualmente o ranking agregado com o método BORDA pareceu melhor, o que na verdade também é explicado pelas métricas
# O descritor concatenado, como era esperado, parece ter ignorado os aspectos de cor das imagens e dado mais enfoque nas de forma,
# pois podemos perceber que as imagens recuperadas tem certa similaridade nesta feature.
# 
# 
# 
# (iv)
# criando rankings com descritor de cor
ranking_c_biloba <- get_ranking_by_distance(features_c, consulta_biloba)
ranking_c_europaea <- get_ranking_by_distance(features_c, consulta_europaea)
ranking_c_ilex <- get_ranking_by_distance(features_c, consulta_ilex)
ranking_c_monogyna <- get_ranking_by_distance(features_c, consulta_monogyna)
ranking_c_regia <- get_ranking_by_distance(features_c, consulta_regia)

# analisando os rankings 
# analisando rankings gerados com caracteristicas de cor
metrics_c_biloba <- analyse_rankings(ranking_c_biloba, ground_truth_biloba)
metrics_c_europaea <- analyse_rankings(ranking_c_europaea, ground_truth_europaea)
metrics_c_ilex <- analyse_rankings(ranking_c_ilex, ground_truth_ilex)
metrics_c_monogyna <- analyse_rankings(ranking_c_monogyna, ground_truth_monogyna)
metrics_c_regia <- analyse_rankings(ranking_c_regia, ground_truth_regia)

# criando rankings com descritor textura
ranking_t_biloba <- get_ranking_by_distance(features_t, consulta_biloba)
ranking_t_europaea <- get_ranking_by_distance(features_t, consulta_europaea)
ranking_t_ilex <- get_ranking_by_distance(features_t, consulta_ilex)
ranking_t_monogyna <- get_ranking_by_distance(features_t, consulta_monogyna)
ranking_t_regia <- get_ranking_by_distance(features_t, consulta_regia)

# analisando os rankings 
# analisando rankings gerados com caracteristicas de textura
metrics_t_biloba <- analyse_rankings(ranking_t_biloba, ground_truth_biloba)
metrics_t_europaea <- analyse_rankings(ranking_t_europaea, ground_truth_europaea)
metrics_t_ilex <- analyse_rankings(ranking_t_ilex, ground_truth_ilex)
metrics_t_monogyna <- analyse_rankings(ranking_t_monogyna, ground_truth_monogyna)
metrics_t_regia <- analyse_rankings(ranking_t_regia, ground_truth_regia)


# criando rankings com descritor de forma
ranking_s_biloba <- get_ranking_by_distance(features_s, consulta_biloba)
ranking_s_europaea <- get_ranking_by_distance(features_s, consulta_europaea)
ranking_s_ilex <- get_ranking_by_distance(features_s, consulta_ilex)
ranking_s_monogyna <- get_ranking_by_distance(features_s, consulta_monogyna)
ranking_s_regia <- get_ranking_by_distance(features_s, consulta_regia)

# analisando os rankings 
# analisando rankings gerados com caracteristicas de forma
metrics_s_biloba <- analyse_rankings(ranking_s_biloba, ground_truth_biloba)
metrics_s_europaea <- analyse_rankings(ranking_s_europaea, ground_truth_europaea)
metrics_s_ilex <- analyse_rankings(ranking_s_ilex, ground_truth_ilex)
metrics_s_monogyna <- analyse_rankings(ranking_s_monogyna, ground_truth_monogyna)
metrics_s_regia <- analyse_rankings(ranking_s_regia, ground_truth_regia)

# Criando os elementos necessários para o cálculo do ranking concatenado para as plantas restantes (que não usamos nas outras análises)
# criando rankings com descritor concatenado
ranking_concat_biloba <- names(sort(get_distance_vector(
  M = desc_all,
  query = consulta_biloba,
  method = "euclidean")))

ranking_concat_ilex <- names(sort(get_distance_vector(
  M = desc_all,
  query = consulta_ilex,
  method = "euclidean")))

ranking_concat_monogyna <- names(sort(get_distance_vector(
  M = desc_all,
  query = consulta_monogyna,
  method = "euclidean")))

# analisando os rankings 
metrics_concat_biloba <- analyse_rankings(ranking = ranking_concat_biloba,
                                            ground_truth = ground_truth_biloba)
metrics_concat_ilex <- analyse_rankings(ranking = ranking_concat_ilex,
                                          ground_truth = ground_truth_ilex)
metrics_concat_monogyna <- analyse_rankings(ranking = ranking_concat_monogyna,
                                          ground_truth = ground_truth_monogyna)


# Criando os elementos necessários para o cálculo dos rankings para agregador de borda para as plantas restantes (que não usamos nas outras análises)
# calculando as distancias, descritor:  histograma de cor 
dist_hist_biloba <- get_distance_vector(M = features_c,
                                          query = consulta_biloba,
                                          method = "euclidean") 
dist_hist_ilex <- get_distance_vector(M = features_c,
                                       query = consulta_ilex,
                                       method = "euclidean") 
dist_hist_monogyna <- get_distance_vector(M = features_c,
                                       query = consulta_monogyna,
                                       method = "euclidean") 

# calculando as distancias, descritor:  textura 
dist_text_biloba <- get_distance_vector(M = features_t,
                                        query = consulta_biloba,
                                        method = "euclidean") 
dist_text_ilex <- get_distance_vector(M = features_t,
                                      query = consulta_ilex,
                                      method = "euclidean") 
dist_text_monogyna <- get_distance_vector(M = features_t,
                                          query = consulta_monogyna,
                                          method = "euclidean")

# calculando as distancias, descritor:  forma 
dist_forma_biloba <- get_distance_vector(M = features_s,
                                        query = consulta_biloba,
                                        method = "euclidean") 
dist_forma_ilex <- get_distance_vector(M = features_s,
                                      query = consulta_ilex,
                                      method = "euclidean") 
dist_forma_monogyna <- get_distance_vector(M = features_s,
                                          query = consulta_monogyna,
                                          method = "euclidean")

# Calculando os rankings
r_borda_biloba <- names(imagens)[bordacount(dist_hist_biloba,
                                              dist_text_biloba,
                                              dist_forma_biloba)]
r_borda_ilex <- names(imagens)[bordacount(dist_hist_ilex,
                                           dist_text_ilex,
                                           dist_forma_ilex)]
r_borda_monogyna <- names(imagens)[bordacount(dist_hist_monogyna,
                                              dist_text_monogyna,
                                              dist_forma_monogyna)]

# analisando os rankings 
# analisando rankings gerados com caracteristicas concatenadas
metrics_borda_biloba <- analyse_rankings(ranking = r_borda_biloba,
                                           ground_truth = ground_truth_biloba)
metrics_borda_ilex <- analyse_rankings(ranking = r_borda_ilex,
                                        ground_truth = ground_truth_ilex)
metrics_borda_monogyna <- analyse_rankings(ranking = r_borda_monogyna,
                                           ground_truth = ground_truth_monogyna)



map_cor_k_10 <- (metrics_c_europaea$`Average Precision`[2] + 
                   metrics_c_regia$`Average Precision`[2] + 
                   metrics_c_biloba$`Average Precision`[2]+
                   metrics_c_ilex$`Average Precision`[2]+
                   metrics_c_monogyna$`Average Precision`[2]) / 5

map_shape_k_10 <- (metrics_s_europaea$`Average Precision`[2] + 
                      metrics_s_regia$`Average Precision`[2] + 
                      metrics_s_biloba$`Average Precision`[2]+
                      metrics_s_ilex$`Average Precision`[2]+
                      metrics_s_monogyna$`Average Precision`[2]) / 5

map_texture_k_10 <- (metrics_t_europaea$`Average Precision`[2] + 
                      metrics_t_regia$`Average Precision`[2] + 
                      metrics_t_biloba$`Average Precision`[2]+
                      metrics_t_ilex$`Average Precision`[2]+
                      metrics_t_monogyna$`Average Precision`[2]) / 5


map_borda_k_10 <- (metrics_borda_europaea$`Average Precision`[2] + 
                     metrics_borda_regia$`Average Precision`[2] + 
                     metrics_borda_biloba$`Average Precision`[2]+
                     metrics_borda_ilex$`Average Precision`[2]+
                     metrics_borda_monogyna$`Average Precision`[2]) / 5

map_concat_k_10 <- (metrics_concat_europaea$`Average Precision`[2] + 
                      metrics_concat_regia$`Average Precision`[2] + 
                      metrics_concat_biloba$`Average Precision`[2]+
                      metrics_concat_ilex$`Average Precision`[2]+
                      metrics_concat_monogyna$`Average Precision`[2]) / 5

MAP <- c(map_borda_k_10, map_concat_k_10, map_cor_k_10, map_texture_k_10, map_shape_k_10)
names(MAP) <- c('borda', 'concatenado', 'cor', 'textura', 'forma')
df <- data.frame(MAP); df

# Calculando a MAP em top 10, observamos que a técnica BORDA foi melhor do que as outras e novamente a de forma dominou o ranking concatenado.
# A técnica Borda apresentou valores melhores para todos as espécies de plantas analisados. Tal técnica, como agregadora, se mostrou como uma boa
# composição das múltiplas features que extraímos das imagens.
# A de cor também apresentou um resultado um pouco melhor do que as features de textura, forma e a concatenada. Lembrando que, para cores tínhamos
# um valor de features muito mais elevados do que os descritores de forma e textura, possivelmente extraindo mais características de cor das imagens do que
# os outros descritores foram capaz de extrair para forma e textura.
# Visualmente falando, também observamos evidentemente a diferença de cores nas imagens apresentadas e estas diferenças ficam ainda mais evidentes
# entre espécies distintas. Ao analisar as fotos, vemos também que algumas espécies são parecidas quanto à forma e textura,
# o que pode ter levado a uma performance um pouco pior.
# 
#----------------------------------------------------------------#