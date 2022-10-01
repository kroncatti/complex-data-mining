######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   - Kaleb Roncatti de Souza                                        #
#   - Nelson Brasil Junior                                           #
#   -                                                                #
#                                                                    #
######################################################################

######################################################################
# Configurações Preliminares                                         #
######################################################################

# Carregando as bibliotecas
library(corpus)
library(dplyr)
library(udpipe)
library(tidytext)
library(tidyverse)


# Carregando os arquivos auxiliares
source("./ranking_metrics.R", encoding = "UTF-8")
source("./trabalho1_base.R", encoding = "UTF-8")

# Configure aqui o diretório onde se encontram os arquivos do trabalho
# setw("")


######################################################################
#
# Questão 1
#
######################################################################

# Lendo os documentos (artigos da revista TIME)
# sem processamento de texto (não mude essa linha)
docs <- process_data("time.txt", "XX-Text [[:alnum:]]", "Article_0", 
                     convertcase = TRUE, remove_stopwords = FALSE)

# Visualizando os documentos (apenas para debuging)
head(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                        "Query_0", convertcase = TRUE, 
                        remove_stopwords = FALSE)
# Visualizando as consultas (apenas para debuging)
head(queries)

# Exemplo de acesso aos tokens de uma consulta
q1 <- queries[queries$doc_id == "Query_01",]; q1

# Lendo uma lista de vetores de ground_truth
ground_truths <- read.csv("relevance.csv", header = TRUE)

# Visualizando os ground_truths (apenas para debuging)
head(ground_truths)

# Exemplo de acesso vetor de ground_truth da consulta 1:
ground_truths[1,]

# Exemplo de impressão dos ids dos documentos relevantes da consulta 1:
# Visualizando o ranking (apenas para debuging)
names(ground_truths)[ground_truths[1,]==1]


# Computando a matriz de termo-documento
term_freq <- document_term_frequencies(docs); term_freq

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats <- as.data.frame(document_term_frequencies_statistics(term_freq, 
                            k = 1.9, 
                            b = 0.75)); docs_stats
# Visualizando as estatísticas da coleção (apenas para debuging)
head(docs_stats)


######################################################################
#
# Questão 2
#
######################################################################


# query: Elemento da lista de consultas, use a segunda coluna desse 
#        objeto para o cálculo do ranking
# ground_truth: Linha do data.frame de ground_truths referente a query
# stats: data.frame contendo as estatísticas da base
# stat_name: Nome da estatística de interesse, como ela está escrita 
#            no data.frame stats
# top: Tamanho do ranking a ser usado nos cálculos de precisão 
#      e revocação
# text: Título adicional do gráfico gerado, deve ser usado para 
#       identificar a questão e a consulta
computa_resultados <- function(query, ground_truth, stats, stat_name, 
                               top, text) {
  # Criando ranking (função do arquivo base)
  # Dica: você pode acessar a segunda coluna da query a partir de $word ou [["word"]]
  ranking <- get_ranking_by_stats(stat_name = stat_name,
                                  docs_stats = stats,
                                  tokens_query = query$word
                                  )
  # Visualizando o ranking (apenas para debuging)
  head(ranking, n = 5)
  
  # Calculando a precisão
  # Dica: para calcular a precisão, revocação e utilizar a função plot_prec_e_rev,
  # utilize a coluna doc_id do ranking gerado (você pode acessar com $doc_id)
  p <- precision(gtruth = ground_truth,
                 ranking = ranking$doc_id,
                 k = top); p

  # Calculando a revocação
  r <- recall(gtruth = ground_truth,
              ranking = ranking$doc_id,
              k = top); r

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], "\nPrecisão: ", p, 
            "\tRevocação: ", r, "\n"))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking = ranking$doc_id,
                  groundtruth = ground_truth,
                  k = top,
                  text = text
                  ) 
}

# Definindo a consulta 1 
consulta1 <- queries[queries$doc_id == "Query_01",]; consulta1
n_consulta1 <- 1

## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ], 
#                    docs_stats, "nome da statistica", 
#                    top = 15, "titulo")

# Resultados para a consulta 1 e tf_idf
computa_resultados(query = consulta1,
                   ground_truth = ground_truths[n_consulta1, ],
                   stats = docs_stats,
                   stat_name = "tf_idf",
                   top = 20,
                   text = "\n Consulta 1 - tf_idf"
                   )
                   

# Resultados para a consulta 1 e bm25
computa_resultados(query = consulta1,
                   ground_truth = ground_truths[n_consulta1, ],
                   stats = docs_stats,
                   stat_name = "bm25",
                   top = 20,
                   text = "\n Consulta 1 - BM25"
)


# Definindo a consulta 2 
consulta2 <- queries[queries$doc_id == "Query_012",]; consulta2
n_consulta2 <- 12

# Resultados para a consulta 2 e tf_idf
computa_resultados(query = consulta2,
                   ground_truth = ground_truths[n_consulta2, ],
                   stats = docs_stats,
                   stat_name = "tf_idf",
                   top = 20,
                   text = "\n Consulta 2 - tf_idf"
)

# Resultados para a consulta 2 e bm25
computa_resultados(query = consulta2,
                   ground_truth = ground_truths[n_consulta2, ],
                   stats = docs_stats,
                   stat_name = "bm25",
                   top = 20,
                   text = "\n Consulta 2 - BM25"
)


######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################
# Baseando-se nos pares de curva precisão/revocação, gostaríamos de sempre
# encontrar um par cuja precisão e revocação sejam ambos valores elevados para
# um dado k, sabendo-se que existe um compromisso entre estes dois elementos.
# Deseja-se então praticamente "maximizar" o valor de ambos fatores.
# Comparando-se TF_IDF com BM25, BM25 apresentou um resultado melhor para
# ambas as consultas. 
# Pela consulta 1: observe que para k fixado (exemplo, tome k = 5 resultados),
# tanto a precisão quanto a revocação apresentam valores mais elevados pelo BM25.
# Sendo mais específico, com k = 5 resultados em uma busca: 
# TF_IDF --> PRECISÃO ~ 60% - 3 elementos relevantes recuperados dentre os 5 recuperados
# TF_IDF --> REVOCAÇÃO ~ 45%
# BM25 --> PRECISÃO ~ 80% - 4 elementos relevantes recuperados dentre os 5 recuperados
# BM25 --> REVOCAÇÃO ~ 58%> TF_IDF
# 
#
# Pela consulta 2, observa-se praticamente o mesmo fenômeno, tome k = 5,
# TF_IDF --> PRECISÃO ~ 80% - 4 elementos relevantes recuperados dentre os 5 recuperados
# TF_IDF --> REVOCAÇÃO ~ 58% 
# BM25 --> PRECISÃO ~ 100% - 5 elementos relevantes recuperados dentre o total de recuperados (5)
# BM25 --> REVOCAÇÃO ~ 72% > TF_IDF 


######################################################################
#
# Questão 3
#
######################################################################
# Na função process_data está apenas a função para remoção de 
# stopwords está implementada. Sinta-se a vontade para testar 
# outras técnicas de processamento de texto vista em aula.

# Lendo os documentos (artigos da revista TIME) 
# com processamento de texto
docs_proc <- process_data("time.txt", "XX-Text [[:alnum:]]",  
                          "Article_0", convertcase = TRUE, 
                          remove_stopwords = TRUE)
# Visualizando os documentos (apenas para debuging)
head(docs_proc)


# Lendo uma lista de consultas
queries_proc <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                             "Query_0", convertcase = TRUE, 
                             remove_stopwords = TRUE)
# Visualizando as consultas (apenas para debuging)
head(queries_proc)

# Computando a matriz de termo-documento
term_freq_proc <- document_term_frequencies(docs_proc); term_freq_proc

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(term_freq_proc, 
                                                                      k = 1.9, 
                                                                      b = 0.75)); docs_stats_proc


# Definindo a consulta 1 
consulta1_proc <- queries_proc[queries_proc$doc_id == "Query_01",]; consulta1_proc
n_consulta1_proc <- 1
# Resultados para a consulta 1 e tf_idf
computa_resultados(query = consulta1_proc,
                   ground_truth = ground_truths[n_consulta1_proc, ],
                   stats = docs_stats_proc,
                   stat_name = "tf_idf",
                   top = 20,
                   text = "\n Consulta 1 - tf_idf - without stopwords"
)

# Resultados para a consulta 1 e bm25
computa_resultados(query = consulta1_proc,
                   ground_truth = ground_truths[n_consulta1_proc, ],
                   stats = docs_stats_proc,
                   stat_name = "bm25",
                   top = 20,
                   text = "\n Consulta 1 - bm25 - without stopwords"
)


# Definindo a consulta 2 
consulta2_proc <- queries_proc[queries_proc$doc_id == "Query_012",]; consulta2_proc
n_consulta2_proc <- 12

# Resultados para a consulta 2 e tf_idf
computa_resultados(query = consulta2_proc,
                   ground_truth = ground_truths[n_consulta2_proc, ],
                   stats = docs_stats_proc,
                   stat_name = "tf_idf",
                   top = 20,
                   text = "\n Consulta 2 - tf_idf - without stopwords"
)

# Resultados para a consulta 2 e bm25
computa_resultados(query = consulta2_proc,
                   ground_truth = ground_truths[n_consulta2_proc, ],
                   stats = docs_stats_proc,
                   stat_name = "bm25",
                   top = 20,
                   text = "\n Consulta 2 - bm25 - without stopwords"
)

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
# Percebe-se que, para a consulta 1:
# TF_IDF: Apresentou uma melhora de precisão/revocação para k = 3, 7, 11, 
#   porém para k = 5 precisão/revocação foram menores no modelo sem stopwords
# BM25: Apresentou uma melhora a nível precisão e revocação para k = 9
# 
# Para a consulta 2:
# Não houveram alterações a nível precisão/revocação em ambos os modelos
# Como conclusão, a remoção de stopwords apresentou uma melhora no overall

######################################################################
#
# Extra
#
# # Comando para salvar todos os plots gerados e que estão abertos no 
# Rstudio no momemto da execução. Esse comando pode ajudar a comparar 
# os gráfico lado a lado.
# 
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics",
#                              full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", 
#                               full.names = TRUE)
# file.copy(from=plots.png.paths, to="~/Desktop/")
######################################################################
































