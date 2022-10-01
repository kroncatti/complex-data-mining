########################################
# Teste 1a - INF-0612          
# Nome(s): Kaleb Roncatti de Souza e Nelson Brasil Junior
########################################

## Os vetores C, L e V representam os produtos distribuidos nas cidades de Campinas, Limeira e Vinhedo, respectivamente.

C <- c("Xampu", "Sabonete", "Arroz", "Chocolate", "Leite", "Refrigerante", "Queijo", "Suco", "Vinho", "Cerveja")
L <- c("Leite", "Cerveja", "Arroz", "Chocolate")
V <- c("Sabonete", "Detergente", "Refrigerante", "Carne", "Vinho", "Chocolate", "Papel", "Leite", "Iogurte")


## Perguntas:
## Quais os produtos que sao vendidos em Campinas, mas nao sao vendidos em Limeira?

C_not_in_L <- setdiff(C, L); C_not_in_L

## Quais os produtos que sao vendidos em Vinhedo, mas nao sao vendidos em Campinas?

V_not_in_C <- setdiff(V, C); V_not_in_C

## Quais os produtos que sao vendidos em pelo menos uma cidade?

all_products <- union(union(C, L), V); all_products # Already remove duplicates 

## Quais os produtos que sao vendidos em todas as cidades?

market_intersection <- intersect(intersect(C, L), V); market_intersection

## Se a filial de Limeira parar a distribuicao de produtos, a filial de Campinas 
## possui todos os itens necessarios para atender a demanda de Limeira? 

# Vamos procurar quais elementos de L estão em C. Logo após, pegamos os unique's
# Se TODOS os elementos L estão em C --> Resultado do unique = c(TRUE)
# Se ALGUNS elementos de L estão em C --> Resultado do unique = c(TRUE, FALSE)
# Se NENHUM element de L está em C --> Resultado do unique = c(FALSE)
L_elements_in_C <- unique(is.element(L, C)); L_elements_in_C
# Recebemos a resposta c(TRUE) --> TODOS os elementos de Limeira estão -->
# contidos nos elementos de Campinas --> Campinas POSSUE todos os itens
# necessários para atender a demanda de Limeira



