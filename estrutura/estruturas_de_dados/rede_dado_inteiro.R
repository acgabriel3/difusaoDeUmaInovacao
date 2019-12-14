#***
#CAMINHO
#estrutura/estrutura_de_dados/rede_dado_inteiro.R

library(readxl)
library(dplyr)
library(igraph)
library(stringi)

#-Esta funcao deve receber a tabela original dos dados objetos de pesquisa
#-A partir destes dados tratados de maneira adequada, eh construida uma rede 
#-Retorna no formato igraph indirecionada e conectada a qual representa a malha de transportes brasileira
gera_rede_dado_inteiro <- function(tabela) { #qtd linhas da tabela = n
  
  edgelist <- tabela[,c(4, 8, 13)] 
  
  #edgelist$IBGE_A <- as.character(edgelist$IBGE_A)
  #edgelist$IBGE_B <- as.character(edgelist$IBGE_B)
  
  edges <- as.matrix(edgelist[,c(1,2)]) #problema aqui 
  pesos <- edgelist[,3]
  
  rede <- graph_from_edgelist(edges, directed = FALSE) 
  rede <- set_edge_attr(rede, "distanciaMin", value = pesos)
  
  #***
  #existem dois vertices desconectados
  componentes <- components(rede)
  compDesconectado <- V(rede)$name[componentes$membership %in% 2] #complexidade 2n
  rede <- delete.vertices(rede, compDesconectado) #deleta vertices desconectados
  
  #Procura os municipios que nao foram classificados segundo divisao-urbano-regional do IBGE
  fatores <- as.factor(tabela$VAR02)
  diferentes <- levels(fatores)
  vertices_nao_validos <- tabela$IBGE_A[which(tabela$VAR01 == diferentes[91])]
  vertices_nao_validos <- c(vertices_nao_validos, tabela$IBGE_B[which(tabela$VAR02 == diferentes[91])])
  
  rede <- delete.vertices(rede, vertices_nao_validos) #exclui municipios nao classificados da rede
  
  return(rede)
  
}#complexidade n(chao de 2n)
