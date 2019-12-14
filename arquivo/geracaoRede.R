#Arquivo de como foram geradas as funcoes para rede e tabela
library(readxl)
library(igraph)

#Limpeza dos dados disponiveis deixando-os prontos para o uso segundo a biblioteca netdiffuse

tabela <- read_excel("dados/Base_de_dados_ligacoes_rodoviarias_e_hidroviarias_2016.xlsx", sheet = "Base de dados")

#Essa parte deve fazer parte de um script de integracao
colnames(tabela)[13] <- "tempoDesloc" #Refere-se a coluna que informa a distancia em minutos
colnames(tabela)[4] <- "IBGE_A" #ego
tabela[[4]] <- as.character(tabela[[4]])
colnames(tabela)[8] <- "IBGE_B" #alter-ego
tabela[[8]] <- as.character(tabela[[8]])


#tabela <- tabela[,c(2,3,4,5,6,7,8,9,13)] #Seleciona somente as colunas necessárias, referentes aos municípios, seu dados e a respectiva distância

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
compDesconectado <- V(rede)$name[componentes$membership %in% 2]
rede <- delete.vertices(rede, compDesconectado)


fatores <- as.factor(tabela$VAR02)
diferentes <- levels(fatores)
vertices_nao_validos <- tabela$IBGE_A[which(tabela$VAR01 == diferentes[91])]
vertices_nao_validos <- c(vertices_nao_validos, tabela$IBGE_B[which(tabela$VAR02 == diferentes[91])])

rede <- delete.vertices(rede, vertices_nao_validos)


#Funcao para gerar o data.frame chamado cidades
cidades <- data.frame(vertices = names(V(rede)))

cidades["betweenness"] <- NA
centralidade <- betweenness(rede)

for(i in 1:length(cidades$vertices)) {
  cidades$betweenness[i] <- centralidade[cidades$vertices[i]]  
}

cidades["grau"] <- NA
graus <- degree(rede)

for(i in 1:length(cidades$vertices)) {
  cidades$grau[i] <- graus[cidades$vertices[i]]  
}

cidades["closeness"] <- NA
closeness <- closeness(rede)

for(i in 1:length(cidades$vertices)) {
  cidades$closeness[i] <- closeness[cidades$vertices[i]]  
}

cidades["constraint"] <- NA
constraint <- constraint(rede)

for(i in 1:length(cidades$vertices)) {
  cidades$constraint[i] <- constraint[cidades$vertices[i]]  
}

tabela$VAR01 <- gsub("\\(.*", "", tabela$VAR01)
tabela$VAR01 <- as.factor(tabela$VAR01)

tabela$VAR02 <- gsub("\\(.*", "" , tabela$VAR02)
tabela$VAR02 <- as.factor(tabela$VAR02)

levels_A <- levels(tabela$VAR01)
levels_B <- levels(tabela$VAR02)

#***
#Colocando classificacao numerica, apenas para testes (a classificacao necessita ser segundo 
# o tamanho do municipio na classificacao do ibge): 

levels(tabela$VAR01) <- c(6,7,14,15,17,11,12,8,9,10,16,2,3,4,5,13,1,18)
levels(tabela$VAR02) <- c(6,7,14,15,17,11,12,8,9,10,16,2,3,4,5,13,1,18)

cidades["classificacao"] <- NA
cidades$classificacao <- as.character(cidades$classificacao)

for(i in 1:length(cidades$vertices)){
  
  if(sum(tabela$IBGE_A == cidades$vertices[i], na.rm = TRUE) != 0) {
    cidades$classificacao[i] <- as.character(tabela$VAR01[tabela$IBGE_A == cidades$vertices[i]][1])
  } else {
    cidades$classificacao[i] <- as.character(tabela$VAR01[tabela$IBGE_B == cidades$vertices[i]][1])
  }
  
} #ha uma observacao permanecendo vazia em classificacao

  #Medidas apenas experimentais, pois classificacao nao esta correta

#Este tipos de correlacao abaixo nao serve para este caso
spearmanBetw <- cor.test( ~ classificacao + betweenness, 
                          data=cidades,
                          method = "spearman",
                          continuity = FALSE,
                          conf.level = 0.95)

spearmanBetw

spearmanConstr <- cor.test( ~ classificacao + constraint, 
                            data=cidades,
                            method = "spearman",
                            continuity = FALSE,
                            conf.level = 0.95)

spearmanConstr

spearmanClosen <- cor.test( ~ classificacao + closeness, 
                            data=cidades,
                            method = "spearman",
                            continuity = FALSE,
                            conf.level = 0.95)

spearmanClosen

spearmanGrau <- cor.test( ~ classificacao + grau, 
                          data=cidades,
                          method = "spearman",
                          continuity = FALSE,
                          conf.level = 0.95)

spearmanGrau

#neste modelo jah eh possivel fazer aprendizado

#***
#(duvida)Realizar a divisao por funcoes
#***
#Para aprendizado de maquina:
#Determinar conjuntos iniciais diferentes de cidades
#segundo a classificacao do IBGE, entao deixar o algoritmo
#determinar segundo caracteristicas estruturais, a qual tipo aquele
#municipio pertence, para comparar a classificacao social com a estrutural

#calcular indice de spearman

encontro <- match(cidades$vertices, distanciasMU$'IBGE_A')

cidades$lat <- distanciasMU$VAR09[encontro]
cidades$lon <- distanciasMU$VAR10[encontro]

#***
#Lembrar de registrar a tabela

