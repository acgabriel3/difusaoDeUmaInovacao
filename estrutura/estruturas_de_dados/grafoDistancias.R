library(readxl)
library(dplyr)
library(igraph)
library(stringi)

#***
#o que esta sendo utilizado aqui eh um vetor de fronteiras, para capturar apenas as ligacoes entre municipios
#fronteiricos. Mas isso parece nao ser mais o objetivo atual do projeto

#Criar função para tratar de qualquer dado.
#Pensar em um modelo melhor para a captura de todas as distâncias por meio das fronteiras.

#Domínio em que os dados foram encontrados:
sitioDadosIBGE <- "https://ww2.ibge.gov.br/home/geociencias/geografia/redes_fluxos/ligacoes_rodoviarias_hidroviarias_2016/base.shtm"

#Dicionário de variáveis do data frame a ser lido a seguir:
variaveisMu <- read_excel("dados/Base_de_dados_ligacoes_rodoviarias_e_hidroviarias_2016.xlsx", sheet = "Dicionário de variáveis")


#Leitura do data frame contendo as informações de rodovias e hidrovias:
distanciasMU <- read_excel("dados/Base_de_dados_ligacoes_rodoviarias_e_hidroviarias_2016.xlsx", sheet = "Base de dados")

#Leitura do dado referente aos municípios que fazem fronteira entre si(dois a dois)
fronteiras <- read.csv2("dados/fronteiras.csv")
colnames(fronteiras) <- c('IBGE_A', 'IBGE_B') 

colnames(distanciasMU)[13] <- "DisMinutos" #Refere-se a coluna que informa a distancia em minutos
colnames(distanciasMU)[4] <- "IBGE_A" #ego
colnames(distanciasMU)[8] <- "IBGE_B" #alter-ego

distanciasMU <- distanciasMU[,c(2,3,4,5,6,7,8,9,13)] #Seleciona somente as colunas necessárias, referentes aos municípios, seu dados e a respectiva distância

#Parte referente a biblioteca igraph, a respeito de como gerar um objeto rede(igraph) nesta biblioteca:

distanciasMU$IBGE_A <- substring(distanciasMU$IBGE_A, 1, 6)
distanciasMU$IBGE_B <- substring(distanciasMU$IBGE_B, 1, 6)


distanciasMU$IBGE_A <- as.factor(distanciasMU$IBGE_A)
distanciasMU$IBGE_B <- as.factor(distanciasMU$IBGE_B)

edges <- as.matrix(distanciasMU[,c(3,7)])
pesos <- distanciasMU[,9]

#***
#ha um componente com 4980 vertices conectados, e outros 33 com no maximo 20 vertices conectados (variando de 2 a 20)
#que representam os restantes 160 vertices na rede (um valor pequeno se comparado ao total)
redeMun <- graph_from_edgelist(edges, directed = FALSE) 
redeMunPesos <- set_edge_attr(redeMun, "distanciaMin", E(redeMun), value = pesos)

# varLayout <- layout_nicely(redeMun)
# 
# plot(redeMun, vertex.size = 3, vertex.label = "", layout = layout_nicely(redeMun))

