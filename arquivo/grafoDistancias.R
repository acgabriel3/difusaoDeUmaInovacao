library(readxl)
library(dplyr)
library(igraph)
library(stringi)


#Criar função para tratar de qualquer dado.
#Pensar em um modelo melhor para a captura de todas as distâncias por meio das fronteiras.

#Domínio em que os dados foram encontrados:
sitioDadosIBGE <- "https://ww2.ibge.gov.br/home/geociencias/geografia/redes_fluxos/ligacoes_rodoviarias_hidroviarias_2016/base.shtm"

#Dicionário de variáveis do data frame a ser lido a seguir:
variaveisMu <- read_excel("dados/Base_de_dados_ligacoes_rodoviarias_e_hidroviarias_2016.xlsx", sheet = "Dicionário de variáveis")


#Leitura do data frame contendo as informações de rodovias e hidrovias:
distanciasMU <- read_excel("dados/Base_de_dados_ligacoes_rodoviarias_e_hidroviarias_2016.xlsx", sheet = "Base de dados")

#Leitura do dado referente aos municípios que fazem fronteira entre si(dois a dois)
fronteiras <- read.csv2("fronteiras.csv")
colnames(fronteiras) <- c('IBGE_A', 'IBGE_B') 

colnames(distanciasMU)[13] <- "DisMinutos" #Refere-se a coluna que informa a distancia em minutos
colnames(distanciasMU)[4] <- "IBGE_A" #ego
colnames(distanciasMU)[8] <- "IBGE_B" #alter-ego

distanciasMU <- distanciasMU[,c(2,3,4,5,6,7,8,9,13)] #Seleciona somente as colunas necessárias, referentes aos municípios, seu dados e a respectiva distância

# Acerca da quantidade de municípios cuja ainda não temos informações:
sum(is.na(match(mapaTeste@data$CD_GEOCMU, distanciasMU$IBGE_A) & is.na(match(mapaTeste@data$CD_GEOCMU, distanciasMU$IBGE_B)))) #conta a quantidade de municípios não encontrados nos dados do IBGE
sum(is.na(match(mapaTeste@data$CD_GEOCMU, fronteiras$IBGE_A) & is.na(match(mapaTeste@data$CD_GEOCMU, fronteiras$IBGE_B)))) #conta a quantidade de municípios não encontrados nos dados de municípios que fazem fronteiras

sum(is.na(match(fronteiras$IBGE_A, distanciasMU$IBGE_A)) &
  is.na(match(fronteiras$IBGE_A, distanciasMU$IBGE_B)) &
  is.na(match(fronteiras$IBGE_B, distanciasMU$IBGE_A)) &
  is.na(match(fronteiras$IBGE_B, distanciasMU$IBGE_B)))  #Conta a quantidade de municípios que não possuem informações sobre a distância

#Teste de método mais eficaz para encontrar distância entre municípios que fazem fronteira:

#encontro_1 <- match(fronteiras$IBGE_A, distanciasMU$IBGE_A)
#encontro_2 <- match(fronteiras$IBGE_A, distanciasMU$IBGE_B)
#encontro_3 <- match(fronteiras$IBGE_B, distanciasMU$IBGE_A)
#encontro_4 <- match(fronteiras$IBGE_B, distanciasMU$IBGE_B)

#encontro <- encontro_1
#encontro[((!is.na(encontro_1)) & (is.na(encontro_2)) & (is.na(encontro_3)) & (is.na(encontro_4)))] <- encontro_1[((!is.na(encontro_1)) & (is.na(encontro_2)) & (is.na(encontro_3)) & (is.na(encontro_4)))] 
#encontro[((is.na(encontro_1)) & (!is.na(encontro_2)) & (is.na(encontro_3)) & (is.na(encontro_4)))] <- encontro_2[((is.na(encontro_1)) & (!is.na(encontro_2)) & (is.na(encontro_3)) & (is.na(encontro_4)))]   
#encontro[((is.na(encontro_1)) & (is.na(encontro_2)) & (!is.na(encontro_3)) & (is.na(encontro_4)))] <- encontro_3[((is.na(encontro_1)) & (is.na(encontro_2)) & (!is.na(encontro_3)) & (is.na(encontro_4)))] 
#encontro[((is.na(encontro_1)) & (is.na(encontro_2)) & (is.na(encontro_3)) & (!is.na(encontro_4)))] <- encontro_4[((is.na(encontro_1)) & (is.na(encontro_2)) & (is.na(encontro_3)) & (!is.na(encontro_4)))] 

#junta as duas colunas com alter e ego para fazer o encontro do edge(que completa sua informação
#com as duas colunas)
fronteirasMatch <- paste(fronteiras$IBGE_A, fronteiras$IBGE_B, sep = "") #Define formato nos dados de fronteira
#fronteirasMatch2 <- paste(fronteiras$IBGE_B, fronteiras$IBGE_A, sep = "") 

distanciasMatch <- paste(distanciasMU$IBGE_A, distanciasMU$IBGE_B, sep = "")  #procurar os municipios nos dados de rodovias na ordem 1-2
distanciasMatch2 <- paste(distanciasMU$IBGE_B, distanciasMU$IBGE_A, sep = "")  #procurar os municipios nos dados de rodovias na ordem 2-1 


  
encontro <- match(fronteirasMatch, distanciasMatch) #procura ordem 1-2
  
encontro2 <- match(fronteirasMatch, distanciasMatch2) #procura ordem 2-1

#Seleciona apenas as informações dos municípios que fazem fronteira.
distanciasFronteiras <- distanciasMU[encontro,]
distanciasFronteiras <- distanciasFronteiras[!is.na(distanciasFronteiras$COD_UF_A),]
  
distanciasFronteiras2 <- distanciasMU[encontro2,]
distanciasFronteiras2 <- distanciasFronteiras2[!is.na(distanciasFronteiras2$COD_UF_A),]

#Consolida os dados de ambas as ligações
distanciasFronteiras <- rbind(distanciasFronteiras, distanciasFronteiras2) 

#Ainda é possível melhorar a busca pelos edges necessários.


#Exemplo de determinação das fronteiras da rede por região:

#Nordeste <-   distanciasFronteiras$UF_A == 'PI' | 
#            distanciasFronteiras$UF_A == 'PI' |
#            distanciasFronteiras$UF_A == 'PE' |
#            distanciasFronteiras$UF_A == 'CE' |
#            distanciasFronteiras$UF_A == 'RN' |
#            distanciasFronteiras$UF_A == 'PB' |
#            distanciasFronteiras$UF_A == 'AL' |
#            distanciasFronteiras$UF_A == 'SE' |
#            distanciasFronteiras$UF_A == 'BA'

#distanciasFronteiras <- distanciasFronteiras[Nordeste,]


distanciasFronteiras$IBGE_A <- as.character(distanciasFronteiras$IBGE_A)
distanciasFronteiras$IBGE_B <- as.character(distanciasFronteiras$IBGE_B)

#coloca o IBGE no formato de 6 dígitos(a maioria dos dados estão neste formato)
distanciasFronteiras$IBGE_A <- substring(distanciasFronteiras$IBGE_A, 1 , 6)
distanciasFronteiras$IBGE_B <- substring(distanciasFronteiras$IBGE_B, 1 , 6)


distanciasMU <- distanciasFronteiras

#Cria rede referente aos municípios que fazem fronteira
distanciasMU <- distanciasMU[,c(3, 7, 9)]


#Parte referente a biblioteca igraph, a respeito de como gerar um objeto rede(igraph) nesta biblioteca:

#edges <- as.matrix(distanciasMU[,c(1,2)])
#pesos <- distanciasMU[,3]

#distanciasMU <- graph_from_edgelist(edges, directed = FALSE)
#distanciasMU <- set_edge_attr(distanciasMU, "distanciaMin", value = pesos)

#varLayout <- layout_nicely(distanciasMU)

#plot(distanciasMU, vertex.size = 3, vertex.label = "", layout = varLayout)


#Testa se existem valores repetidos em fronteira (pode ser transformado em uma funcao)
#testa <- paste(fronteiras$IBGE_A, fronteiras$IBGE_B)

#for(i in 1:length(fronteiras$IBGE_A)) {
#  conjunto <- which(testa[i] == testa)
#  print(length(conjunto))
#}