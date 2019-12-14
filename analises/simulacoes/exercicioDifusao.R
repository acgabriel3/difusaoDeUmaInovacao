#Execução de rotinas encontradas no capítulo 8 do payek em script do R Studio.

#***************************************************************************************************************#
#-Necessidades:

#-Até o momento todas as necessidades anteriores foram resolvidas.
#***************************************************************************************************************#

library(gridExtra)
library(dplyr)
library(netdiffuseR)
#Recebe dados consolidados do script "nomes_colunas" referentes ao LIRAa
liraaMunicipios_2009_a_2017 <- tabConsolidada

#retira registros sem código de ibge ou índice de infestação predial - não úteis (NA)
liraaMunicipios_2009_a_2017 <- liraaMunicipios_2009_a_2017[!is.na(liraaMunicipios_2009_a_2017$IBGE),]
liraaMunicipios_2009_a_2017 <- liraaMunicipios_2009_a_2017[!is.na(liraaMunicipios_2009_a_2017$IIP),]


edgeList_estradas <- distanciasMU[,1:2]
edgeList_minutos_para_transito <- distanciasMU$DisMinutos
#edgeList_forca_relacao <- ((1/distanciasMU$DisMinutos)*(max(distanciasMU$DisMinutos)))+1;
#hist(edgeList_forca_relacao)
#Gera a rede no modelo difnet com a matriz gerada pelo script "grafosDistancias" guardada na variável "distanciasMU"
adjmat_distancia_minutos <- edgelist_to_adjmat(
  edgelist   = edgeList_estradas, # Should be a two column matrix/data.frame.
  w          = edgeList_minutos_para_transito, # An optional vector with weights.
  #dat        = matrizToa,
  #idvar      = "Municipios",
  toavar     =  frame_todos_municipios$toa, #Nao estao dando certo ainda.
  undirected = TRUE,         # In this case, the edgelist is undirected. 
  keep.isolates = TRUE  # Cria Na's para poderem haver mais atributos do que vertices ao criar a rede. 
 )


# grafo completo, com 8153 estradas originais
graph_distancia_minutos_estradas <- graph_from_adjacency_matrix(adjmat_distancia_minutos, 
                                                       mode=c('undirected'), 
                                                       weighted = TRUE) # 'minutos_para_transito')

#obtem o time of adoption para todos os municípios
adjmat_grafo_completo <- as_adjacency_matrix(graph_distancia_minutos_estradas)
frame_todos_municipios <- as.data.frame(adjmat_grafo_completo@Dimnames[[1]])
frame_todos_municipios['toa'] <- NA
colnames(frame_todos_municipios)[1] <- 'IBGE'
frame_todos_municipios$IBGE <- as.character(frame_todos_municipios$IBGE)
frame_todos_municipios$IBGE <- as.numeric(frame_todos_municipios$IBGE)
for(j in 1:length(frame_todos_municipios$IBGE)) {
  conjunto <- which(frame_todos_municipios$IBGE[j] == liraaMunicipios_2009_a_2017$IBGE)
  frame_todos_municipios$toa[j] <- liraaMunicipios_2009_a_2017$ANO[conjunto[1]]
}

# computa todo o conjunto das distâncias entre 5140 municípios do brasil de 5570 no total
matriz_distancias_totais <- distances(graph_distancia_minutos_estradas, weights = NULL)
graph_distancia_totais_estradas <- graph_from_adjacency_matrix(matriz_distancias_totais, 
                                                                mode=c('undirected'), 
                                                                weighted = TRUE) # 'minutos_para_transito')
print(paste('Quantidade total de arestas entre todas as cidades: ',length(E(graph_distancia_totais_estradas))))
sink()

qtd_maxima_horas_viagem<-1
limite_maximo <- qtd_maxima_horas_viagem*120 #horas de viagem, no máximo
#computa o grafo das distâncias entre todas as cidades, 
# em duração de viagem com até <qtd_maxima_horas_viagem> horas * 60 minutos
graph_distancia_totais_estradas_corte_limite_maximo <- delete.edges(graph_distancia_totais_estradas,
                                E(graph_distancia_totais_estradas)[
                                  weight>limite_maximo])
print(paste('Quantidade total de arestas após corte para tempo máximo de :', limite_maximo, ' minutos:', length(E(graph_distancia_totais_estradas_corte_limite_maximo))))

nomes_cidades<-V(graph_distancia_totais_estradas_corte_limite_maximo)

#gera uma série de de intervalos de deslocamento distintos.
# reinicie as simulações por aqui
intervalos_tempo <- list()
i <- 1
intervalo_minimo <- 5 # menor intervalo de minutos analisado
intervalo_maximo <- qtd_maxima_horas_viagem/12*60 # maior intervalo de tempo para computar correlação
decrescimo <- -1 # aumente esse número para fazer uma varredura mais rápida, até o limite do intervalo máximo
for (inicio in seq(from=limite_maximo-intervalo_maximo+1, to=1, by=decrescimo)) {
  for (intervalo in seq(from = intervalo_maximo-1, to = intervalo_minimo, by =decrescimo)) {
    intervalos_tempo[[i]] <- c(inicio,inicio+intervalo)
    if (intervalo<=0) print(paste('erro no intervalo',i))
    i<-i+1
  }
}
print(paste('Qtd intervalos: ',i))
sink()
# reinicie as simulações por aqui
# armazena atributos calculados para as redes simuladas
qtd_networks <- length(intervalos_tempo)
network_limiteinferior <- list()
network_limitesuperior <- list()
network_qtd_arestas <- list()
network_moran_obs <- list()
network_moran_pval <- list()
network_hazard_rate <- list()
network_adopters <- list()
network_toa  <- list()
network_thr <- list()
network_infection <- list()
network_exposure <- list()
network_fitbass_p <- list()
network_fitbass_q <- list()

sink()
output_file_preffix <- format(Sys.time(),"%Y%b%d%H%M")
output_file_preffix<-paste(output_file_preffix,'_Simulacoes',sep='')
print(output_file_preffix)
output_file_pdf <- paste(output_file_preffix,'.pdf',sep='')
output_file_txt <- paste(output_file_preffix,'.txt',sep='')
output_file_image_networks <- paste(output_file_preffix,'.Rdata',sep='')
pdf(output_file_pdf)
ultimo_limiteinferior <- 0
ultimo_limitesuperior <- limite_maximo
ultimo_grafo <- graph_distancia_totais_estradas_corte_limite_maximo
graph_distancia_totais_estradas <- graph_distancia_totais_estradas_corte_limite_maximo 

#sink()
ultimo_intervalo_calculado <- 1#210 # comeca com um
k <- 0
for (i in ultimo_intervalo_calculado:qtd_networks) {
  k <- k+1
  sink()
  intervalo <- intervalos_tempo[[i]]
  limiteinferior <- intervalo[1]
  limitesuperior <- intervalo[2]
  network_limiteinferior[[k]]<-limiteinferior
  network_limitesuperior[[k]]<-limitesuperior
  if (limiteinferior >= ultimo_limiteinferior & limitesuperior <= ultimo_limitesuperior) {
    # é possível aproveitar o ultimo grafo usado, sem precisar do grafo baseline
    graph_distancia_totais_estradas <- ultimo_grafo
    print(paste('Reusando ultimo grafo, pois limites atuais [',limiteinferior,',',limitesuperior,'] estão contidos em [',
                ultimo_limiteinferior,',',ultimo_limitesuperior,']'))
  } else {
    # os limites do intervalo foram deslocados. Logo, deve-se utilizar o grafo total
    graph_distancia_totais_estradas <- graph_distancia_totais_estradas_corte_limite_maximo
    print(paste('Descartando ultimo grafo, pois limites atuais [',limiteinferior,',',limitesuperior,'] não estão contidos em [',
                ultimo_limiteinferior,',',ultimo_limitesuperior,']'))
  }
  ultimo_limiteinferior <- limiteinferior
  ultimo_limitesuperior <- limitesuperior
  print(paste('Simulacao #',k,' de ',qtd_networks,' intervalo:',intervalo[1],' a ',intervalo[2]))
  sink(output_file_txt,c = TRUE)
  print(paste('Simulacao #',k,' de ',qtd_networks,' intervalo:',intervalo[1],' a ',intervalo[2]))
  graph_intervalo <- delete.edges(graph_distancia_totais_estradas,
                                  E(graph_distancia_totais_estradas)[
                                    weight<limiteinferior|
                                      weight>limitesuperior])
  ultimo_grafo <- graph_intervalo
  qtd_arestas <- length(E(graph_intervalo))
  network_qtd_arestas[[k]] <- qtd_arestas
  print(paste('Qtd arestas:',qtd_arestas))
  adjmat_grafo_intervalo<- as_adjacency_matrix(graph_intervalo)
  
  redeDiffnet_intervalo <- new_diffnet(adjmat_grafo_intervalo, frame_todos_municipios$toa)
  s <- summary(redeDiffnet_intervalo, valued = TRUE, no.print = TRUE)
  network_moran_obs[[k]] <- s$moran_obs
  network_moran_pval[[k]] <- s$moran_pval
  network_hazard_rate[[k]] <- s$hazard
  network_adopters[[k]] <- s$adopt
  print(s)
  redeDiffnet_intervalo_adopters <- classify_adopters(redeDiffnet_intervalo)
#  plot(redeDiffnet_intervalo_adopters$toa,main=paste('Adopters$toa from:',limiteinferior,' to:',limitesuperior))
  network_toa[[k]]  <- redeDiffnet_intervalo_adopters$toa
#  plot(redeDiffnet_intervalo_adopters$thr,main=paste('Adopters$thr from:',limiteinferior,' to:',limitesuperior))
  network_thr[[k]]  <- redeDiffnet_intervalo_adopters$thr
  infection_ <- infection(redeDiffnet_intervalo)
#  plot(infection,main=paste('Infection from:',limiteinferior,' to:',limitesuperior))
  network_infection[[k]]  <- infection_
  exposure <- exposure(redeDiffnet_intervalo)
  hist(exposure,main=paste('Exposure from:',limiteinferior,' to:',limitesuperior))
  network_exposure[[k]] <- exposure
  fitbass <- fitbass(redeDiffnet_intervalo)
#  print(summary(fitbass))
  network_fitbass_p[[k]] <- coef(fitbass)[['p']]
  network_fitbass_q[[k]] <- coef(fitbass)[['q']]
  sink()
}
dev.off()
networks_list <- list(
  limiteinferior_=network_limiteinferior, 
  limitesuperior_=network_limitesuperior, 
  qtd_arestas_=network_qtd_arestas, 
  moran_obs_=network_moran_obs, 
  moran_pval_=network_moran_pval,
  hazard_rate_=network_hazard_rate,
  adopters_=network_adopters,
  toa_=network_toa,
  thr_=network_thr,
  infection_=network_infection,
  exposure_=network_exposure,
  fitbass_p=network_fitbass_p, 
  fitbass_q=network_fitbass_q 
)  

save(networks_list,nomes_cidades,file=output_file_image_networks)
#save.image()
#plot(redeDiffnet_distancia_minutos)
#plot(redeDiffnet_fronteiras)

