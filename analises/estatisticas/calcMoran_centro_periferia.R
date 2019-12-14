


#Para classificar se a adocao foi periferica ou nao

tab_result_init <- NULL

for(i in 1:length(walktrap)) {
  
  if(length(walktrap[[i]]) < 50) next
  
  
  encontro <- (distanciasMU$IBGE_A %in% walktrap[[i]]) | (distanciasMU$IBGE_B %in% walktrap[[i]]) 
  
  
  comunidadeAnalise <- distanciasMU[which(encontro),]
  
  
  tabDist <- comunidadeAnalise
  
  # recebe netdiffuse
  
  #calcula indice de Moran
  edgeList_estradas <- tabDist[,c(1,2)]
  
  edgeList_minutos_para_transito <- tabDist$DisMinutos
  #edgeList_forca_relacao <- ((1/distanciasMU$DisMinutos)*(max(distanciasMU$DisMinutos)))+1;
  #hist(edgeList_forca_relacao)
  #Gera a rede no modelo difnet com a matriz gerada pelo script "grafosDistancias" guardada na variável "distanciasMU"
  adjmat_distancia_minutos <- edgelist_to_adjmat(
    edgelist   = edgeList_estradas, # Should be a two column matrix/data.frame.
    # w          = edgeList_minutos_para_transito, # An optional vector with weights.
    #dat        = matrizToa,
    #idvar      = "Municipios",
    #toavar     =  frame_todos_municipios$toa, #Nao estao dando certo ainda.
    undirected = TRUE,         # In this case, the edgelist is undirected. 
    keep.isolates = TRUE  # Cria Na's para poderem haver mais atributos do que vertices ao criar a rede. 
  )
  
  # grafo completo, com 8153 estradas originais
  graph_distancia_minutos_estradas <- graph_from_adjacency_matrix(adjmat_distancia_minutos, 
                                                                  mode=c('undirected'), 
                                                                  weighted = TRUE) # 'minutos_para_transito')
  
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
  
  # recebe igraph
  
  
  redeTot <- graph_from_edgelist(as.matrix(comunidadeAnalise[,c(1,2)]), directed = FALSE) 
  
  munPresentes <- frame_todos_municipios[frame_todos_municipios$toa == 2009,]
  
  #varia grau 
  graus <- degree(redeTot)
  
  grausPrimeiros <- graus[as.character(munPresentes$IBGE[!is.na(munPresentes$IBGE)])]
  
  print(grausPrimeiros)
  
  classe <- "desconhecido"
  
  if((length(median(grausPrimeiros, na.rm = TRUE))) == 0 || (length(mean(graus, na.rm = TRUE)) == 0)
     || is.null(median(grausPrimeiros, na.rm = TRUE)) || is.null(mean(graus, na.rm = TRUE))
     || is.na(median(grausPrimeiros, na.rm = TRUE)) || is.na(mean(graus, na.rm = TRUE))
     ) next
  
  print(median(grausPrimeiros, na.rm = TRUE))
  print(mean(graus, na.rm = TRUE))
  
  if(mean(grausPrimeiros, na.rm = TRUE) > mean(graus, na.rm = TRUE)) {
    
    classe <- "centro"
    
  } else if(!is.null(grausPrimeiros)) {
    
    classe <- "periferia"
    
  } else {
    
    classe <- "desconhecido"
    
  }
  
  grafoDif <- as_diffnet(adjmat_distancia_minutos, frame_todos_municipios$toa)
  
  result <- summary(grafoDif)
  
  
  for(j in 1:9) {
    
    n <- length(walktrap[[i]])
    
    aux1 <- NULL
    
      aux1 <- data_frame(comunidade = c(i), nComunidade = c(n), 
                         inicio = classe, 
                         moran = c(result$moran_obs[j]),
                         ano = c((2008 + j))
      )
    
    
      tab_result_init <- rbind(tab_result_init, aux1)
    
  }
  
}
