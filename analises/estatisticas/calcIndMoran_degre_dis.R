
#Para cada ano eu preciso dos resultados para cada uma das 4 possibilidadades, em cada uma das comunidades com mais de 50 vértices
walktrap  <-  walktrap.community(redeMun)

tab_result_moran <- NULL
  
  #data_frame(comunidade = NA, nComunidade = NA, distancia = NA, conectividade = NA, moran = NA, ano = NA)

for(i in 1:length(walktrap)) {
  
  if(length(walktrap[[i]]) < 50) next
  
  
  encontro <- (distanciasMU$IBGE_A %in% walktrap[[i]]) | (distanciasMU$IBGE_B %in% walktrap[[i]]) 
  
  
  comunidadeAnalise <- distanciasMU[which(encontro),]
  
  
  #varia distancia
  comunidadeAnalise_ma120 <- comunidadeAnalise[which(comunidadeAnalise$DisMinutos > 80),]
  comunidadeAnalise_me120 <- comunidadeAnalise[which(comunidadeAnalise$DisMinutos < 81),]
  
  
  # recebe igraph
  redema120 <- graph_from_edgelist(as.matrix(comunidadeAnalise_ma120[,c(1,2)]), directed = FALSE) 
  redeme120 <- graph_from_edgelist(as.matrix(comunidadeAnalise_me120[,c(1,2)]), directed = FALSE)
  
  #varia grau 
  
  degrema120 <- degree(redema120)
  degreme120 <- degree(redeme120)
  
  degreme3ma120 <- names(degrema120[degrema120 < 4])
  degrema3ma120 <- names(degrema120[degrema120 > 3])
  degreme3me120 <- names(degreme120[degreme120 < 4])
  degrema3me120 <- names(degreme120[degreme120 > 3])
  
  
  # recebe netdiffuse
  
  #calcula indice de Moran
  if(length(degreme3ma120) > 0)
    mema <- calcMoran(comunidadeAnalise[which((comunidadeAnalise$IBGE_A %in% degreme3ma120) | (comunidadeAnalise$IBGE_B %in% degreme3ma120)),])
  
  if(length(degrema3ma120) > 0)
    mama <- calcMoran(comunidadeAnalise[which((comunidadeAnalise$IBGE_A %in% degrema3ma120) | (comunidadeAnalise$IBGE_B %in% degrema3ma120)),])
  
  
  if(length(degreme3me120) > 0)
    meme <- calcMoran(comunidadeAnalise[which((comunidadeAnalise$IBGE_A %in% degreme3me120) | (comunidadeAnalise$IBGE_B %in% degreme3me120)),])
  
  if(length(degrema3me120) > 0)
    mame <- calcMoran(comunidadeAnalise[which((comunidadeAnalise$IBGE_A %in% degrema3me120) | (comunidadeAnalise$IBGE_B %in% degrema3me120)),])
  
  print('a')
  
  for(j in 1:9) {
    
    n <- length(walktrap[[i]])
    
    aux1 <- NULL
    aux2 <- NULL
    aux3 <- NULL
    aux4 <- NULL
    
    if(length(degrema3ma120) > 0)
    aux1 <- data_frame(comunidade = c(i), nComunidade = c(n), nsubset = length(degrema3ma120),
                      distancia = c(">80") , 
                      conectividade = c(">3"),
                      moran = c(mama$moran_obs[j]),
                      ano = c((2008 + j))
                      )
    
    if(length(degreme3ma120) > 0)
    aux2 <- data_frame(comunidade = c(i), nComunidade = c(n), nsubset = length(degrema3me120),
                       distancia = c(">80") , 
                       conectividade = c("<3"),
                       moran = c(mame$moran_obs[j]),
                       ano = c((2008 + j))
            )
    
    if(length(degrema3me120) > 0)
    aux3 <- data_frame(comunidade = c(i), nComunidade = c(n), nsubset = length(degreme3ma120),
                       distancia = c("<80") , 
                       conectividade = c(">3"),
                       moran = c(mema$moran_obs[j]),
                       ano = c((2008 + j))
            )
    
    if(length(degreme3me120) > 0)
    aux4 <- data_frame(comunidade = c(i), nComunidade = c(n), nsubset = length(degreme3me120),
                       distancia = c("<80") , 
                       conectividade = c("<3"),
                       moran = c(meme$moran_obs[j]),
                       ano = c((2008 + j))
            )
    
    
    tab_result_moran <- rbind(tab_result_moran, aux1, aux2, aux3, aux4)

  }
  
}
