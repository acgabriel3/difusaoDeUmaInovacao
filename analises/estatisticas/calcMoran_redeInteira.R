
comunidadeAnalise <- distanciasMU


#varia distancia
comunidadeAnalise_ma120 <- comunidadeAnalise[which(comunidadeAnalise$DisMinutos < 80),]


# recebe igraph
redema120 <- graph_from_edgelist(as.matrix(comunidadeAnalise_ma120[,c(1,2)]), directed = FALSE) 

#varia grau 

degrema120 <- degree(redema120)

degrema3ma120 <- names(degrema120[degrema120 > 3])



# recebe netdiffuse

#calcula indice de Moran
calcMoran(comunidadeAnalise[which((comunidadeAnalise$IBGE_A %in% degrema3ma120) | (comunidadeAnalise$IBGE_B %in% degrema3ma120)),])

