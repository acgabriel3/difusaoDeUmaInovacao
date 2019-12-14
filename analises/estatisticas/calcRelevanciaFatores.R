

# >80 = 1  e <80 = -1
# >3 = 1 e <3 = -1

#  qa qb qab
#  -1 -1  1  >>
#   1 -1 -1  ><
#  -1  1 -1  <>
#   1  1  1  <<

#q0 = sum(y)
#qa = qa*y
#qb = qb*y
#qab = qab*y


relevanciaFatores <- NULL


for(comunidade in unique(tab_result_moran$comunidade)) {
  
  tabAnalise <- tab_result_moran[tab_result_moran$comunidade == comunidade,]
  
  if((nrow(tabAnalise)/4 - floor(nrow(tabAnalise)/4)) > 0) next 
  
  for(i in seq(-3, nrow(tabAnalise) - 4, by = 4)) {
    
    qa <- tabAnalise$moran[i + 7] * -1 + tabAnalise$moran[i + 6] + tabAnalise$moran[i + 5] * -1 + tabAnalise$moran[i + 4]  
    qb <- tabAnalise$moran[i + 7] * -1 + tabAnalise$moran[i + 6] * -1 + tabAnalise$moran[i + 5]  + tabAnalise$moran[i + 4]  
    qab <- tabAnalise$moran[i + 7] + tabAnalise$moran[i + 6] * -1 + tabAnalise$moran[i + 5] * -1 + tabAnalise$moran[i + 4]  
    
    
    sst <- 4*qa^2 + 4*qb^2 + 4*qab^2
    
    ssa <- 4*qa^2/sst
    ssb <- 4*qb^2/sst
    ssab <- 4*qab^2/sst
    
    aux <- data_frame(comunidade = tabAnalise$comunidade[i + 4], nComunidade = tabAnalise$nComunidade[i + 4], 
                      ano = tabAnalise$ano[i + 4], vGraus = ssa, vDistancia = ssb, VRelacao = ssab)
    
    relevanciaFatores <- rbind(relevanciaFatores, aux)
  
  }

}
