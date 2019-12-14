#formação de vetores dos anos, referentes ao LIRAa. 
#Anos: de 2009 a 2017.
#Buscar na mão os dados de IBGE NA dos 11 municípios restantes.
#pensar em complementar os dados com os dados de coleta. 

library(dplyr)
library(stringi)

conjuntoAnalisadoA <- array(dim = length(distanciasFronteiras$IBGE_A))
conjuntoAnalisadoB <- array(dim = length(distanciasFronteiras$IBGE_B))


for(i in 1:length(distanciasFronteiras$IBGE_A)) {
  
  conjunto <- which(distanciasFronteiras$IBGE_A[i] == distanciasFronteiras$IBGE_A)
  
  conjunto <- conjunto[1]
  
  conjuntoAnalisadoA[i] <- distanciasFronteiras$IBGE_A[conjunto]
  
  if(!(distanciasFronteiras$IBGE_A[i] %in% distanciasFronteiras$IBGE_B)) {
    
   # print("passei por aqui")
    conjunto <- which(distanciasFronteiras$IBGE_B[i] == distanciasFronteiras$IBGE_B)
    
    conjunto <- conjunto[1]
    
    conjuntoAnalisadoB[i] <- distanciasFronteiras$IBGE_B[conjunto]
  }
  
}


conjuntoAnalisadoA <- conjuntoAnalisadoA[!is.na(conjuntoAnalisadoA)]
conjuntoAnalisadoB <- conjuntoAnalisadoB[!is.na(conjuntoAnalisadoB)]

colnames(conjuntoAnalisadoA) <- 'IBGE'
colnames(conjuntoAnalisadoB) <- 'IBGE'


conjuntoTotal <- rbind(conjuntoAnalisadoA, conjuntoAnalisadoB)


IBGE <- conjuntoTotal

for(i in 1:length(IBGE)) {
  
  #print(sum(IBGE[i] == IBGE))
  
  conjunto <- which(IBGE[i] == IBGE)
  
  if(length(conjunto) > 1) {
    
    for(i in 2:length(conjunto)){
      aux <- conjunto[i]
      
      IBGE <- IBGE[(-aux)]
    }    
    
  }
  
}

IBGE <- substring(IBGE, 1, 6)


dadosDifusao <- tabConsolidada

dadosDifusao <- dadosDifusao[!is.na(dadosDifusao$IBGE),]
dadosDifusao <- dadosDifusao[!is.na(dadosDifusao$IIP),]

vetorToa <- array(dim = length(IBGE))


for(i in 1:length(vetorToa)) {
  
  conjunto <- which(IBGE[i] == dadosDifusao$IBGE)
  
  vetorToa[i] <- dadosDifusao$ANO[conjunto[1]]
  
}

matrizToa <- data.frame(IBGE, vetorToa)

colnames(matrizToa) <- c('IBGE', 'Ano_Adocao')

#Forma mais eficiente de achar os dados:
teste <- array(dim = length(idVertices$IBGE))

for(i in 1:length(teste)) {
  
  conjunto <- which(idVertices$IBGE[i] == dadosDifusao$IBGE)
  
  idVertices$Toa[i] <- dadosDifusao$ANO[conjunto[1]]
  
}

