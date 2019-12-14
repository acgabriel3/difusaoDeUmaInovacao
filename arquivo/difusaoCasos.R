#Necessita que o script de epidemiologia rode primeiro, ou que seja criado um arquivo com a tabela gerada pelo mesmo script.
#Procurar dados de casos de dengue confirmados melhores.

#***
#Referente a taxa de ataque de dengue entre os municipios

library(dplyr)

casosDengue <- tab[, -37]
View(casosDengue)

casosDengue <- casosDengue[,c(1, 2, 35, 36, 37, 38, 39, 40)]

casosDengue <- casosDengue[-4954,]

casosDengue$`2007` <- as.numeric(casosDengue$`2007`)
casosDengue$`2008` <- as.numeric(casosDengue$`2008`)
casosDengue$`2009` <- as.numeric(casosDengue$`2009`)
casosDengue$`2010` <- as.numeric(casosDengue$`2010`)
casosDengue$`2011` <- as.numeric(casosDengue$`2011`)
casosDengue$`2012` <- as.numeric(casosDengue$`2012`)

#A taxa de ataque necessita da população. Os dados da população estão disponibilizados em site do SUS e são uma opção.

#Por enquanto será simulado um vetor com os anos em que houve o primeito pico de dengue.

#um pico de dengue será considerado se em um ano houve um número 3 vezes maior de casos do que em um dos dois anos
#anteriores e em um dos dois anos posteriores.

anos <- colnames(casosDengue)

vetorContagio <- array(dim = length(casosDengue$IBGE))

#há também o caso extremo da direita para j = 8.
for(i in 1:length(casosDengue$IBGE)){

vazios <- is.na(casosDengue[i,])
casosBruto <- casosDengue[i,]

for(j in (4:6)) {
 
 
   if(j == 4) {
     
     if(is.na(casosBruto[j])) {
       casosBruto[j] <- -1
     }
     
     casos <- casosBruto
     casos[is.na(casos)] <- 900000
     
     if(
        (as.numeric(casos[j]) > (5 * as.numeric(casos[(j - 1)])))
        
        &
      
        ((as.numeric(casos[j]) > (5 * as.numeric(casos[(j + 1)])))
         || (as.numeric(casos[j]) > (5 * as.numeric(casos[(j + 2)]))))
        ) {
      vetorContagio[i] <- anos[j]
       break;
    } 
  } else {
    
    if(is.na(casosBruto[j])) {
      casosBruto[j] <- -1
    }
    
    casos <- casosBruto
    casos[is.na(casos)] <- 900000
    
      if(
   
     (as.numeric(casos[j]) > (5 * as.numeric(casos[(j - 1)])))
     || (as.numeric(casos[j]) > (5 * as.numeric(casos[(j - 2)])))
   
     &
   
     ((as.numeric(casos[j]) > (5 * as.numeric(casos[(j + 1)])))
      || (as.numeric(casos[j]) > (5 * as.numeric(casos[(j + 2)]))))
       
   ) {
    vetorContagio[i] <- anos[j]
      break;
      }
  }
}
}

vetorIdentificado <- data.frame(casosDengue$IBGE, vetorContagio)
colnames(vetorIdentificado)[1] <- "IBGE"

View(vetorIdentificado)

distanciasFronteiras$IBGE_A <- substring(distanciasFronteiras$IBGE_A, 1, 6)
distanciasFronteiras$IBGE_B <- substring(distanciasFronteiras$IBGE_B, 1, 6)


conjuntoAnalisadoA <- array(dim = length(vetorIdentificado$IBGE))
conjuntoAnalisadoB <- array(dim = length(vetorIdentificado$IBGE)) 

for(i in 1:length(vetorIdentificado$IBGE)) {

conjunto <- which(vetorIdentificado$IBGE[i] == distanciasFronteiras$IBGE_A)

conjunto <- conjunto[1]

conjuntoAnalisadoA[i] <- distanciasFronteiras$NOMEMUN_A[conjunto]

if(!(vetorIdentificado$IBGE[i] %in% distanciasFronteiras$IBGE_A[i])) {
  conjunto <- which(vetorIdentificado$IBGE[i] == distanciasFronteiras$IBGE_B)
  
  conjunto <- conjunto[1]
  
  conjuntoAnalisadoB[i] <- distanciasFronteiras$NOMEMUN_B[conjunto]
}

} 

View(conjuntoAnalisadoA)
View(conjuntoAnalisadoB)

vetorIdentificadoA <- vetorIdentificado 
vetorIdentificadoB <- vetorIdentificado 

vetorIdentificadoA["nomeMun"] <- conjuntoAnalisadoA
vetorIdentificadoB["nomeMun"] <- conjuntoAnalisadoB


vetorIdentificadoA <- vetorIdentificadoA[!is.na(vetorIdentificadoA$nomeMun),]
vetorIdentificadoB <- vetorIdentificadoB[!is.na(vetorIdentificadoB$nomeMun),]

vetorIdentificado <- rbind(vetorIdentificadoA, vetorIdentificadoB)

View(vetorIdentificado)

for(i in 1:length(vetorIdentificado$IBGE)) {
  
  print(sum(vetorIdentificado$IBGE[i] == vetorIdentificado$IBGE))
  
  conjunto <- which(vetorIdentificado$IBGE[i] == vetorIdentificado$IBGE)
  
  if(length(conjunto) > 1) {
  
    conjunto <- conjunto[2]
  
    vetorIdentificado <- vetorIdentificado[-conjunto,]
    
  }
  
}

#Preciso unir o nome dos municípios com seu respectivo UF, pois existem municípios que possuem o mesmo nome
#porem estou trabalhando com o ibge e o ibge é distinto para os municípios



