
walktrap <- walktrap.community(redeMu)

count <- 0

number <- 0

for(i in 1:181) {
  
  n <- length(walktrap[[i]])
  
  count <- n + count
  
  
  if(n > 40) {
    
    number <- number + 1
    
    #print(i)
    
    #print(n)
        
    #encontro <- match(walktrap[[i]], distanciasMU$IBGE_A)
    
    #print(unique(distanciasMU$UF_A[encontro]))
  
  } 

  print(number)  
} #Este parece mais eficiente

comunity1 <- igraph::delete.vertices(redeMun, setdiff(names(V(redeMun)), walktrap[[1]])) #Exemplo de selecao de comunidade
for(i in 1:length(walktrap)) print(mean(degree(delete.vertices(redeMu, setdiff(names(V(redeMu)), walktrap[[i]])))))

print(count)

for(i in 1:35) {
  
  if(length(fatGreedy[[i]]) > 50) {
    
    encontro <- match(fatGreedy[[i]], distanciasMU$IBGE_A)
    
    print(unique(distanciasMU$UF_A[encontro]))
    
  } 
  
}
