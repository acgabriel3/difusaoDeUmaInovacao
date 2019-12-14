#***
#CAMINHO
#estrutura/estrutura_de_dados/geraMedidas.R

#-Recebe uma rede conectada
#-calcula medidas de analise de redes: betweeness, grau, closeness e constraint
#-retorna tabela com tais medidas para cada vertice da rede
geraMedidas <- function(rede) {
  
  cidades <- data.frame(vertices = names(V(rede)))
  
  cidades["betweenness"] <- NA
  centralidade <- betweenness(rede)
  
  #traduz hash para data.frame, possuindo uma observacao por vertice
  for(i in 1:length(cidades$vertices)) {
    cidades$betweenness[i] <- centralidade[cidades$vertices[i]] #complexidade n  
  } #complexidade n quadrado
  
  cidades["grau"] <- NA
  graus <- degree(rede)
  
  for(i in 1:length(cidades$vertices)) {
    cidades$grau[i] <- graus[cidades$vertices[i]] #complexidade n
  } #complexidade n quadrado
  
  cidades["closeness"] <- NA
  closeness <- closeness(rede)
  
  for(i in 1:length(cidades$vertices)) {
    cidades$closeness[i] <- closeness[cidades$vertices[i]] #complexidade n 
  }#complexidade n quadrado
  
  cidades["constraint"] <- NA
  constraint <- constraint(rede)
  
  for(i in 1:length(cidades$vertices)) {
    cidades$constraint[i] <- constraint[cidades$vertices[i]]  #complexidade n
  } #complexidade n quadrado
  
  return(cidades)
  
  #***
  #Precisa-se pesquisar as complexidades das funcoes de medida aplicadas
} #complexidade n quadrado

