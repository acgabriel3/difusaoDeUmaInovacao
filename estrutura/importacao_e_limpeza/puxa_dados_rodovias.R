#CAMINHO
#estrutura/importacao_e_limpeza/puxa_dados_rodovias.R  

puxa_dados_rodovias <- function() {
  
  #Leitura do data frame contendo as informações de rodovias e hidrovias:
  distanciasMU <- read_excel("dados/Base_de_dados_ligacoes_rodoviarias_e_hidroviarias_2016.xlsx", sheet = "Base de dados")
  
  colnames(distanciasMU)[13] <- "DisMinutos" #Refere-se a coluna que informa a distancia em minutos
  colnames(distanciasMU)[4] <- "IBGE_A" #ego
  colnames(distanciasMU)[8] <- "IBGE_B" #alter-ego
  
  return(distanciasMU)
  
}