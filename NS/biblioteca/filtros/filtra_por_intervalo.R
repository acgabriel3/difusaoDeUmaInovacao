#CAMINHO
#NS/biblioteca/filtros/filtra_por_intervalo.R 

filtra_por_intervalo <- function(tabela, min, max, colunaFiltro) {
  
  resultado <- tabela[min <= tabela[[colunaFiltro]] & max >= tabela[[colunaFiltro]],]
  
  return(resultado)
  
}