#CAMINHO
#NS/biblioteca/renderizaLinhas.R

#***
#Essa parte abaixo deu certo. Pode-se adicionar cores e filtros ---> No shiny podemos ter um 
#selective input

renderizaLinhas <- function(mapa, tabela) {

  for(i in 1:nrow(tabela)) {
    
    print(i)
    mapa <- mapa %>% addPolylines(lng = c(tabela$VAR08[i], tabela$VAR10[i]),
                                  lat = c(tabela$VAR09[i], tabela$VAR11[i]),
                                  weight = 0.1
                                    )
  }
  
  return(mapa)
  
}

