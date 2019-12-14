load('networks_list_global.Rdata')

#Simulacao generalizada das cursvas de adocao, indice de mooran e outros

qtd_simulacoes_validas <- length(networks_list_global$limiteinferior_)
qtd_anos_simulacao<- length(networks_list_global$moran_obs_[1,])
qtd_cidades<-length(networks_list_global$infection_[1,])
names_cidades<-attr(networks_list_global$exposure_,'dimnames')[[1]]
names_anos_simulacao<-attr(networks_list_global$exposure_,'dimnames')[[2]]
names_simulacoes <- 1:qtd_simulacoes_validas

# busca os intervalos inferiores únicos, sendo que uma curva será gerada parea cada intervalo inferior
curvas <- unique(networks_list_global$limiteinferior_)
print(curvas)
# busca cada uma das faixas intervalos que contém o mesmo limite inferior 
intervalos <- findInterval(curvas, networks_list_global$limiteinferior_)
print(intervalos)

# vai imprimir as várias curvas, com a quantidade de arestas para o ano
idx_anterior_ini_lim_inf<-1 #1276
for (i in 1:length(curvas)) {
  #i<-97
  lim_inf <- curvas[i]
  idx_atual_fim_lim_inf <- intervalos[i]
  nomeCurva<-paste('(',lim_inf,')')
  qtd_intervalos_na_linha <- idx_atual_fim_lim_inf-idx_anterior_ini_lim_inf+1
  print(paste('Curva com nome ',nomeCurva,
              ' vai de ',idx_anterior_ini_lim_inf,' a ',idx_atual_fim_lim_inf,
              ' com ',(qtd_intervalos_na_linha),' elementos.'))
  if (i == 1) { # gera uma plotagem inicial
    plot(NULL,
         xlim=c(min(networks_list_global$limitesuperior_-lim_inf),max(networks_list_global$limitesuperior_-lim_inf)),
         ylim=c(min(networks_list_global$qtd_arestas_),max(networks_list_global$qtd_arestas_)),
         main='Arestas por intervalo de tempo de deslocamento',
         ylab='Qtd de arestas',
         xlab='Amplitude do intervalo')
  }
  x<-networks_list_global$limitesuperior_[idx_atual_fim_lim_inf]
  y<-networks_list_global$qtd_arestas_[idx_atual_fim_lim_inf]
  if (qtd_intervalos_na_linha>= 10) text(x,y,nomeCurva)
  print(paste('Plotando curva terminando em ',x,',',y))
  lines(networks_list_global$limitesuperior_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf],
        networks_list_global$qtd_arestas_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf])
  idx_anterior_ini_lim_inf<-idx_atual_fim_lim_inf+1
}


# vai imprimir as várias curvas, com o índice de moran para os vários anos (2009 a 2017)
for (j in 1:length(names_anos_simulacao)) {
#  j <- 2
  ano <- names_anos_simulacao[j]
  idx_anterior_ini_lim_inf<-1 #1276
  for (i in 1:length(curvas)) {
    lim_inf <- curvas[i]
    idx_atual_fim_lim_inf <- intervalos[i]
    nomeCurva<-paste('(',lim_inf,')')
    qtd_intervalos_na_linha <- idx_atual_fim_lim_inf-idx_anterior_ini_lim_inf+1
    print(paste('Curva com nome ',nomeCurva,
                ' vai de ',idx_anterior_ini_lim_inf,' a ',idx_atual_fim_lim_inf,
                ' com ',(idx_atual_fim_lim_inf-idx_anterior_ini_lim_inf+1),' elementos.'))
    if (i == 1) { # gera uma plotagem inicial
      plot(NULL,
           xlim=c(
             min(networks_list_global$limitesuperior_-
             networks_list_global$limiteinferior_),
             max(networks_list_global$limitesuperior_-networks_list_global$limiteinferior_)),
           ylim=c(min(networks_list_global$moran_obs_[,j]),max(networks_list_global$moran_obs_[,j])),
           main='Moran I / adoção LIRAa / Intervalo de deslocamento',
           ylab=paste('Moran I (',ano,')'),
           xlab='Amplitude do intervalo')
    }
    validos<-(networks_list_global$moran_pval_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf,j])<1e-3
    amplitudes<-networks_list_global$limitesuperior_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf]-
      networks_list_global$limiteinferior_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf]
    amplitudes_validas<-amplitudes[validos]
    x<-max(amplitudes_validas)
    y<-networks_list_global$moran_obs_[idx_atual_fim_lim_inf,j]
    print(paste('Plotando curva terminando em ',x,',',y))
    valores_x<-networks_list_global$limitesuperior_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf]-
      networks_list_global$limiteinferior_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf]
    valores_y<-networks_list_global$moran_obs_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf,j]
    cor_=sample(rainbow(10))
    lines(valores_x[validos],
          valores_y[validos],col=cor_)
    if (length(valores_x[validos]) && qtd_intervalos_na_linha>= 3) 
      text(x,y,nomeCurva,col=cor_)
    idx_anterior_ini_lim_inf<-idx_atual_fim_lim_inf+1
  }
}

