# testando funcionamento de order
test0_list<-list(1, 1, 1, 2, 2, 3, 5, 1, 1, 2)
test1_list<-list(2, 4, 7, 9, 1, 3, 5, 6, 8,10)
test2_list<-list(1, 2, 4, 7, 6, 9,10, 3, 5, 8)
# listas que devem ter ordenação aninhada
print('Primeira lista')
unlist(test0_list)
print('Segunda lista:')
unlist(test1_list)
# deve ficar de 1 a 10, após ordenação aninhada
print('Lista que deve ser reordenada')
unlist(test2_list)

# ordenação aninhada
ordem<-order(unlist(test0_list),unlist(test1_list))
# [1]  1  2  8  3  9  5  4 10  6  7
print(ordem)

test2_list_ordenada<-test2_list[ordem]

# deve estar tudo TRUE
print(test2_list_ordenada==c(1:10))

#ordena e plota calculos

networks_list_global <- list(
  limiteinferior_ = list(), 
  limitesuperior_ = list(), 
  qtd_arestas_ = list(), 
  moran_obs_ = list(), 
  moran_pval_ = list(),
  hazard_rate_ = list(),
  adopters_ = list(),
  toa_ = list(),
  thr_ = list(),
  infection_ = list(),
  exposure_ = list(),
  fitbass_p = list(), 
  fitbass_q = list() 
)  

imagenslist<- c(
  '2018Mai041802_Simulacoes.Rdata',
  '2018Mai042152_Simulacoes.Rdata',
  '2018Mai051307_Simulacoes.Rdata',
  '2018Mai061943_Simulacoes.Rdata',
  '2018Mai080025_Simulacoes.Rdata',
  '2018Mai080033_Simulacoes.Rdata',
  '2018Mai080040_Simulacoes.Rdata',
  '2018Mai080049_Simulacoes.Rdata',
  '2018Mai080821_Simulacoes.Rdata'
)

# consolida os dados
for (i in 1:length(imagenslist)) {
  print(paste('Rodada ',i))
  print(paste('Lendo arquivo de imagem:',imagenslist[i]))
  load(imagenslist[i])
  print(paste('Carregada lista com ',length(networks_list$limiteinferior_),' medições'))
  print('Remove elementos não plenamente computados ao final da lista')
  len<-c(1:13)
  for (j in 1:length(len)) {
    len[j]<- length(networks_list[[j]])
  }
  min<-min(len)
  if (min < max(len)) {
    print(paste('Lista ',i,' apresenta quantidade de parâmetros irregular ',imagenslist[i], ' vetor de tamanhos ',length(len), 'organizando'))
    for (j in 1:length(len)) {
      if (min < length(networks_list[[j]])) {
        print(paste('encurtando lista de ',length(networks_list[[j]]), ' para ', min))
        networks_list[[j]][[length(networks_list[[j]])]] <- NULL
      }
    }
  } else {
    print(paste('Lista ',i,' OK: ',imagenslist[i], ' min ',min))
  }
  
  networks_list_global$limiteinferior_<-append(networks_list_global$limiteinferior_,networks_list$limiteinferior_) 
  networks_list_global$limitesuperior_<-append(networks_list_global$limitesuperior_,networks_list$limitesuperior_)
  networks_list_global$qtd_arestas_<-append(networks_list_global$qtd_arestas_,networks_list$qtd_arestas_) 
  networks_list_global$moran_obs_<-append(networks_list_global$moran_obs_,networks_list$moran_obs_) 
  networks_list_global$moran_pval_<-append(networks_list_global$moran_pval_,networks_list$moran_pval_)
  # hazard_rate_ = [1] 0.00000000 0.04988029 0.04766905 0.22138920
  # [5] 0.09855565 0.09205152 0.04844291 0.23381818
  # [9] 0.89985762
  networks_list_global$hazard_rate_<-append(networks_list_global$hazard_rate_,networks_list$hazard_rate_)
  #networks_list_global$hazard_rate_<-list()
  # networks_list_global$adopters_[[1000]]
  # [1]  128  250  227 1004  348  293  140  643 1896
  networks_list_global$adopters_<-append(networks_list_global$adopters_,networks_list$adopters_)
  #networks_list_global$adopters_<- list()
  #> sum(!networks_list_global$toa_[[1]]==networks_list_global$toa_[[100]])
  #[1] 0
  networks_list_global$toa_<-append(networks_list_global$toa_,networks_list$toa_)
  #networks_list_global$toa_<-list()
  networks_list_global$thr_<-append(networks_list_global$thr_,networks_list$thr_)
  networks_list_global$infection_<-append(networks_list_global$infection_,networks_list$infection_)
  networks_list_global$exposure_<-append(networks_list_global$exposure_,networks_list$exposure_)
  #> networks_list_global$fitbass_p[[1]]
  #[1] 0.02568422
  #> mean(unlist(networks_list_global$fitbass_p))
  #[1] 0.02568422
  networks_list_global$fitbass_p<-append(networks_list_global$fitbass_p,networks_list$fitbass_p)
  #networks_list_global$fitbass_p<-list()
  networks_list_global$fitbass_q<-append(networks_list_global$fitbass_q,networks_list$fitbass_q)
}

qtd_simulacoes_validas <- length(networks_list_global$limiteinferior_)
qtd_anos_simulacao<- length(networks_list_global$moran_obs_[[1]])
qtd_cidades<-length(networks_list_global$infection_[[1]])

nomes_simulacoes_validas <- networks_list_global$limiteinferior_
nomes_anos_simulacao<- c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)

remove(networks_list)
#unlist(networks_list_global$limiteinferior_)

# verifica se todas as listas possuem o mesmo tamanho
len<-c(1:13)
for (j in 1:length(len)) {
  len[j]<- length(networks_list_global[[j]])
}
print (len)

#computa a ordenação aninhada das demais listas.
ordenacao_inf_sup<-order(unlist(networks_list_global$limiteinferior_),unlist(networks_list_global$limitesuperior_))
#plot(unlist(ordenacao_inf_sup))
#plot(unlist(networks_list_global$limiteinferior_))

#names(networks_list_global$limiteinferior_)<-ordenacao_inf_sup
#names(networks_list_global$limitesuperior_)<-ordenacao_inf_sup
#names(networks_list_global$qtd_arestas_)<-ordenacao_inf_sup
#names(networks_list_global$moran_obs_)<-ordenacao_inf_sup
#names(networks_list_global$moran_pval_)<-ordenacao_inf_sup

# hazard_rate_ = [1] 0.00000000 0.04988029 0.04766905 0.22138920
# [5] 0.09855565 0.09205152 0.04844291 0.23381818
# [9] 0.89985762
#networks_list_global$hazard_rate_<-append(networks_list_global$hazard_rate_,networks_list$hazard_rate_)
#names(networks_list_global$hazard_rate_)<-ordenacao_inf_sup

# networks_list_global$adopters_[[1000]]
# [1]  128  250  227 1004  348  293  140  643 1896
#networks_list_global$adopters_<-append(networks_list_global$adopters_,networks_list$adopters_)
#names(networks_list_global$adopters_)<-ordenacao_inf_sup

#> sum(!networks_list_global$toa_[[1]]==networks_list_global$toa_[[100]])
#[1] 0
#networks_list_global$toa_<-append(networks_list_global$toa_,networks_list$toa_)
#names(networks_list_global$toa_)<-ordenacao_inf_sup

#names(networks_list_global$thr_)<-ordenacao_inf_sup
#names(networks_list_global$infection_)<-ordenacao_inf_sup
#names(networks_list_global$exposure_)<-ordenacao_inf_sup
#networks_list_global$exposure_[['1']]==networks_list_global$exposure_[['100']]

#> networks_list_global$fitbass_p[[1]]
#[1] 0.02568422
#> mean(unlist(networks_list_global$fitbass_p))
#[1] 0.02568422
#names(networks_list_global$fitbass_p)<-ordenacao_inf_sup
#names(networks_list_global$fitbass_q)<-ordenacao_inf_sup

#names(networks_list_global$limiteinferior_)

# gera sequencia de reordenamento com base na sequencia [limite_inferior_+limite_superior_]
sequencia_reordenamento<-order(unlist(networks_list_global$limiteinferior_),unlist(networks_list_global$limitesuperior_))
#names(sequencia_reordenamento)<-sequencia_reordenamento
#print(sequencia_reordenamento)

# reordena todos os elementos com base na sequencia [limite_inferior_+limite_superior_]
networks_list_global$limiteinferior_ <- unlist(networks_list_global$limiteinferior_[sequencia_reordenamento]) 
networks_list_global$limitesuperior_ <- unlist(networks_list_global$limitesuperior_[sequencia_reordenamento])
networks_list_global$qtd_arestas_ <- unlist(networks_list_global$qtd_arestas_[sequencia_reordenamento])

networks_list_global$moran_obs_ <- matrix(unlist(networks_list_global$moran_obs_[sequencia_reordenamento]), ncol = 9, byrow = TRUE)
networks_list_global$moran_pval_ <- matrix(unlist(networks_list_global$moran_pval_[sequencia_reordenamento]), ncol = 9, byrow = TRUE)
networks_list_global$hazard_rate_ <- matrix(unlist(networks_list_global$hazard_rate_[sequencia_reordenamento]), ncol = 9, byrow = TRUE)

networks_list_global$adopters_ <- matrix(unlist(networks_list_global$adopters_[sequencia_reordenamento]), ncol = 9, byrow = TRUE)
networks_list_global$toa_ <- matrix(unlist(networks_list_global$toa_[sequencia_reordenamento]), ncol = length(networks_list_global$toa_[[1]]), byrow = TRUE)
networks_list_global$thr_ <- matrix(unlist(networks_list_global$thr_[sequencia_reordenamento]), ncol = length(networks_list_global$thr_[[1]]), byrow = TRUE)


networks_list_global$infection_ <- array(data=unlist(networks_list_global$exposure_[sequencia_reordenamento]), dim = c(qtd_cidades,qtd_anos_simulacao,qtd_simulacoes_validas),dimnames = list(nomes_cidades,nomes_anos_simulacao,nomes_simulacoes_validas))

networks_list_global$fitbass_p = unlist(networks_list_global$fitbass_p[sequencia_reordenamento]) 
networks_list_global$fitbass_q = unlist(networks_list_global$fitbass_q[sequencia_reordenamento]) 

plot(networks_list_global$limiteinferior_,networks_list_global$limitesuperior_)
#save(networks_list_global,file='networks_list_global.Rdata')

# busca os intervalos inferiores únicos, sendo que uma curva será gerada parea cada intervalo inferior
curvas = unique(networks_list_global$limiteinferior_)
print(curvas)
# busca cada uma das faixas intervalos que contém o mesmo limite inferior 
intervalos = findInterval(curvas, networks_list_global$limiteinferior_)
print(intervalos)

# vai imprimir as várias curvas, com a quantidade de arestas para o ano
idx_anterior_ini_lim_inf<-1 #1276
for (i in 1:length(curvas)) {
   #i<-97
   lim_inf <- curvas[i]
   idx_atual_fim_lim_inf <- intervalos[i]
   nomeCurva<-paste('(',lim_inf,')')
   print(paste('Curva com nome ',nomeCurva,
               ' vai de ',idx_anterior_ini_lim_inf,' a ',idx_atual_fim_lim_inf,
               ' com ',(idx_atual_fim_lim_inf-idx_anterior_ini_lim_inf+1),' elementos.'))
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
   text(x,y,nomeCurva)
   print(paste('Plotando curva terminando em ',x,',',y))
   lines(networks_list_global$limitesuperior_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf],
         networks_list_global$qtd_arestas_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf])
   idx_anterior_ini_lim_inf<-idx_atual_fim_lim_inf+1
}


# vai imprimir as várias curvas, com o índice de moran para o ano de 2009
for (j in 1:length(nomes_anos_simulacao)) {
  ano <- nomes_anos_simulacao[j]
  idx_anterior_ini_lim_inf<-1 #1276
  for (i in 1:length(curvas)) {
    #i<-97
    lim_inf <- curvas[i]
    idx_atual_fim_lim_inf <- intervalos[i]
    nomeCurva<-paste('(',lim_inf,')')
    print(paste('Curva com nome ',nomeCurva,
                ' vai de ',idx_anterior_ini_lim_inf,' a ',idx_atual_fim_lim_inf,
                ' com ',(idx_atual_fim_lim_inf-idx_anterior_ini_lim_inf+1),' elementos.'))
    if (i == 1) { # gera uma plotagem inicial
      plot(NULL,
           xlim=c(min(networks_list_global$limitesuperior_-lim_inf),max(networks_list_global$limitesuperior_-lim_inf)),
           ylim=c(min(networks_list_global$moran_obs_[,j]),max(networks_list_global$moran_obs_[,j])),
           main='Moran I / adoção LIRAa / Intervalo de deslocamento',
           ylab=paste('Moran I (',ano,')'),
           xlab='Amplitude do intervalo')
    }
    x<-networks_list_global$limitesuperior_[idx_atual_fim_lim_inf]
    y<-networks_list_global$moran_obs_[idx_atual_fim_lim_inf,j]
    #text(x,y,nomeCurva)
    print(paste('Plotando curva terminando em ',x,',',y))
    valores_x<-networks_list_global$limitesuperior_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf]
    valores_y<-networks_list_global$moran_obs_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf,j]
    validos<-(networks_list_global$moran_pval_[idx_anterior_ini_lim_inf:idx_atual_fim_lim_inf,j])<10e-3
    lines(valores_x[validos],
          valores_y[validos])
    idx_anterior_ini_lim_inf<-idx_atual_fim_lim_inf+1
  }
}


