library(readxl)
library(stringr)
library(ggplot2)
library(dplyr)
library(xlsx)


Ncol <- c('Municipio', 'UF', 'IIP')

tab2009 <- read_excel("dados/IIP - IB (2009).xls", col_names = Ncol)
tab2010 <- read_excel("dados/IIP - IB (2010).xls", col_names = Ncol)
tab2011 <- read_excel("dados/IIP - IB (2011).xls", col_names = Ncol)
tab2012 <- read_excel("dados/IIP - IB (2012).xls", col_names = Ncol)

Ncol <- c('IBGE', 'Municipio', 'UF', 'IIP', 'IB')

tab2013 <- read_excel("dados/IIP - IB (2013).xls", col_names = Ncol)
tab2014 <- read_excel("dados/IIP - IB (2014).xls", col_names = Ncol)
tab2015 <- read_excel("dados/IIP - IB (2015).xls", col_names = Ncol)
tab2016 <- read_excel("dados/IIP - IB (2016).xls", col_names = Ncol)

Ncol <- c('descarte', 'IBGE', 'Municipio', 'UF', 'Periodo_de_execucao','IIP', 'IB')

tab2017 <- read_excel("dados/IIP - IB (2017).xls", col_names = Ncol)

tab2009_1 <- tab2009
tab2009_1["ANO"] <- 2009
tab2009_1["IBGE"] <- NA
tab2009_1["IB"] <- NA
tab2009_1 <- tab2009_1[-1,]
tab2009_1 <- tab2009_1[c('IBGE', 'Municipio', 'UF', 'IIP', 'IB', 'ANO')]

tab2010_1 <- tab2010
tab2010_1["ANO"] <- 2010
tab2010_1["IBGE"] <- NA
tab2010_1["IB"] <- NA
tab2010_1 <- tab2010_1[-1,]
tab2010_1 <- tab2010_1[c('IBGE', 'Municipio', 'UF', 'IIP', 'IB', 'ANO')]

tab2011_1 <- tab2011
tab2011_1["ANO"] <- 2011
tab2011_1["IBGE"] <- NA
tab2011_1["IB"] <- NA
tab2011_1 <- tab2011_1[-1,]
tab2011_1 <- tab2011_1[c('IBGE', 'Municipio', 'UF', 'IIP', 'IB', 'ANO')]

tab2012_1 <- tab2012
tab2012_1["ANO"] <- 2012
tab2012_1["IBGE"] <- NA
tab2012_1["IB"] <- NA
tab2012_1 <- tab2012_1[-1,]
tab2012_1 <- tab2012_1[c('IBGE', 'Municipio', 'UF', 'IIP', 'IB', 'ANO')]

tab2013_1 <- tab2013[c(-1, -2, -3),]
tab2013_1["ANO"] <- 2013
tab2013_1 <- tab2013_1[c('IBGE', 'Municipio', 'UF', 'IIP', 'IB', 'ANO')]


tab2014_1 <- tab2014[c(-1, -2, -3),]
tab2014_1["ANO"] <- 2014
tab2014_1 <- tab2014_1[c('IBGE', 'Municipio', 'UF', 'IIP', 'IB', 'ANO')]

tab2015_1 <- tab2015[c(-1, -2, -3),]
tab2015_1["ANO"] <- 2015
tab2015_1 <- tab2015_1[c('IBGE', 'Municipio', 'UF', 'IIP', 'IB', 'ANO')]

tab2016_1 <- tab2016[c(-1, -2, -3),]
tab2016_1["ANO"] <- 2016
tab2016_1 <- tab2016_1[c('IBGE', 'Municipio', 'UF', 'IIP', 'IB', 'ANO')]

tab2017_1I <- tab2017[c(-1, -2, -3),]
tab2017_1I <- tab2017_1I[,-1]
tab2017_1 <- tab2017_1I[,-4]
tab2017_1I["ANO"] <- 2017
tab2017_1["ANO"] <- 2017
tab2017_1 <- tab2017_1[c('IBGE', 'Municipio', 'UF', 'IIP', 'IB', 'ANO')]

tab2009_1$Municipio <- toupper(tab2009_1$Municipio)
tab2010_1$Municipio <- toupper(tab2010_1$Municipio)
tab2011_1$Municipio <- toupper(tab2011_1$Municipio)
tab2012_1$Municipio <- toupper(tab2012_1$Municipio)
tab2013_1$Municipio <- toupper(tab2013_1$Municipio)
tab2014_1$Municipio <- toupper(tab2014_1$Municipio)
tab2015_1$Municipio <- toupper(tab2015_1$Municipio)
tab2016_1$Municipio <- toupper(tab2016_1$Municipio)
tab2017_1$Municipio <- toupper(tab2017_1$Municipio)

RmAcentos <- list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                  'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                  'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                  'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                  'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

tab2009_1$Municipio <- chartr(paste(names(RmAcentos), collapse=''),
                              paste(RmAcentos, collapse=''),
                              tab2009_1$Municipio)

tab2010_1$Municipio <- chartr(paste(names(RmAcentos), collapse=''),
                              paste(RmAcentos, collapse=''),
                              tab2010_1$Municipio)

tab2011_1$Municipio <- chartr(paste(names(RmAcentos), collapse=''),
                              paste(RmAcentos, collapse=''),
                              tab2011_1$Municipio)

tab2012_1$Municipio <- chartr(paste(names(RmAcentos), collapse=''),
                              paste(RmAcentos, collapse=''),
                              tab2012_1$Municipio)

tab2013_1$Municipio <- chartr(paste(names(RmAcentos), collapse=''),
                              paste(RmAcentos, collapse=''),
                              tab2013_1$Municipio)

tab2014_1$Municipio <- chartr(paste(names(RmAcentos), collapse=''),
                              paste(RmAcentos, collapse=''),
                              tab2014_1$Municipio)

tab2015_1$Municipio <- chartr(paste(names(RmAcentos), collapse=''),
                              paste(RmAcentos, collapse=''),
                              tab2015_1$Municipio)

tab2016_1$Municipio <- chartr(paste(names(RmAcentos), collapse=''),
                              paste(RmAcentos, collapse=''),
                              tab2016_1$Municipio)

tab2017_1$Municipio <- chartr(paste(names(RmAcentos), collapse=''),
                              paste(RmAcentos, collapse=''),
                              tab2017_1$Municipio)

Mu2012Ori <- tab2012_1$Municipio
tab2012_1$Municipio <- gsub("[*]", "", tab2012_1$Municipio) #criei uma variável para guardar os asteríscos
#caso seja necessário recuperar essa informação

#for (i in 1:length(tab2009_1$Municipio)){
# for (j in 1:length(tab2017_1$Municipio)){
#  if(tab2009_1$Municipio[i] == tab2017_1$Municipio[j]){
#   tab2009_1[i,1] <- tab2017_1[j,1]
#  break
#    }
# }
#}
MuUf2009 <- paste(tab2009_1$Municipio, tab2009_1$UF, sep = "")
MuUf2010 <- paste(tab2010_1$Municipio, tab2010_1$UF, sep = "")
MuUf2011 <- paste(tab2011_1$Municipio, tab2011_1$UF, sep = "")
MuUf2012 <- paste(tab2012_1$Municipio, tab2012_1$UF, sep = "")
MuUf2013 <- paste(tab2013_1$Municipio, tab2013_1$UF, sep = "")
MuUf2014 <- paste(tab2014_1$Municipio, tab2014_1$UF, sep = "")
MuUf2015 <- paste(tab2015_1$Municipio, tab2015_1$UF, sep = "")
MuUf2016 <- paste(tab2016_1$Municipio, tab2016_1$UF, sep = "")
MuUf2017 <- paste(tab2017_1$Municipio, tab2017_1$UF, sep = "")


matches1 <- match(MuUf2009, MuUf2017)
matches2 <- match(MuUf2009, MuUf2016)
matches3 <- match(MuUf2009, MuUf2015)
matches4 <- match(MuUf2009, MuUf2014)
matches5 <- match(MuUf2009, MuUf2013)

for (i in 1:length(tab2009_1$Municipio)){

  if(!is.na(matches1[i])) {
    tab2009_1[i, 1] <- tab2017_1$IBGE[matches1[i]]
  }

  if(!is.na(matches2[i])) {
    tab2009_1[i, 1] <- tab2016_1$IBGE[matches2[i]]
  }

  if(!is.na(matches3[i])) {
    tab2009_1[i, 1] <- tab2015_1$IBGE[matches3[i]]
  }

  if(!is.na(matches4[i])) {
    tab2009_1[i, 1] <- tab2014_1$IBGE[matches4[i]]
  }

  if(!is.na(matches5[i])) {
    tab2009_1[i, 1] <- tab2013_1$IBGE[matches5[i]]
  }
}

#for (i in 1:length(tab2010_1$Municipio)){
#  for (j in 1:length(tab2017_1$Municipio)){
#    if(tab2010_1$Municipio[i] == tab2017_1$Municipio[j]){
#      tab2010_1[i,1] <- tab2017_1[j,1]
#      break
#    }
#  }
#}

matches1 <- match(MuUf2010, MuUf2017)
matches2 <- match(MuUf2010, MuUf2016)
matches3 <- match(MuUf2010, MuUf2015)
matches4 <- match(MuUf2010, MuUf2014)
matches5 <- match(MuUf2010, MuUf2013)

for (i in 1:length(tab2010_1$Municipio)){

  if(!is.na(matches1[i])) {
    tab2010_1[i, 1] <- tab2017_1$IBGE[matches1[i]]
  }

  if(!is.na(matches2[i])) {
    tab2010_1[i, 1] <- tab2016_1$IBGE[matches2[i]]
  }

  if(!is.na(matches3[i])) {
    tab2010_1[i, 1] <- tab2015_1$IBGE[matches3[i]]
  }

  if(!is.na(matches4[i])) {
    tab2010_1[i, 1] <- tab2014_1$IBGE[matches4[i]]
  }

  if(!is.na(matches5[i])) {
    tab2010_1[i, 1] <- tab2013_1$IBGE[matches5[i]]
  }
}

#for (i in 1:length(tab2011_1$Municipio)){
#  for (j in 1:length(tab2017_1$Municipio)){
#    if(tab2011_1$Municipio[i] == tab2017_1$Municipio[j]){
#      tab2011_1[i,1] <- tab2017_1[j,1]
#      break
#    }
#  }
#}

matches1 <- match(MuUf2011, MuUf2017)
matches2 <- match(MuUf2011, MuUf2016)
matches3 <- match(MuUf2011, MuUf2015)
matches4 <- match(MuUf2011, MuUf2014)
matches5 <- match(MuUf2011, MuUf2013)

for (i in 1:length(tab2011_1$Municipio)){

  if(!is.na(matches1[i])) {
    tab2011_1[i, 1] <- tab2017_1$IBGE[matches1[i]]
  }

  if(!is.na(matches2[i])) {
    tab2011_1[i, 1] <- tab2016_1$IBGE[matches2[i]]
  }

  if(!is.na(matches3[i])) {
    tab2011_1[i, 1] <- tab2015_1$IBGE[matches3[i]]
  }

  if(!is.na(matches4[i])) {
    tab2011_1[i, 1] <- tab2014_1$IBGE[matches4[i]]
  }

  if(!is.na(matches5[i])) {
    tab2011_1[i, 1] <- tab2013_1$IBGE[matches5[i]]
  }
}

#for (i in 1:length(tab2012_1$Municipio)){
#  for (j in 1:length(tab2017_1$Municipio)){
#    if(tab2012_1$Municipio[i] == tab2017_1$Municipio[j]){
#      tab2012_1[i,1] <- tab2017_1[j,1]
#      break
#    }
#  }
#}


#exemplo de conformação anterior: matches1 <- match(tab2012_1$Municipio, tab2017_1$Municipio)
matches1 <- match(MuUf2012, MuUf2017)
matches2 <- match(MuUf2012, MuUf2016)
matches3 <- match(MuUf2012, MuUf2015)
matches4 <- match(MuUf2012, MuUf2014)
matches5 <- match(MuUf2012, MuUf2013)

for (i in 1:length(tab2012_1$Municipio)){

  if(!is.na(matches1[i])) {
    tab2012_1[i, 1] <- tab2017_1$IBGE[matches1[i]]
  }

  if(!is.na(matches2[i])) {
    tab2012_1[i, 1] <- tab2016_1$IBGE[matches2[i]]
  }

  if(!is.na(matches3[i])) {
    tab2012_1[i, 1] <- tab2015_1$IBGE[matches3[i]]
  }

  if(!is.na(matches4[i])) {
    tab2012_1[i, 1] <- tab2014_1$IBGE[matches4[i]]
  }

  if(!is.na(matches5[i])) {
    tab2012_1[i, 1] <- tab2013_1$IBGE[matches5[i]]
  }
}

#Encontro "manual" dos restantes:
tab2009_1$IBGE[match("GUAJARAMIRIM", tab2009_1$Municipio)] <- tab2017_1$IBGE[
  agrep("GUAJARAMIR", tab2017_1$Municipio, max.distance = 1)]

tab2009_1$IBGE[match("JI PARANA", tab2009_1$Municipio)] <- tab2017_1$IBGE[
  agrep("JI PARANA", tab2017_1$Municipio, max.distance = 1)]

tab2009_1$IBGE[match("SAO JOSE DO RIBAMAR", tab2009_1$Municipio)] <- tab2017_1$IBGE[
  agrep("S J D RIBAMAR", tab2017_1$Municipio, max.distance  = 3)]

tab2009_1$IBGE[match("CABO DE S. AGOSTINHO", tab2009_1$Municipio)] <- tab2017_1$IBGE[
  agrep("C D S AGOSTINHO", tab2017_1$Municipio, max.distance  = 5)]

tab2009_1$IBGE[match("CARAMAGIBE", tab2009_1$Municipio)] <- tab2017_1$IBGE[1401]

tab2009_1$IBGE[match("NOSSA S. SOCORRO", tab2009_1$Municipio)] <- tab2017_1$IBGE[
  agrep("N S SOCORRO", tab2017_1$Municipio, max.distance = 3)]

tab2009_1$IBGE[match("S. J. RIO PRETO", tab2009_1$Municipio)] <- tab2017_1$IBGE[
  agrep("SAO JOSE RIO PRETO", tab2017_1$Municipio, max.distance = 3)]
#Belfor roxo -> Não encontrado por agrep
#em nenhum dos anos de 2013 a 2017(máxima dist?ncia de 5)

tab2009_1$IBGE[match("C. GOYTACAZES", tab2009_1$Municipio)] <- tab2017_1$IBGE[
  agrep("C GOYTACAZES", tab2017_1$Municipio, max.distance = 1)]

tab2009_1$IBGE[match("S. J. DE MERITI", tab2009_1$Municipio)] <- tab2017_1$IBGE[
  agrep("S J D MERITI", tab2017_1$Municipio, max.distance = 4)]

tab2009_1$IBGE[match("C. ITAPEMIRIM", tab2009_1$Municipio)] <- tab2017_1$IBGE[
  agrep("C ITAPEMIRIM", tab2017_1$Municipio, max.distance = 1)]

tab2009_1$IBGE[match("APAUCARANA", tab2009_1$Municipio)] <- tab2017_1$IBGE[
  agrep("APAUCARA", tab2017_1$Municipio, max.distance = 1)]

tab2010_1$IBGE[match("GUAJARAMIRIM", tab2010_1$Municipio)] <- tab2017_1$IBGE[
  agrep("GUAJARAMIR", tab2017_1$Municipio, max.distance = 1)]

tab2010_1$IBGE[match("JI PARANA", tab2010_1$Municipio)] <- tab2017_1$IBGE[
  agrep("JI PARANA", tab2017_1$Municipio, max.distance = 1)]

tab2010_1$IBGE[match("SAO JOSE DO RIBAMAR", tab2010_1$Municipio)] <- tab2017_1$IBGE[
  agrep("S J D RIBAMAR", tab2017_1$Municipio, max.distance  = 3)]

tab2010_1$IBGE[match("ITAMARACA", tab2010_1$Municipio)] <- tab2017_1$IBGE[1446]

tab2010_1$IBGE[match("NOSSA S. SOCORRO", tab2010_1$Municipio)] <- tab2017_1$IBGE[
  agrep("N S SOCORRO", tab2017_1$Municipio, max.distance = 3)]

tab2010_1$IBGE[match("MOJI-GUACU", tab2010_1$Municipio)] <- tab2017_1$IBGE[
  agrep("MOJI GUACU", tab2017_1$Municipio, max.distance = 1)]

tab2010_1$IBGE[match("MOJI-MIRIM", tab2010_1$Municipio)] <- tab2017_1$IBGE[
  agrep("MOJI MIRIM", tab2017_1$Municipio, max.distance = 1)]

tab2010_1$IBGE[match("RODEIROS", tab2010_1$Municipio)] <- tab2017_1$IBGE[2714]

#IPABA não foi encontrada em 2017, a priore julguei melhor o
#preenchimento manual

#Idem para Santa Cruz de minas
#Deve-se checar o UF em todas as considerações dos "matches"

tab2010_1$IBGE[match("CACHOEIRO DO ITAPEMIRIM", tab2010_1$Municipio)] <- tab2017_1$IBGE[
  agrep("CACHOEIRO DO ITAPEMIRIM", tab2017_1$Municipio, max.distance = 1)]

#planaltina de goias não foi encontrado em nenhuma das tabelas

tab2010_1$IBGE[match("BOTAGUASSU", tab2010_1$Municipio)] <- tab2017_1$IBGE[
  agrep("BOTAGUAS", tab2017_1$Municipio, max.distance = 1)]

tab2010_1$IBGE[match("NOVA ADRADINA", tab2010_1$Municipio)] <- tab2017_1$IBGE[
  agrep("NOVA ADRADINA", tab2017_1$Municipio, max.distance = 2)]

tab2010_1$IBGE[match("GUIRA", tab2010_1$Municipio)] <- tab2017_1$IBGE[3804]

tab2011_1$IBGE[match("ITAMARACA", tab2011_1$Municipio)] <- tab2017_1$IBGE[1446]

tab2011_1$IBGE[match("MOJI-GUACU", tab2011_1$Municipio)] <- tab2017_1$IBGE[
  agrep("MOJI GUACU", tab2017_1$Municipio, max.distance = 1)]

tab2011_1$IBGE[match("MOJI-MIRIM", tab2011_1$Municipio)] <- tab2017_1$IBGE[
  agrep("MOJI MIRIM", tab2017_1$Municipio, max.distance = 1)]

tab2011_1$IBGE[match("NOVA ADRADINA", tab2011_1$Municipio)] <- tab2017_1$IBGE[
  agrep("NOVA ADRADINA", tab2017_1$Municipio, max.distance = 2)]
#não foi encontrado o Municipio de paraty nas tabelas de 2013 a 2017,
#distancia 1 e 1 a 5 para 2017

tab2011_1$IBGE[match("SANTA TEREZINHA DO ITAIPU", tab2011_1$Municipio)] <- tab2017_1$IBGE[
  agrep("S T D ITAIPU", tab2017_1$Municipio, max.distance = 4)]

tab2012_1$IBGE[match("MOJI MIRIM", tab2012_1$Municipio)] <- tab2017_1$IBGE[
  agrep("MOJI MIRIM", tab2017_1$Municipio, max.distance = 1)]

tab2012_1$IBGE[match("ITAMARACA", tab2012_1$Municipio)] <- tab2017_1$IBGE[1446]

tab2012_1$IBGE[match("SITIO DABADIA", tab2012_1$Municipio)] <- tab2017_1$IBGE[
  agrep("SITIO DABADIA", tab2017_1$Municipio, max.distance = 1)]

tab2012_1$IBGE[match("AGUAS LINDAS", tab2012_1$Municipio)] <- tab2017_1$IBGE[
  agrep("AGUAS LINDAS", tab2017_1$Municipio, max.distance = 1)]

#Municipio de jauru não foi encontrado em nenhuma das tabelas
#Municipio Gaucha do norte não foi encontrado em nenhuma das tabelas
#Municipio figueir?polis do oeste não foi encontrado em nenhuma das tabelas

tab2012_1$IBGE[match("MURITINGA DO SUL", tab2012_1$Municipio)] <- tab2017_1$IBGE[
  agrep("MURITINGA D S", tab2017_1$Municipio, max.distance = 2)]

#Foi encontrada IPAU?U ? o mesmo que IPAUSSU?

tab2012_1$IBGE[match("GUARANI D'OESTE", tab2012_1$Municipio)] <- tab2017_1$IBGE[
  agrep("GUARANI OESTE", tab2017_1$Municipio, max.distance = 2)]

#não foi encontrado engenheiro coelho em nenhuma das tabelas

tab2012_1$IBGE[match("CAPAO BONITO", tab2012_1$Municipio)] <- tab2017_1$IBGE[3167]

tab2012_1$IBGE[match("BOA ESPERNCA DO SUL", tab2012_1$Municipio)] <- tab2017_1$IBGE[
  agrep("BOA ESPER DO SUL", tab2017_1$Municipio, max.distance = 4)]

tab2012_1$IBGE[match("BENTO DE ABERU", tab2012_1$Municipio)] <- tab2017_1$IBGE[
  agrep("BENTO DE AB", tab2017_1$Municipio, max.distance = 1)]

tab2012_1$IBGE[match("APARECIDA D'OESTE", tab2012_1$Municipio)] <- tab2017_1$IBGE[
  agrep("APARECIDA D OESTE", tab2017_1$Municipio, max.distance = 1)]

#S?o Jose Do Goiabal não foi encontrado em nenhuma das tabelas
#Santa Cruz De Minas não foi encontrado em nenhuma das tabelas
#Marilac não foi encontrado em nenhuma das tabelas
#Itanhomi não foi encontrado em nenhuma das tabelas
#Itabirinha não foi encontrado em nenhuma das tabelas
#Ipaba não foi encontrado, como j? acima citado
#dom cavati não foi encontrado em nenhuma das tabelas
#Capitao andrade não foi encontrado em nenhuma das tabelas
#Belo Oriente não foi encontrado em nenhuma das tabelas
#S?o Caetano não foi encontrado em nenhuma das tabelas

#Em ambas as buscas abaixo os nomes estavam iguais, procurar saber o que est? falhando
tab2012_1$IBGE[match("CAMOCIM", tab2012_1$Municipio)] <- tab2017_1$IBGE[822]

tab2012_1$IBGE[match("ARACATI", tab2012_1$Municipio)] <- tab2017_1$IBGE[803]

tab2012_1$IBGE[match("SAO FELIX DO PIAUI", tab2012_1$Municipio)] <- tab2017_1$IBGE[
  agrep("SAO FELIX DO PIAUI", tab2017_1$Municipio, max.distance = 1)]

#Sao Salvador de tocantins não foi encontrado em nenhuma das tabelas
#Sao bento de tocantins não foi encontrado em nenhuma das tabelas
#Santa Maria Do Tocantins não foi encontrado em nenhuma das tabelas
#Rio Dos bois não foi encontrado em nenhuma das tabelas
#Palmeiropolis não foi encontrado em nenhuma das tabelas
#Maurilandia Do Tocantins não foi encontrado em nenhuma das tabelas
#Marianopolis Do Tocantins não foi encontrado em nenhuma das tabelas
#Crixas Do Tocantins não foi encontrado em nenhuma das tabelas
#Aguiarnopolis não foi encontrado em nenhuma das tabelas

#Preenchimento manual das tabelas:
tab2012_1$IBGE[match("NOVO HORIZONTERO", MuUf2012)] <- 110050
tab2012_1$IBGE[match("AGUIARNOPOLISTO", MuUf2012)] <- 170030
tab2012_1$IBGE[match("CRIXAS DO TOCANTINSTO", MuUf2012)] <- 170625
tab2012_1$IBGE[match("MARIANOPOLIS DO TOCANTINSTO", MuUf2012)] <- 171250
tab2012_1$IBGE[match("MAURILANDIA DO TOCANTINSTO", MuUf2012)] <- 171280
tab2012_1$IBGE[match("PALMEIROPOLISTO", MuUf2012)] <- 171575
tab2012_1$IBGE[match("RIO DOS BOISTO", MuUf2012)] <- 171870
tab2012_1$IBGE[match("SANTA MARIA DO TOCANTINSTO", MuUf2012)] <- 171888
tab2012_1$IBGE[match("SAO BENTO DO TOCANTINSTO", MuUf2012)] <- 172010
tab2012_1$IBGE[match("SAO SALVADOR DO TOCANTINSTO", MuUf2012)] <- 172025
tab2012_1$IBGE[match("AGUA BRANCAPI", MuUf2012)] <- 220020
tab2012_1$IBGE[match("SAO CAETANOPE", MuUf2012)] <- 261310
tab2012_1$IBGE[match("BELO ORIENTEMG", MuUf2012)] <- 310630
tab2012_1$IBGE[match("CAPITAO ANDRADEMG", MuUf2012)] <- 3112653
tab2012_1$IBGE[match("DOM CAVATIMG", MuUf2012)] <- 312250
tab2012_1$IBGE[match("IPABAMG", MuUf2012)] <- 313115
tab2012_1$IBGE[match("ITABIRINHAMG", MuUf2012)] <- 313180
tab2012_1$IBGE[match("ITANHOMIMG", MuUf2012)] <- 313320
tab2012_1$IBGE[match("MARILACMG", MuUf2012)] <- 314010
tab2012_1$IBGE[match("MESQUITAMG", MuUf2012)] <- 314170
tab2012_1$IBGE[match("SANTA CRUZ DE MINASMG", MuUf2012)] <- 315733
tab2012_1$IBGE[match("SAO JOSE DO GOIABALMG", MuUf2012)] <- 316340
tab2012_1$IBGE[match("ENGENHEIRO COELHOSP", MuUf2012)] <- 351515
tab2012_1$IBGE[match("IPAUSSUSP", MuUf2012)] <- 352090
tab2012_1$IBGE[match("FIGUEIROPOLIS D'OESTEMT", MuUf2012)] <- 510380
tab2012_1$IBGE[match("GAUCHA DO NORTEMT", MuUf2012)] <- 510385
tab2012_1$IBGE[match("JAURUMT", MuUf2012)] <- 510500
tab2012_1$IBGE[match("BOM JARDIMGO", MuUf2012)] <- 520340
tab2012_1$IBGE[match("MORRO AGUDOGO", MuUf2012)] <- 521385


#Inicio da transformação dos dados pata a sua posterior visualiza??o:
tabConsolidadaCheia <- rbind(tab2009_1, tab2010_1, tab2011_1,
                             tab2012_1, tab2013_1, tab2013_1,
                             tab2014_1, tab2015_1, tab2016_1,
                             tab2017_1)

tabConsolidada$Ano <- as.factor(tabConsolidada$ANO)

tabConsolidada <- tabConsolidadaCheia
tabConsolidada$IIP <- as.numeric(tabConsolidada$IIP)

UFNorte <- (str_detect(tabConsolidada$UF, "AM") |
              str_detect(tabConsolidada$UF, "RR") |
              str_detect(tabConsolidada$UF, "AP") |
              str_detect(tabConsolidada$UF, "PA") |
              str_detect(tabConsolidada$UF, "TO") |
              str_detect(tabConsolidada$UF, "RO") |
              str_detect(tabConsolidada$UF, "AC"))

UFNordeste <- (str_detect(tabConsolidada$UF, "MA") |
                 str_detect(tabConsolidada$UF, "PI") |
                 str_detect(tabConsolidada$UF, "CE") |
                 str_detect(tabConsolidada$UF, "RN") |
                 str_detect(tabConsolidada$UF, "PE") |
                 str_detect(tabConsolidada$UF, "PB") |
                 str_detect(tabConsolidada$UF, "SE") |
                 str_detect(tabConsolidada$UF, "AL") |
                 str_detect(tabConsolidada$UF, "BA"))

UFCentro <- (str_detect(tabConsolidada$UF, "MT") |
               str_detect(tabConsolidada$UF, "MS") |
               str_detect(tabConsolidada$UF, "GO"))

UFSudeste <- (str_detect(tabConsolidada$UF, "SP") |
                str_detect(tabConsolidada$UF, "RJ") |
                str_detect(tabConsolidada$UF, "ES") |
                str_detect(tabConsolidada$UF, "MG"))

UFSul <- (str_detect(tabConsolidada$UF, "PR") |
            str_detect(tabConsolidada$UF, "RS") |
            str_detect(tabConsolidada$UF, "SC"))

tabNorte <- tabConsolidada[UFNorte, ]
tabNordeste <- tabConsolidada[UFNordeste, ]
tabCentro <- tabConsolidada[UFCentro, ]
tabSudeste <- tabConsolidada[UFSudeste, ]
tabSul <- tabConsolidada[UFSul, ]

mediaGeralBr <- mean(na.omit(tabConsolidada$IIP))
mediaGeralNorte <- mean(na.omit(tabNorte$IIP))
mediaGeralNordeste <- mean(na.omit(tabNordeste$IIP))
mediaGeralCentro <- mean(na.omit(tabCentro$IIP))
mediaGeralSudeste <- mean(na.omit(tabSudeste$IIP))
mediaGeralSul <- mean(na.omit(tabSul$IIP))

qualidadeIIP <- (sum(is.na(tabConsolidada$IIP))/length(tabConsolidada$IIP)) * 100
qualidadeIB <- (sum(is.na(tabConsolidada$IB))/length(tabConsolidada$IB)) * 100
qualidadeIBGE <- (sum(is.na(tabConsolidada$IBGE))/length(tabConsolidada$IBGE)) * 100

Consolidada2009 <- str_detect(tabConsolidada$ANO, "2009")
Consolidada2010 <- str_detect(tabConsolidada$ANO, "2010")
Consolidada2011 <- str_detect(tabConsolidada$ANO, "2011")
Consolidada2012 <- str_detect(tabConsolidada$ANO, "2012")
Consolidada2013 <- str_detect(tabConsolidada$ANO, "2013")
Consolidada2014 <- str_detect(tabConsolidada$ANO, "2014")
Consolidada2015 <- str_detect(tabConsolidada$ANO, "2015")
Consolidada2016 <- str_detect(tabConsolidada$ANO, "2016")
Consolidada2017 <- str_detect(tabConsolidada$ANO, "2017")

tabConsolidada2009 <- tabConsolidada[Consolidada2009,]
tabConsolidada2010 <- tabConsolidada[Consolidada2010,]
tabConsolidada2011 <- tabConsolidada[Consolidada2011,]
tabConsolidada2012 <- tabConsolidada[Consolidada2012,]
tabConsolidada2013 <- tabConsolidada[Consolidada2013,]
tabConsolidada2014 <- tabConsolidada[Consolidada2014,]
tabConsolidada2015 <- tabConsolidada[Consolidada2015,]
tabConsolidada2016 <- tabConsolidada[Consolidada2016,]
tabConsolidada2017 <- tabConsolidada[Consolidada2017,]

mediaConsolidada2009 <- mean(na.omit(tabConsolidada2009$IIP))
mediaConsolidada2010 <- mean(na.omit(tabConsolidada2010$IIP))
mediaConsolidada2011 <- mean(na.omit(tabConsolidada2011$IIP))
mediaConsolidada2012 <- mean(na.omit(tabConsolidada2012$IIP))
mediaConsolidada2013 <- mean(na.omit(tabConsolidada2013$IIP))
mediaConsolidada2014 <- mean(na.omit(tabConsolidada2014$IIP))
mediaConsolidada2015 <- mean(na.omit(tabConsolidada2015$IIP))
mediaConsolidada2016 <- mean(na.omit(tabConsolidada2016$IIP))
mediaConsolidada2017 <- mean(na.omit(tabConsolidada2017$IIP))

mediaConsolidada2009["ANO"] <- 2009
mediaConsolidada2010["ANO"] <- 2010
mediaConsolidada2011["ANO"] <- 2011
mediaConsolidada2012["ANO"] <- 2012
mediaConsolidada2013["ANO"] <- 2013
mediaConsolidada2014["ANO"] <- 2014
mediaConsolidada2015["ANO"] <- 2015
mediaConsolidada2016["ANO"] <- 2016
mediaConsolidada2017["ANO"] <- 2017

mediaConsolidada <- rbind(mediaConsolidada2017, mediaConsolidada2016,
                          mediaConsolidada2015, mediaConsolidada2014,
                          mediaConsolidada2013, mediaConsolidada2012,
                          mediaConsolidada2011, mediaConsolidada2010,
                          mediaConsolidada2009)

mediaConsolidada <- as.data.frame(mediaConsolidada)
NCol <- c("MediaIIP", "ANO")
mediaConsolidada <- `colnames<-`(mediaConsolidada, NCol)
mediaConsolidada <- arrange(mediaConsolidada, ANO)

barplot(mediaConsolidada$MediaIIP, names.arg = mediaConsolidada$ANO,
        main = "Média do IIP No Brasil Para Cada ANO")

Consolidada2009 <- str_detect(tabNordeste$ANO, "2009")
Consolidada2010 <- str_detect(tabNordeste$ANO, "2010")
Consolidada2011 <- str_detect(tabNordeste$ANO, "2011")
Consolidada2012 <- str_detect(tabNordeste$ANO, "2012")
Consolidada2013 <- str_detect(tabNordeste$ANO, "2013")
Consolidada2014 <- str_detect(tabNordeste$ANO, "2014")
Consolidada2015 <- str_detect(tabNordeste$ANO, "2015")
Consolidada2016 <- str_detect(tabNordeste$ANO, "2016")
Consolidada2017 <- str_detect(tabNordeste$ANO, "2017")

tabConsolidada2009Nor <- tabNordeste[Consolidada2009,]
tabConsolidada2010Nor <- tabNordeste[Consolidada2010,]
tabConsolidada2011Nor <- tabNordeste[Consolidada2011,]
tabConsolidada2012Nor <- tabNordeste[Consolidada2012,]
tabConsolidada2013Nor <- tabNordeste[Consolidada2013,]
tabConsolidada2014Nor <- tabNordeste[Consolidada2014,]
tabConsolidada2015Nor <- tabNordeste[Consolidada2015,]
tabConsolidada2016Nor <- tabNordeste[Consolidada2016,]
tabConsolidada2017Nor <- tabNordeste[Consolidada2017,]

mediaConsolidada2009Nor <- mean(na.omit(tabConsolidada2009Nor$IIP))
mediaConsolidada2010Nor <- mean(na.omit(tabConsolidada2010Nor$IIP))
mediaConsolidada2011Nor <- mean(na.omit(tabConsolidada2011Nor$IIP))
mediaConsolidada2012Nor <- mean(na.omit(tabConsolidada2012Nor$IIP))
mediaConsolidada2013Nor <- mean(na.omit(tabConsolidada2013Nor$IIP))
mediaConsolidada2014Nor <- mean(na.omit(tabConsolidada2014Nor$IIP))
mediaConsolidada2015Nor <- mean(na.omit(tabConsolidada2015Nor$IIP))
mediaConsolidada2016Nor <- mean(na.omit(tabConsolidada2016Nor$IIP))
mediaConsolidada2017Nor <- mean(na.omit(tabConsolidada2017Nor$IIP))

mediaConsolidada2009Nor["ANO"] <- 2009
mediaConsolidada2010Nor["ANO"] <- 2010
mediaConsolidada2011Nor["ANO"] <- 2011
mediaConsolidada2012Nor["ANO"] <- 2012
mediaConsolidada2013Nor["ANO"] <- 2013
mediaConsolidada2014Nor["ANO"] <- 2014
mediaConsolidada2015Nor["ANO"] <- 2015
mediaConsolidada2016Nor["ANO"] <- 2016
mediaConsolidada2017Nor["ANO"] <- 2017

mediaConsolidadaNor <- rbind(mediaConsolidada2017Nor, mediaConsolidada2016Nor,
                             mediaConsolidada2015Nor, mediaConsolidada2014Nor,
                             mediaConsolidada2013Nor, mediaConsolidada2012Nor,
                             mediaConsolidada2011Nor, mediaConsolidada2010Nor,
                             mediaConsolidada2009Nor)

mediaConsolidadaNor <- as.data.frame(mediaConsolidadaNor)
NCol <- c("MediaIIP", "ANO")
mediaConsolidadaNor <- `colnames<-`(mediaConsolidadaNor, NCol)
mediaConsolidadaNor <- arrange(mediaConsolidadaNor, ANO)

barplot(mediaConsolidadaNor$MediaIIP, names.arg = mediaConsolidadaNor$ANO,
        main = "Média do IIP No Nordeste Para Cada ANO")

Consolidada2009 <- str_detect(tabNorte$ANO, "2009")
Consolidada2010 <- str_detect(tabNorte$ANO, "2010")
Consolidada2011 <- str_detect(tabNorte$ANO, "2011")
Consolidada2012 <- str_detect(tabNorte$ANO, "2012")
Consolidada2013 <- str_detect(tabNorte$ANO, "2013")
Consolidada2014 <- str_detect(tabNorte$ANO, "2014")
Consolidada2015 <- str_detect(tabNorte$ANO, "2015")
Consolidada2016 <- str_detect(tabNorte$ANO, "2016")
Consolidada2017 <- str_detect(tabNorte$ANO, "2017")

tabConsolidada2009Norte <- tabNorte[Consolidada2009,]
tabConsolidada2010Norte <- tabNorte[Consolidada2010,]
tabConsolidada2011Norte <- tabNorte[Consolidada2011,]
tabConsolidada2012Norte <- tabNorte[Consolidada2012,]
tabConsolidada2013Norte <- tabNorte[Consolidada2013,]
tabConsolidada2014Norte <- tabNorte[Consolidada2014,]
tabConsolidada2015Norte <- tabNorte[Consolidada2015,]
tabConsolidada2016Norte <- tabNorte[Consolidada2016,]
tabConsolidada2017Norte <- tabNorte[Consolidada2017,]

mediaConsolidada2009Norte <- mean(na.omit(tabConsolidada2009Norte$IIP))
mediaConsolidada2010Norte <- mean(na.omit(tabConsolidada2010Norte$IIP))
mediaConsolidada2011Norte <- mean(na.omit(tabConsolidada2011Norte$IIP))
mediaConsolidada2012Norte <- mean(na.omit(tabConsolidada2012Norte$IIP))
mediaConsolidada2013Norte <- mean(na.omit(tabConsolidada2013Norte$IIP))
mediaConsolidada2014Norte <- mean(na.omit(tabConsolidada2014Norte$IIP))
mediaConsolidada2015Norte <- mean(na.omit(tabConsolidada2015Norte$IIP))
mediaConsolidada2016Norte <- mean(na.omit(tabConsolidada2016Norte$IIP))
mediaConsolidada2017Norte <- mean(na.omit(tabConsolidada2017Norte$IIP))

mediaConsolidada2009Norte["ANO"] <- 2009
mediaConsolidada2010Norte["ANO"] <- 2010
mediaConsolidada2011Norte["ANO"] <- 2011
mediaConsolidada2012Norte["ANO"] <- 2012
mediaConsolidada2013Norte["ANO"] <- 2013
mediaConsolidada2014Norte["ANO"] <- 2014
mediaConsolidada2015Norte["ANO"] <- 2015
mediaConsolidada2016Norte["ANO"] <- 2016
mediaConsolidada2017Norte["ANO"] <- 2017

mediaConsolidadaNorte <- rbind(mediaConsolidada2017Norte, mediaConsolidada2016Norte,
                               mediaConsolidada2015Norte, mediaConsolidada2014Norte,
                               mediaConsolidada2013Norte, mediaConsolidada2012Norte,
                               mediaConsolidada2011Norte, mediaConsolidada2010Norte,
                               mediaConsolidada2009Norte)

mediaConsolidadaNorte <- as.data.frame(mediaConsolidadaNorte)
NCol <- c("MediaIIP", "ANO")
mediaConsolidadaNorte <- `colnames<-`(mediaConsolidadaNorte, NCol)
mediaConsolidadaNorte <- arrange(mediaConsolidadaNorte, ANO)

barplot(mediaConsolidadaNorte$MediaIIP, names.arg = mediaConsolidadaNorte$ANO,
        main = "Média do IIP No Norte Para Cada ANO")

Consolidada2009 <- str_detect(tabSudeste$ANO, "2009")
Consolidada2010 <- str_detect(tabSudeste$ANO, "2010")
Consolidada2011 <- str_detect(tabSudeste$ANO, "2011")
Consolidada2012 <- str_detect(tabSudeste$ANO, "2012")
Consolidada2013 <- str_detect(tabSudeste$ANO, "2013")
Consolidada2014 <- str_detect(tabSudeste$ANO, "2014")
Consolidada2015 <- str_detect(tabSudeste$ANO, "2015")
Consolidada2016 <- str_detect(tabSudeste$ANO, "2016")
Consolidada2017 <- str_detect(tabSudeste$ANO, "2017")

tabConsolidada2009Sud <- tabSudeste[Consolidada2009,]
tabConsolidada2010Sud <- tabSudeste[Consolidada2010,]
tabConsolidada2011Sud <- tabSudeste[Consolidada2011,]
tabConsolidada2012Sud <- tabSudeste[Consolidada2012,]
tabConsolidada2013Sud <- tabSudeste[Consolidada2013,]
tabConsolidada2014Sud <- tabSudeste[Consolidada2014,]
tabConsolidada2015Sud <- tabSudeste[Consolidada2015,]
tabConsolidada2016Sud <- tabSudeste[Consolidada2016,]
tabConsolidada2017Sud <- tabSudeste[Consolidada2017,]

mediaConsolidada2009Sud <- mean(na.omit(tabConsolidada2009Sud$IIP))
mediaConsolidada2010Sud <- mean(na.omit(tabConsolidada2010Sud$IIP))
mediaConsolidada2011Sud <- mean(na.omit(tabConsolidada2011Sud$IIP))
mediaConsolidada2012Sud <- mean(na.omit(tabConsolidada2012Sud$IIP))
mediaConsolidada2013Sud <- mean(na.omit(tabConsolidada2013Sud$IIP))
mediaConsolidada2014Sud <- mean(na.omit(tabConsolidada2014Sud$IIP))
mediaConsolidada2015Sud <- mean(na.omit(tabConsolidada2015Sud$IIP))
mediaConsolidada2016Sud <- mean(na.omit(tabConsolidada2016Sud$IIP))
mediaConsolidada2017Sud <- mean(na.omit(tabConsolidada2017Sud$IIP))

mediaConsolidada2009Sud["ANO"] <- 2009
mediaConsolidada2010Sud["ANO"] <- 2010
mediaConsolidada2011Sud["ANO"] <- 2011
mediaConsolidada2012Sud["ANO"] <- 2012
mediaConsolidada2013Sud["ANO"] <- 2013
mediaConsolidada2014Sud["ANO"] <- 2014
mediaConsolidada2015Sud["ANO"] <- 2015
mediaConsolidada2016Sud["ANO"] <- 2016
mediaConsolidada2017Sud["ANO"] <- 2017

mediaConsolidadaSud <- rbind(mediaConsolidada2017Sud, mediaConsolidada2016Sud,
                             mediaConsolidada2015Sud, mediaConsolidada2014Sud,
                             mediaConsolidada2013Sud, mediaConsolidada2012Sud,
                             mediaConsolidada2011Sud, mediaConsolidada2010Sud,
                             mediaConsolidada2009Sud)

mediaConsolidadaSud <- as.data.frame(mediaConsolidadaSud)
NCol <- c("MediaIIP", "ANO")
mediaConsolidadaSud <- `colnames<-`(mediaConsolidadaSud, NCol)
mediaConsolidadaSud <- arrange(mediaConsolidadaSud, ANO)

barplot(mediaConsolidadaSud$MediaIIP, names.arg = mediaConsolidadaSud$ANO,
        main = "Média do IIP No Sudeste Para Cada ANO")

Consolidada2009 <- str_detect(tabSul$ANO, "2009")
Consolidada2010 <- str_detect(tabSul$ANO, "2010")
Consolidada2011 <- str_detect(tabSul$ANO, "2011")
Consolidada2012 <- str_detect(tabSul$ANO, "2012")
Consolidada2013 <- str_detect(tabSul$ANO, "2013")
Consolidada2014 <- str_detect(tabSul$ANO, "2014")
Consolidada2015 <- str_detect(tabSul$ANO, "2015")
Consolidada2016 <- str_detect(tabSul$ANO, "2016")
Consolidada2017 <- str_detect(tabSul$ANO, "2017")

tabConsolidada2009Sul <- tabSul[Consolidada2009,]
tabConsolidada2010Sul <- tabSul[Consolidada2010,]
tabConsolidada2011Sul <- tabSul[Consolidada2011,]
tabConsolidada2012Sul <- tabSul[Consolidada2012,]
tabConsolidada2013Sul <- tabSul[Consolidada2013,]
tabConsolidada2014Sul <- tabSul[Consolidada2014,]
tabConsolidada2015Sul <- tabSul[Consolidada2015,]
tabConsolidada2016Sul <- tabSul[Consolidada2016,]
tabConsolidada2017Sul <- tabSul[Consolidada2017,]

mediaConsolidada2009Sul <- mean(na.omit(tabConsolidada2009Sul$IIP))
mediaConsolidada2010Sul <- mean(na.omit(tabConsolidada2010Sul$IIP))
mediaConsolidada2011Sul <- mean(na.omit(tabConsolidada2011Sul$IIP))
mediaConsolidada2012Sul <- mean(na.omit(tabConsolidada2012Sul$IIP))
mediaConsolidada2013Sul <- mean(na.omit(tabConsolidada2013Sul$IIP))
mediaConsolidada2014Sul <- mean(na.omit(tabConsolidada2014Sul$IIP))
mediaConsolidada2015Sul <- mean(na.omit(tabConsolidada2015Sul$IIP))
mediaConsolidada2016Sul <- mean(na.omit(tabConsolidada2016Sul$IIP))
mediaConsolidada2017Sul <- mean(na.omit(tabConsolidada2017Sul$IIP))

mediaConsolidada2009Sul["ANO"] <- 2009
mediaConsolidada2010Sul["ANO"] <- 2010
mediaConsolidada2011Sul["ANO"] <- 2011
mediaConsolidada2012Sul["ANO"] <- 2012
mediaConsolidada2013Sul["ANO"] <- 2013
mediaConsolidada2014Sul["ANO"] <- 2014
mediaConsolidada2015Sul["ANO"] <- 2015
mediaConsolidada2016Sul["ANO"] <- 2016
mediaConsolidada2017Sul["ANO"] <- 2017

mediaConsolidadaSul <- rbind(mediaConsolidada2017Sul, mediaConsolidada2016Sul,
                             mediaConsolidada2015Sul, mediaConsolidada2014Sul,
                             mediaConsolidada2013Sul, mediaConsolidada2012Sul,
                             mediaConsolidada2011Sul, mediaConsolidada2010Sul,
                             mediaConsolidada2009Sul)

mediaConsolidadaSul <- as.data.frame(mediaConsolidadaSul)
NCol <- c("MediaIIP", "ANO")
mediaConsolidadaSul <- `colnames<-`(mediaConsolidadaSul, NCol)
mediaConsolidadaSul <- arrange(mediaConsolidadaSul, ANO)

barplot(mediaConsolidadaSul$MediaIIP, names.arg = mediaConsolidadaSul$ANO,
        main = "Média do IIP No Sul Para Cada ANO")

Consolidada2009 <- str_detect(tabCentro$ANO, "2009")
Consolidada2010 <- str_detect(tabCentro$ANO, "2010")
Consolidada2011 <- str_detect(tabCentro$ANO, "2011")
Consolidada2012 <- str_detect(tabCentro$ANO, "2012")
Consolidada2013 <- str_detect(tabCentro$ANO, "2013")
Consolidada2014 <- str_detect(tabCentro$ANO, "2014")
Consolidada2015 <- str_detect(tabCentro$ANO, "2015")
Consolidada2016 <- str_detect(tabCentro$ANO, "2016")
Consolidada2017 <- str_detect(tabCentro$ANO, "2017")

tabConsolidada2009Cen <- tabCentro[Consolidada2009,]
tabConsolidada2010Cen <- tabCentro[Consolidada2010,]
tabConsolidada2011Cen <- tabCentro[Consolidada2011,]
tabConsolidada2012Cen <- tabCentro[Consolidada2012,]
tabConsolidada2013Cen <- tabCentro[Consolidada2013,]
tabConsolidada2014Cen <- tabCentro[Consolidada2014,]
tabConsolidada2015Cen <- tabCentro[Consolidada2015,]
tabConsolidada2016Cen <- tabCentro[Consolidada2016,]
tabConsolidada2017Cen <- tabCentro[Consolidada2017,]

mediaConsolidada2009Cen <- mean(na.omit(tabConsolidada2009Cen$IIP))
mediaConsolidada2010Cen <- mean(na.omit(tabConsolidada2010Cen$IIP))
mediaConsolidada2011Cen <- mean(na.omit(tabConsolidada2011Cen$IIP))
mediaConsolidada2012Cen <- mean(na.omit(tabConsolidada2012Cen$IIP))
mediaConsolidada2013Cen <- mean(na.omit(tabConsolidada2013Cen$IIP))
mediaConsolidada2014Cen <- mean(na.omit(tabConsolidada2014Cen$IIP))
mediaConsolidada2015Cen <- mean(na.omit(tabConsolidada2015Cen$IIP))
mediaConsolidada2016Cen <- mean(na.omit(tabConsolidada2016Cen$IIP))
mediaConsolidada2017Cen <- mean(na.omit(tabConsolidada2017Cen$IIP))

mediaConsolidada2009Cen["ANO"] <- 2009
mediaConsolidada2010Cen["ANO"] <- 2010
mediaConsolidada2011Cen["ANO"] <- 2011
mediaConsolidada2012Cen["ANO"] <- 2012
mediaConsolidada2013Cen["ANO"] <- 2013
mediaConsolidada2014Cen["ANO"] <- 2014
mediaConsolidada2015Cen["ANO"] <- 2015
mediaConsolidada2016Cen["ANO"] <- 2016
mediaConsolidada2017Cen["ANO"] <- 2017

mediaConsolidadaCen <- rbind(mediaConsolidada2017Cen, mediaConsolidada2016Cen,
                             mediaConsolidada2015Cen, mediaConsolidada2014Cen,
                             mediaConsolidada2013Cen, mediaConsolidada2012Cen,
                             mediaConsolidada2011Cen, mediaConsolidada2010Cen,
                             mediaConsolidada2009Cen)

mediaConsolidadaCen <- as.data.frame(mediaConsolidadaCen)
NCol <- c("MediaIIP", "ANO")
mediaConsolidadaCen <- `colnames<-`(mediaConsolidadaCen, NCol)
mediaConsolidadaCen <- arrange(mediaConsolidadaCen, ANO)

barplot(mediaConsolidadaCen$MediaIIP, names.arg = mediaConsolidadaCen$ANO,
        main = "Média do IIP No Centro-Oeste Para Cada ANO")

#plot da representatividade, utilizando ggplot2:
ggplot(tabConsolidada, aes(x = ANO)) +
  geom_histogram(binwidth = 1, position = "identity")


#Parte referente a segunda planinha, de 2016-2017:

Ncol <- c('UF', 'Municipio', 'IBGE', 'Populacao', 'Total_de_imoveis',
          'Municipios_com_visitas', 'Municipios_total', 'Visitas_realizadas', 'Imoveis_trabalhados',
          'Imoveis_recuperados', 'Total_imoveis_trabalhados', 'Imoveis_trabalhados_divided_by_visitados',
          'Imoveis_trabalhados_com_focos', 'Imoveis_recuperados_com_focos', 'Total_imoveis_com_focos', 'Imoveis_com_focos_divided_by_total_trabalhados',
          'Trabalhados_com_tratamento_larvicida', 'Recuperados_com_tratamento_larvicida', 'Total_tratamento_larvicida',
          'Imoveis_tratamento_larvicida_divided_by_total_trabalhados', 'Imoveis_fechados', 'Visitas_recusadas', 'Total_fechados_e_recusados',
          'Fechados_e_recusados_divided_by_visitados', 'Municipio_infestado_bool')

Atab2017Coleta <- read_excel("dados/tabela2017DadosDeColeta1.xls", col_names = Ncol)
Atab2016Coleta <- read_excel("dados/tabela2016DadosDeColeta.xls", col_names = Ncol)

Atab2016Coleta <- Atab2016Coleta[-1,]
Atab2017Coleta <- Atab2017Coleta[-1,]

Atab2016Coleta <- Atab2016Coleta[-5571,]
Atab2017Coleta <- Atab2017Coleta[-5571,]

Atab2016MVisitados <- filter(Atab2016Coleta, Atab2016Coleta$Municipios_com_visitas != 0)
Atab2017MVisitados <- filter(Atab2017Coleta, Atab2017Coleta$Municipios_com_visitas != 0)

Atab2016Coleta$Visitas_realizadas <- as.numeric(Atab2016Coleta$Visitas_realizadas)
Atab2016Coleta$Municipios_com_visitas <- as.numeric(Atab2016Coleta$Municipios_com_visitas)
Atab2016Coleta$Imoveis_fechados <- as.numeric(Atab2016Coleta$Imoveis_fechados)

Atab2017Coleta$Visitas_realizadas <- as.numeric(Atab2017Coleta$Visitas_realizadas)
Atab2017Coleta$Municipios_com_visitas <- as.numeric(Atab2017Coleta$Municipios_com_visitas)
Atab2017Coleta$Imoveis_fechados <- as.numeric(Atab2017Coleta$Imoveis_fechados)

Aaceitabilidade2016 <- sum(Atab2016Coleta$Imoveis_fechados)/
  sum(Atab2016Coleta$Visitas_realizadas) * 100

Aaceitabilidade2017 <- sum(Atab2017Coleta$Imoveis_fechados)/
  sum(Atab2017Coleta$Visitas_realizadas) * 100

Apontualidade <- sum(Atab2016MVisitados$Municipios_com_visitas)/
  sum(Atab2017MVisitados$Municipios_com_visitas) * 100

estabilidade2016Vis <- sum(is.na(Atab2016MVisitados$Visitas_recusadas))/
  length(Atab2016MVisitados$Visitas_recusadas) * 100

estabilidade2017Vis <- sum(is.na(Atab2017MVisitados$Visitas_recusadas))/
  length(Atab2017MVisitados$Visitas_recusadas)*100

estabilidade2016Foc <- sum(is.na(Atab2016MVisitados$Total_imoveis_com_focos))/
  length(Atab2016MVisitados$Total_imoveis_com_focos) * 100

estabilidade2017Foc <- sum(is.na(Atab2017MVisitados$Total_imoveis_com_focos))/
  length(Atab2017MVisitados$Total_imoveis_com_focos) * 100

Atab2016MVisitados$Municipio_infestado_bool <- str_replace(Atab2016MVisitados$Municipio_infestado_bool,
                                                         "Sim", "1")

Atab2016MVisitados$Municipio_infestado_bool <- str_replace(Atab2016MVisitados$Municipio_infestado_bool,
                                                         "não", "0")

Atab2017MVisitados$Municipio_infestado_bool <- str_replace(Atab2017MVisitados$Municipio_infestado_bool,
                                                         "Sim", "1")

Atab2017MVisitados$Municipio_infestado_bool <- str_replace(Atab2017MVisitados$Municipio_infestado_bool,
                                                         "não", "0")

Atab2016MVisitados$Municipio_infestado_bool <- as.numeric(Atab2016MVisitados$Municipio_infestado_bool)
Atab2017MVisitados$Municipio_infestado_bool <- as.numeric(Atab2017MVisitados$Municipio_infestado_bool)

indices <- is.na(Atab2016MVisitados$Municipio_infestado_bool)
indices <- !indices
Atab2016MVisitados <- Atab2016MVisitados[indices,]
Atab2016MVisitados$Municipio_infestado_bool <- as.numeric(Atab2016MVisitados$Municipio_infestado_bool)

Arepresentatividade2016 <- sum(Atab2016MVisitados$Municipio_infestado_bool)/
  length(Atab2016MVisitados$Municipio_infestado_bool) * 100

indices <- is.na(Atab2017MVisitados$Municipio_infestado_bool)
indices <- !indices
Atab2017MVisitados <- Atab2017MVisitados[indices,]
Atab2017MVisitados$Municipio_infestado_bool <- as.numeric(Atab2017MVisitados$Municipio_infestado_bool)

Arepresentatividade2017 <- sum(Atab2017MVisitados$Municipio_infestado_bool)/
  length(Atab2017MVisitados$Municipio_infestado_bool) *100

#*************************************************************************************************#
#****************Anota??es acerca do codigo e sua an?lise, para posterior utiliza??o***************#

#S? existem dados de IB referentes aos anos a partir de 2013. Nesse sentido os anos de 2013 a 2015
#possuem munic?pos com IIP's maiores do que seus respectivos IB's. 2016 e 2017 não possuem tal erro
#nos dados.

#****************************************************************************************************#