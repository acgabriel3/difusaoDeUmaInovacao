#********************************************************************************************#

#-Geração de redes aleatórias, possuindo o mesmo nome dos vértices da rede original, o mesmo
#tamanho, e o mesmo vetor de adoção. 

#-Objetivo: Comparar os índices estatísticos em redes aleatórias geradas por um mesmo vetor de
#adoção, com os índices gerados por redes não aleatórias.

#-Fim: Gerar provas de que a hipótese de que a difusão de uma inovação é influenciada pela
#distância. 

#********************************************************************************************#

#problema:
#Acrescentar vetor aleatório de distâncias deve ser aferitivamente mais acurado. No entanto,
#como construir tal vetor?

#********************************************************************************************#

library(netdiffuseR)
library(dplyr)

#***
#Tentativa de simular redes pela propria biblioeca do netdiffuse

redeDiffnet_aleatoria <- rdiffnet(5180, 9, seed.p.adopt = .02596876,
                      seed.nodes = "random",
                      seed.graph = "small-world",
                      rgraph.args = list(undirected=TRUE, k=4, p=.5),
                      threshold.dist = function(x) 0.3)

aleatoria <- redeDiffnet_aleatoria

redeDiffnet_aleatoria$vertex.static.attrs <- redeDiffnet_fronteiras$vertex.static.attrs
redeDiffnet_aleatoria$toa <- redeDiffnet_fronteiras$toa
redeDiffnet_aleatoria$adopt <- redeDiffnet_fronteiras$adopt
redeDiffnet_aleatoria$cumadopt <- redeDiffnet_fronteiras$cumadopt
redeDiffnet_aleatoria$vertex.dyn.attrs <- redeDiffnet_fronteiras$vertex.dyn.attrs
redeDiffnet_aleatoria$graph.attrs <- redeDiffnet_fronteiras$graph.attrs
redeDiffnet_aleatoria$meta <- redeDiffnet_fronteiras$meta


summary(aleatoria)
summary(redeDiffnet_aleatoria) #comparar com summary original

