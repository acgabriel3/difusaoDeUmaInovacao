library(igraph)

#***
#Separacao das redes por ano de adocao, o objetivo seria gerar analises de complexidae em cada rede.

#Coloca os municipios da tabela toa na mesma ordem em que estao na sequencia de vertices do grafico 
match_reordenamento <- match(as_ids(V(graph_distancia_totais_estradas_corte_limite_maximo)), 
                             frame_todos_municipios$IBGE)
liraaMunicipios_2009_a_2017_ordenado_por_ano_adocao_liraa <- frame_todos_municipios[match_reordenamento,]


#está conseguino plotar os graficos resultantes 
graph_distancia_totais_estradas_2009 <- induced_subgraph(graph_distancia_totais_estradas_corte_limite_maximo, 
                                                         which(liraaMunicipios_2009_a_2017_ordenado_por_ano_adocao_liraa$toa <= 2009))
graph_distancia_totais_estradas_2010 <- induced_subgraph(graph_distancia_totais_estradas_corte_limite_maximo, 
                                                         which(liraaMunicipios_2009_a_2017_ordenado_por_ano_adocao_liraa$toa <= 2010))
graph_distancia_totais_estradas_2011 <- induced_subgraph(graph_distancia_totais_estradas_corte_limite_maximo, 
                                                         which(liraaMunicipios_2009_a_2017_ordenado_por_ano_adocao_liraa$toa <= 2011))
graph_distancia_totais_estradas_2012 <- induced_subgraph(graph_distancia_totais_estradas_corte_limite_maximo, 
                                                         which(liraaMunicipios_2009_a_2017_ordenado_por_ano_adocao_liraa$toa <= 2012))
graph_distancia_totais_estradas_2013 <- induced_subgraph(graph_distancia_totais_estradas_corte_limite_maximo, 
                                                         which(liraaMunicipios_2009_a_2017_ordenado_por_ano_adocao_liraa$toa <= 2013))
graph_distancia_totais_estradas_2014 <- induced_subgraph(graph_distancia_totais_estradas_corte_limite_maximo, 
                                                         which(liraaMunicipios_2009_a_2017_ordenado_por_ano_adocao_liraa$toa <= 2014))
graph_distancia_totais_estradas_2015 <- induced_subgraph(graph_distancia_totais_estradas_corte_limite_maximo, 
                                                         which(liraaMunicipios_2009_a_2017_ordenado_por_ano_adocao_liraa$toa <= 2015))
graph_distancia_totais_estradas_2016 <- induced_subgraph(graph_distancia_totais_estradas_corte_limite_maximo, 
                                                         which(liraaMunicipios_2009_a_2017_ordenado_por_ano_adocao_liraa$toa <= 2016))
graph_distancia_totais_estradas_2017 <- induced_subgraph(graph_distancia_totais_estradas_corte_limite_maximo, 
                                                         which(liraaMunicipios_2009_a_2017_ordenado_por_ano_adocao_liraa$toa <= 2017))
