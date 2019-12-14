#Planejamento da modularizacao do projeto para a analise de malhas rodoviarias relacionadas ao LIRAa

#------ Estrutura de interfaces:

#(DELTB) : difusao e estrutura do LIRAa no territorio brasileiro

    #(estrutura) : Scripts relacionados a limpeza de dados
        #sub(importacao e limpeza de dados) : Scripts relacionados a importacao e limpeza de dados
            #-nomes_colunas e seus dados. (eles surgem em variaveis globais, estas são o link entre os diversos
            #programas. O ideal seria disponibilizar funcoes para evitaro uso de variaveis globais) 
        #sub(geracao de dados) : Scripts relacionados a geracao de dados
            #-buscadorGoogle: Funcao base referente a geracao de dados 
        #sub(estruturas de dados) : Scripts relacionados a construcao dos dados para analise
            #-grafoDistancias : Monta a rede por meio da biblioteca igraph e os graficos (eh possivel criar uma funcao
            # para isso)

    #(arquivo) : Scripts fora de uso por algum motivo, servindo apenas como base
        #-rede_segundo_ano

    #(analises) : Scripts relacionados a analise dos dados
        #sub(simulacoes)
            #-exerciciosDifusao
            #-redesAleatorias_tempFixo *provavelmente sera arquivado
        #sub(visualizacoes)
            #-plotagemCurvas


