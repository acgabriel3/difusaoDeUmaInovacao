#***
#CAMINHO
#NS/pagInicial.R

#INPUTS

#OUTPUTS



#***
#lat e lon gerada por meio de match com o dado original do ibge para as malhas rodoviarias
#Esse dado deveria ser idealmente gerado em estruturas (ou/e registrado em dados)

#mapa <- cidades %>%
#  leaflet() %>%
#  addTiles() %>%
#addPopups(lng = cidades$lon, lat = cidades$lat, popup = popupOptions())%>%
#  addMarkers()

#***
#Tentativa de adicionar linhas
#for(i in 1:nrow(distanciasMU)){
#  mapa <- addPolylines(mapa, lat = as.numeric(distanciasMU[i, c(17, 18)]), 
#                       lng = as.numeric(distanciasMU[i, c(19, 20)]))
#}

#***
#construir um slider com as ligacoes voltadas para um tamanho crescente de distancia entre as mesmas
#Em cada uma das etapas deste slider, tambem exibir caracteristicas da rede
#Mostrar: Componentes, grau, diametro e indice de clusterizacao


pag_Inicial_Ui <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    
    box(width = 4),
    
    box(width = 4,
        solidHeader = TRUE,
            
            sliderInput(inputId = ns("slider_distancia_minutos"),
                        label = "Escolha o intervalo de distância em minutos:",
                        max = 4200,
                        min = 0,
                        step = 30,
                        value = c(4000, 4200)
                        )
        
        ),
    
    box(width = 4,
        solidHeader = TRUE,
         actionButton(inputId = "teste", "construir visao")
        ),
    
    box(width = 12,
        solidHeader = TRUE,
        
         leafletOutput(ns("mapaLinhas"))
        
         )
  )
  
}

pag_inicial_server <- function(input, output, session) {
  
  dadosRodovias <- puxa_dados_rodovias()
  
  mapa <- gera_mapa_linhas_rodovias()
  
  dadosRodovias_filtrado_intervalo <- reactive({
    
    filtra_por_intervalo(tabela = dadosRodovias,
                         min = input$slider_distancia_minutos[1], 
                         max = input$slider_distancia_minutos[2],
                         colunaFiltro = "DisMinutos")
    
    
  })
  
  output$mapaLinhas <- renderLeaflet({
    
    #print(input$teste)
    renderizaLinhas(mapa, dadosRodovias_filtrado_intervalo())
    
  })
  
}