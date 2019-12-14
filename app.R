#***
#Gerar visualizacao no mapa acerca da localizacao dos municipios

library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)
library(readxl)
library(dplyr)
library(igraph)


source("NS.R")
source("estrutura.R")

#***
#ENTRADA TEMPORARIA DE DADOS:
#source("arquivo/geracaoRede.R")

ui <- fluidPage(
  useShinyjs(),
  
  navbarPage("DELTB",
           
           tabPanel("Pagina Inicial",
                    pag_Inicial_Ui("pagInicial")
           )
           
  )
)

server <- function(input, output, session) {
  
  callModule(pag_inicial_server, "pagInicial")

  }

shinyApp(ui, server)
