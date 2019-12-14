#CAMINHO
#NS/bilbioteca/gera_mapa_linhas_rodovias.R

gera_mapa_linhas_rodovias <- function() {
  
  mapa <- 
    #cidades %>%
    leaflet() %>%
    addTiles()
  #%>%
  #addPopups(lng = cidades$lon, lat = cidades$lat, popup = popupOptions())%>%
    #addMarkers()
  
  return(mapa)
}
