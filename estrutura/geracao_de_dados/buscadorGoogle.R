library(jsonlite)
library(urltools)

requestDistance <- function(origin, destination){

url <- "https://maps.googleapis.com/maps/api/distancematrix/json?language=pt-PT&origins=Grupiara&destinations=Belo+Horizonte&key=AIzaSyCHEqgb5HUKoammlv_jORAwQzbWeFuYbkw"

origin <- gsub(" ", "+", origin)

destination <- gsub(" ", "+", destination)

key <- "AIzaSyCHEqgb5HUKoammlv_jORAwQzbWeFuYbkw"

language <- "pt-PT"

url <- param_set(url, key = "language", value = language)
url <- param_set(url, key = "origins", value = origin)
url <- param_set(url, key = "destinations", value = destination)
url <- param_set(url, key = "key", value = key)

request <- fromJSON(url)

if(request$rows == "ZERO_RESULTS"){
  results <- "Não Encontrado"  
} else {
  result <- request$rows
}


return(result)

}

# Teste <- fromJSON("https://maps.googleapis.com/maps/api/distancematrix/json?language=pt-PT&origins=Grupiara&destinations=Belo+Horizonte&key=AIzaSyCHEqgb5HUKoammlv_jORAwQzbWeFuYbkw")