library(stringr)
library(httr)
library(leaflet)
library(stringi)
library(htmlwidgets)

enderecos <- read.csv("/Users/Gabi/Documents/Github/Rscrapping/dois-por-um/enderecos.csv", stringsAsFactors = F)

# Remove telefone do endereco
enderecos$Endereço[grep("tel|Tel|(11)|R.", enderecos$`Endereço`, invert = T)] <- enderecos$`Endereço.2`[grep("tel|Tel|(11)|R.", enderecos$`Endereço`, invert = T)]
enderecos$Endereço.2 <- NULL

ende <- enderecos$Endereço %>%
  str_remove_all("(Tel:|Tel\\.:|Telefone:)|[(11)]\\S+\\s+\\d{4}(\\s+|\\S+)\\d{4}") %>%
  str_remove_all("w{3}\\.(.*)") %>%
  str_remove_all("\\.com") %>%
  str_remove_all("\\.net") %>%
  str_remove("\\.br") %>%
  str_remove_all("(?<=(São Paulo - SP))(.*)|(?<=(São Paulo SP))(.*)")

# Separa os endereços de um mesmo estabelecimento
ende[67] <- sub("(56)", "\\1/", ende[67])
ende[67] <- sub("(387)", "\\1/", ende[67])
x <- strsplit(ende, "/")
y <- sapply(x, length)
enderecos <- data.frame("Nome" = rep(enderecos$Nome, y), "Endereço" = unlist(x),
                        stringsAsFactors = F)

# Remove as ocorrências sem sentido
enderecos <- enderecos[-c(44, 127, 155, 156), ]

# Adequa a string de endereço para buscar pela API
enderecos$Endereço[grepl(":", enderecos$Endereço)] <- paste(str_remove(enderecos$Endereço[grepl(":", enderecos$Endereço)], ".*:"),
                                                             str_extract(enderecos$Endereço[grepl(":", enderecos$Endereço)], ".*:"))
enderecos$Endereço[!grepl("SP", enderecos$Endereço)] <- enderecos$Endereço[!grepl("SP", enderecos$Endereço)] %>%
  str_remove("-") %>%
  str_c(" Sao Paulo SP")

enderecos$Endereço <- enderecos$Endereço %>%
  str_remove_all(":") %>%
  str_remove_all("-") %>%
  str_remove_all(",") %>%
  str_replace("R\\. ", "Rua ") %>%
  str_replace("Av\\. ", "Avenida ") %>%
  str_replace("Dr\\. ", "Doutor ") %>%
  str_remove_all("\\.") %>%
  str_extract("(Rua).*|(Alameda).*|(Praca).*|(Largo).*|(Avenida).*") %>%
  str_remove("\\d+º andar") %>%
  str_remove("Piso \\d+") %>%
  str_remove("Parque do Ibirapuera ") %>%
  str_replace("Brooklin Novo", "Cidade Moncoes") %>%
  str_replace("Cidade Jardim", "Morumbi") %>%
  str_replace("Centro", "República") %>%
  str_replace("VN", "Vila Nova") %>%
  str_replace("Min", "Ministro") %>%
  str_remove("\\d{8}") %>%
  str_squish() 

enderecos <- enderecos[!is.na(enderecos$Endereço), ]
enderecos$Endereço[127] <- str_remove(enderecos$Endereço[127], " \\d{1} .*USP")
enderecos$Endereço[52] <- "Instituto Tomie Ohtake rua corope 88"

enderecos$url <- unname(sapply(enderecos$Endereço, URLencode))
enderecos$url <- gsub("%20", "+", enderecos$url)

# Busca a lat e long pela API
url <- paste0('http://www.mapquestapi.com/geocoding/v1/address?key=',
              key, '&location=', enderecos$url, "+brasil&maxResults=1")

latlong <- lapply(url, function(x) content(GET(x))$results[[1]]$locations[[1]]$latLng)
latlong <- data.frame(matrix(unlist(latlong), nrow=length(latlong), byrow=T))
names(latlong) <- c("Lat", "Long")
enderecos <- cbind(enderecos, latlong)

# Cria mapa com os lugares
map <- leaflet(enderecos) %>% addTiles() %>%
  addControl(paste("Dois por Um <br> Quantidade de Ofertas:", nrow(enderecos)), position = "topright") %>%
  addAwesomeMarkers(~Long, ~Lat, 
                    popup=~paste0("Loja: ", Nome, "<br>",
                                  "Endereço: ", `Endereço`))
saveWidget(map, "mapa.html")
