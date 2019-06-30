library(RSelenium)
library(rvest)
library(xml2)
library(tidyverse)
library(RecordLinkage)
library(stringr)

getAdress <- function(endereco){
  # Reads the restaurants adress
  enderecos <- read.csv("enderecos.csv")
  
  remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                   port = 4445L,
                                   browserName = "chrome")
  remDr$open()
  
  remDr$navigate("https://www.sodexobeneficios.com.br/sodexo-club/rede-credenciada/") #Entering our URL gets the browser to navigate to the page
  remDr$setTimeout(type = "implicit", milliseconds = 10000)
  productType <- remDr$findElement(using = "xpath", '//*[@id="content"]/section/section/aside/div/div[2]/p[1]/a[2]')
  productType$clickElement()
  endForm <- remDr$findElement(using = 'xpath', '//*[@id="endereco"]')
  endForm$sendKeysToElement(list(endereco))
  Sys.sleep(2)
  endForm$sendKeysToElement(list(key = 'down_arrow', key = 'enter'))
  
  buscarButton <- remDr$findElement(using = 'xpath', '//*[@id="content"]/section/section/aside/div/div[2]/div[2]/button')
  
  Sys.sleep(3)
  buscarButton$clickElement()
  
  end <- remDr$findElements(using = "class name", "adress-estab")
  end <- sapply(end, function(x) x$getElementText())
  remDr$screenshot(display = TRUE)
  
  remDr$close()
  
  return(end)
}

enderecos <- read.csv("endScrapping.csv", stringsAsFactors = F)
sodexo <- c()
for (i in 1:nrow(enderecos)){
  end <- getAdress(enderecos[i, 2])
  end <- end %>%
    str_split("\n")

  for (estab in end){
    estabelecimento <- estab[1]  
    endereco <- estab[2]
    distancia <- estab[length(estab)]
    
    if (distancia == "(0.00 km)"){
      sodexo <- c(sodexo, estabelecimento)
    }
  }
}
 
write(sodexo, "sodexo.txt")
