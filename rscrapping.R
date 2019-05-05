library(httr)
library(xml2)
library(rvest)

# Pegando as referências
dois_um_url <- "https://www.doisporum.net"
dois_um <- httr::GET(dois_um_url) %>%
  httr::content('text') %>%
  xml2::read_html() %>%
  html_nodes(".various") %>%
  html_attr("href")

# Pegando as informações de cada oferta
enderecos <- data.frame("Nome" = numeric(0), "Endereço" = numeric(0), stringsAsFactors = F)
for (url in dois_um) {
  page <- httr::GET(paste0(dois_um_url, url)) %>%
    httr::content('text') %>%
    xml2::read_html()

  nome <- page %>%
    html_nodes("h1") %>%
    html_text()

  end <- page %>%
    html_nodes(".refdesc p") %>%
    html_text()

  enderecos <- rbind(enderecos, data.frame("Nome" = nome,
                                           "Endereço" = end[length(end)],
                                           "Endereço 2" = end[length(end) - 1],
                     stringsAsFactors = F))
}

write.csv(enderecos, file = "enderecos.csv", row.names = F)
