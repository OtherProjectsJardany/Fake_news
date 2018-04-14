library(rvest)
library(XML)
library(RCurl)

## https://stackoverflow.com/questions/32889136/how-to-get-google-search-results

#****************************************************************************
## 1. Obtener enlaces F290 ####
#****************************************************************************
# Definimos función para buscar en google
g_search <- function(search_word, quotes=TRUE, domain=""){
      search_word <- gsub('  ', ' ', search_word)
      search_word <- gsub(' ', '%20', search_word)
      if(quotes){
            search_word <- paste0('%22', search_word, '%22') 
      }
            
      getGoogleURL <- paste0('http://www.google.com', domain, '/search?q=',
                            search_word)
      con <- url(getGoogleURL, "rb") 
      return(con)
}

con <- g_search("Nestor jardany serrano")

# Reading the HTML code from the website
webpage <- read_html(con)
summary(webpage)
close(con)

html <- htmlTreeParse(webpage, useInternalNodes = TRUE, error=function(...){})
nodes <- getNodeSet(html, "//h3[@class='r']//a")
return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))




# Obtenemos los enlaces
html_nodes(webpage, "cite")

nodos <- html_nodes(webpage,'a')
links <- html_attr(nodos, "href"); head(links)


# Obtenemos los nombres de los enlaces
reportes <- html_attr(nodos, "title")
reportes

# Obtenemos los textos de enlace
text_link<-html_text(nodos)
text_link

#******************************************
# Limpiamos (dejamos sÃ³lo los necesarios)
#******************************************
links_super<-data.frame(Mes=text_link, titulo=reportes,link=links)
links_super$link <- as.character(links_super$link)

for(i in 1:length(links_super$link)){
      links_super$link[i] <- str_replace(links_super$link[i], "https://www.superfinanciera.gov.co", "")
}

quedan <- "((^|,)([Ee]nero|[Ff]ebrero|[Mm]arzo|[Aa]bril|[Mm]ayo|[Jj]unio|[Jj]ulio|[Aa]gosto|[Ss]eptiembre|[Oo]ctubre|[Nn]oviembre|[Dd]iciembre))"
links_super<-links_super[grepl(quedan, links_super$Mes),]
head(links_super)


#******************************************************************
## 2. Descarga archivos ####
#******************************************************************

server_url<-"https://www.superfinanciera.gov.co"
download.290(server = server_url, df.links=links_super, folder="./data")




















