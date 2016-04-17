require(XML)

# Input der Funktion ist ein Pfad zu einer .txt-Datei mit html-Inhalt
# Beispiel: filepath = "data/0/1000188_raw_html.txt"
get_html_information <- function(filepath){
  
  # Output der Funktion ist eine verschachtelte Liste
  values <- list()
  # Dateiname des Pfads
  filename <- basename(filepath)
  # schreibe Dateiname in Liste "file"
  values["file"] <- filename
  # forme Inhalt der .txt-Datei in "Baumstruktur" um 
  html_raw <- htmlTreeParse(filepath ,useInternalNodes = TRUE)
  # root-Element des Baumes
  root = xmlRoot(html_raw)
  
  # Wenn root-Element nicht existiert, fülle verschachtelte Liste mit NA
  if(is.null(root)) {
    
    values["size_head"] <- NA
    values["size_body"] <- NA
    values["cnt_meta_words"] <- NA
    values["cnt_title_words"] <- NA

    return(values)
  } 
  
  
  #       <head> - Informationen 
  # Nimm alle html-Tags zwischen <head> und </head
  html_head <- getNodeSet(html_raw, "//head/node()")
  #in R Objekt umwandeln
  html_head  <- sapply(html_head, xmlValue) 
  # zähle Zeichen und schreibe Ergebnis in Liste "size_head"
  values["size_head"] <- sum(nchar(html_head)) 
  rm(html_head)
  #       </head> - Informationen
  
  
  
  #       <body>
  # Nimm alle html-Tags zwischen <body> und </body>
  html_body <- getNodeSet(html_raw, "/html/body/node()")
  #in R Objekt umwandeln
  html_body  <- sapply(html_body, xmlValue) 
  # zähle Zeichen und schreibe Ergebnis in Liste "size_body"
  values["size_body"] <- sum(nchar(html_body)) 
  rm(html_body)
  #       </body>
  
  
  
  #       <meta> - Informationen
  # Nimm Attributinhalte des html-Tag <meta> mit dem Attribut name='description' 
  html_meta <- xpathApply(html_raw, "//meta[@name='description']", xmlAttrs)
  if(length(html_meta) != 0) {
   
    html_meta <- strsplit(unlist(html_meta), " ")
    
  }
  else {
    html_meta<- NA
  }
  # description und content aus Wortmenge entfernen
  html_meta <- html_meta[! html_meta %in% c("content", "description")]
  # Anzahl Wörter der Beschreibung
  values["cnt_meta_words"] <- length(unlist(html_meta)) 
  #       </meta> - Informationen
  
  
  
  #       <title> - Informationen
  # Nimm html-Tag <title> 
  html_title <- xpathApply(html_raw, "//title", xmlValue)
  if(length(html_title) != 0) {
    # liste in char vector umwandeln und unerwünschte Zeichen entfernen
    html_title <- gsub("\\n", "", html_title)
    html_title <- strsplit(html_title, split = " ")
    html_title <- html_title[[1]]
    html_title <- grep("\\w", html_title, value=TRUE)
    # Anzahl Wörter des Title
    values["cnt_title_words"] <- length(html_title) 
  }
  else {
    values["cnt_title_words"] <- NA
  }
  #       </title> - Informationen
  
  # Entferne nicht benötigte Objekte, wichtig für Iteration über alle ~ 300.000 Webseiten
  rm(filename, filepath, html_raw, root, html_meta,html_title)
  gc()
  return(values)
}


