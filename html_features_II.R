require(XML)
require(tm)

# Input der Funktion ist ein Pfad zu einer .txt-Datei mit html-Inhalt
# Beispiel: filepath = "data/0/1000188_raw_html.txt"
get_html_information_II <- function(filepath){
  
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
    
    
    values["frequent_words"] <- NA
    values["identical_words"] <- NA
    
    return(values)
  } 
  
  
  #       <body> - Transformation
  # Wandle kompletten Inhalt des <body> in R Objekt um
  html_body <- getNodeSet(html_raw, "/html/body/node()")
  html_body  <- sapply(html_body, xmlValue) #in R Objekt
   
  
  # tm
  docs <- html_body
  # Umwandlung des body für {tm}-Transformationen
  docs <- VCorpus(VectorSource(docs))
  docs <- tm_map(docs, content_transformer(tolower))
  # Transformation: englische stopwords entfernen
  stopwrds <- c(stopwords("english"),stopwords("SMART"),"the","and")
  docs <- tm_map(docs, removeWords, stopwrds)
  # Transformation: Interpunktion entfernen
  docs <- tm_map(docs, removePunctuation)
  # Transformation: Nummern entfernen
  docs <- tm_map(docs, removeNumbers)
  # Transformation: Entferne unnötige Leerzeichen
  docs <- tm_map(docs, stripWhitespace)
  #       </body> - Transformation
  
  #       <meta> 
  # Nimm Inhalt des htlm-Tag <meta> mit name='description'
  html_meta <- xpathApply(html_raw, "//meta[@name='description']", xmlAttrs)
  if(length(html_meta) != 0) {
    html_meta <- strsplit(unlist(html_meta), " ")
  }
  else {
    html_meta<- NA
  }
  # description und content entfernen
  html_meta <- html_meta[! html_meta %in% c("content", "description")]
  #       <meta> - Transformation
  
  
  #             <title>
  # Nimm html-Tag <title> 
  html_title <- xpathApply(html_raw, "//title", xmlValue)
  if(length(html_title) != 0) {
    # liste in char vector umwandeln und unerwünschte Zeichen entfernen
    html_title <- gsub("\\n", "", html_title)
    html_title <- strsplit(html_title, split = " ")
    html_title <- html_title[[1]]
    html_title <- grep("\\w", html_title, value=TRUE)
  }
  #             <title>
  
  # Kombination von <title> und <meta>  
  title_meta <- c(html_title, html_meta)
  title_meta <- unname(title_meta)
  # Transformation: Kleinbuchstaben
  title_meta <- tolower(title_meta)
  
  
  # DocumentTermMatrix aufziehen 
  dtm <- DocumentTermMatrix(docs)
  # Frequente Woerter in <body> finden mit 
  # lowfreq = untere Grenze
  # highfreq = obere Grenze
  frequ_words <- findFreqTerms(dtm, lowfreq = 5, highfreq = 80)
  # Transformation: Kleinbuchstaben
  frequ_words <- tolower(frequ_words)
  count_frequ_words <- length(frequ_words) 
  # schreibe frequente Woerter in Liste 'frequent_words'
  values["frequent_words"] <- count_frequ_words
  
  # Suche identische Woerter in <meta>+<head> und <body>
  ident_words <- title_meta[title_meta %in% frequ_words] 
  # Anzahl identischer Woerter
  count_ident_words <- length(ident_words) 
  values["identical_words"] <- count_ident_words
  
  # Entferne nicht benötigte Objekte, wichtig für Iteration über alle ~ 300.000 Webseiten
  rm(filename, filepath, docs, dtm, count_frequ_words, count_ident_words, 
     frequ_words, stopwrds, html_raw, root, html_meta,html_title, html_body, ident_words, title_meta)
  gc()
  
  return(values)
}











