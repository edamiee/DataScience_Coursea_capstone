### Damien Edwards
### Capstone Task 1

setwd("/Users/dame81/Desktop/DS_classes/capstone/data/final/en_US")
## Read data

Usblogs <- readLines('/Users/dame81/Desktop/DS_classes/capstone/data/final/en_US/en_US.blogs.txt', encoding = 'UTF-8')

### Token FUncitons
Token <- function(text){
  text <- readLines(text, encoding = 'UTF-8')
  text <- unlist(sapply(text,strsplit,'[[:space:]]+'))
  return(text)
}
TokenWords <- function(text){
  text <- readLines(text, encoding = 'UTF-8')
  text <- stringr::str_extract_all(text,'[[:alpha:]]+')
  text <- unlist(sapply(text,strsplit,'[[:space:]]+'))
  return(text)
}

TokenPunct <- function(text){
  text <- readLines(text, encoding = 'UTF-8')
  text <- stringr::str_extract_all(text,'[[:punct:]]+')
  text <- unlist(sapply(text,strsplit,'[[:space:]]+'))
  return(text)
}

TokenNum <- function(text){
  text <- readLines(text, encoding = 'UTF-8')
  text <- stringr::str_extract_all(text,'[[:digit:]]+')
  text <- unlist(sapply(text,strsplit,'[[:space:]]+'))
  return(text)
}



### Filter Bad words

url <- 'https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en'
badwords <- download.file(url,'/Users/dame81/Desktop/DS_classes/capstone/data/badword.txt', method = 'curl')
badwords <- readLines('/Users/dame81/Desktop/DS_classes/capstone/data/badword.txt', encoding = 'UTF-8')
filterdic <- function(text,dic){
  result <- text[!text %in% dic]
  return(result)
}
filterdic(text,badwords)
