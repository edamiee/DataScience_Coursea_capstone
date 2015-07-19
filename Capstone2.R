### Damien Edwards
### Capstone Task 2

setwd("/Users/dame81/Desktop/DS_classes/capstone/data/final/en_US")
install.packages("tm")
library(tm)
library(stringi)
## Read data

Usblogs <- readLines('/Users/dame81/Desktop/DS_classes/capstone/data/final/en_US/en_US.blogs.txt', encoding = 'UTF-8')

lowestr<- stri_trans_tolower(Usblogs)
lowestr<- stri_enc_toascii(lowestr)
lowestr <- stri_replace_all_regex(lowestr,'\032','')

index <- sample(1:length(lowestr),100000)
substrngb <- lowestr[index]

substrngb.vec <- VectorSource(lowestr)
substrngb<- Corpus(substrngb.vec)

## TOKENIZATION

Usenblogs<- tm_map(substrngb, removePunctuation)
Usenblogs <- tm_map(Usenblogs, removeNumbers)

Usenblogs <- tm_map(Usenblogs, removeWords, stopwords("english"))
Usenblogs <- tm_map(Usenblogs, stemDocument,language = ("english"))
Usenblogs <- tm_map(Usenblogs, stripWhitespace)
save(Usenblogs,file='/Users/dame81/Desktop/DS_classes/capstone/data/USenblog.RData')

## Explore Data
USenblogs.tdm <-TermDocumentMatrix(Usenblogs)

save(USenblogs.tdm ,file='Users/dame81/Desktop/DS_classes/capstone/data/USenblogs.tdm ')

options(mc.cores=1)
TrigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 4))}
tridm <- TermDocumentMatrix(Usenblogs, control = list(tokenize = TrigramTokenizer))
save(tridm,file='Users/dame81/Desktop/DS_classes/capstone/data/USbtridm.RData')
tridm9 <- removeSparseTerms(tridm, 0.9)
tridm99 <- removeSparseTerms(tridm, 0.99)
tridm999 <- removeSparseTerms(tridm, 0.999)
tridm9999 <- removeSparseTerms(tridm, 0.9999)
tridm99999 <- removeSparseTerms(tridm, 0.99999)



