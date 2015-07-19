## Damien Edwards
## Capstone Project
install.packages("tm")
install.packages("slam")
install.packages("devtools")

library(tm)
library(stringi)
library(slam)
library(devtools)
install_github("ropensci/plotly")
library(plotly)
# Create data folder

if (!file.exists("data")) {
  dir.create("data")
}


#Download file

fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(fileUrl, destfile = "./data/Swift.zip", method = "curl")
list.files("./data")
dateDownloaded <- date()

unzip('data//Swift.zip',exdir='data/')
list.files('data/final/')
list.files('data/final//en_US')

#Import data

# readLines('data/final/en_US/en_US.twitter.txt',5)
# readLines('data/final/en_US/en_US.news.txt',5)
# readLines('data/final/en_US/en_US.blogs.txt',5)

twitter <- readLines('/Users/dame81/Desktop/DS_classes/capstone/data/final/en_US/en_US.twitter.txt', encoding = 'UTF-8')
USnews<- readLines('/Users/dame81/Desktop/DS_classes/capstone/data/final/en_US/en_US.news.txt', encoding = 'UTF-8')
Usblogs <- readLines('/Users/dame81/Desktop/DS_classes/capstone/data/final/en_US/en_US.blogs.txt', encoding = 'UTF-8')

##Bad Words

### Filter Bad words

url <- 'https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en'
badwords <- download.file(url,'/Users/dame81/Desktop/DS_classes/capstone/data/badword.txt', method = 'curl')
badwords <- readLines('/Users/dame81/Desktop/DS_classes/capstone/data/badword.txt', encoding = 'UTF-8')

# Create Sample Sets of the data
set.seed(2143)
SampleBlogs <- sample(Usblogs, size=round(length(Usblogs)*.01));
SampleNews <- sample(USnews, size=round(length(USnews)*.01)); 
SampleTwitter <- sample(twitter, size=round(length(twitter)*.01))

##Transform data into Class Corpus

blogsC<- Corpus(VectorSource(SampleBlogs), readerControl=list(reader=readPlain, language="en_US", load=TRUE))
newsC<- Corpus(VectorSource(SampleNews), readerControl=list(reader=readPlain, language="en_US", load=TRUE))
twitterC <- Corpus(VectorSource(SampleTwitter), readerControl=list(reader=readPlain, language="en_US", load=TRUE))

##Clean data

blogsC <- tm_map(blogsC, content_transformer(tolower))
newsC <- tm_map(newsC, content_transformer(tolower))
twitterC<- tm_map(twitterC, content_transformer(tolower))

blogsC <- tm_map(blogsC, stripWhitespace)
newsCorpus <- tm_map(newsC, stripWhitespace)
twitterCorpus <- tm_map(twitterC, stripWhitespace)

##Bad words
blogsC <- tm_map(blogsC, removeNumbers); newsC <- tm_map(newsC, removeNumbers);

blogsC <- tm_map(blogsC, removeWords, badwords)
newsC <- tm_map(newsC, removeWords, badwords)
twitterC<- tm_map(twitterC, removeWords, badwords)
### ngram_tokenizer function
ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE, skip_word_number = FALSE) {
  stopifnot(is.numeric(n), is.finite(n), n > 0)
  
  #' To avoid :: calls
  stri_split_boundaries <- stringi::stri_split_boundaries
  stri_join <- stringi::stri_join
  
  options <- stringi::stri_opts_brkiter(
    type="word", skip_word_none = skip_word_none, skip_word_number = skip_word_number
  )
  
  #' Tokenizer
  #' 
  #' @param x character
  #' @return character vector with n-grams
  function(x) {
    stopifnot(is.character(x))
    
    # Split into word tokens
    tokens <- unlist(stri_split_boundaries(x, opts_brkiter=options))
    len <- length(tokens)
    
    if(all(is.na(tokens)) || len < n) {
      # If we didn't detect any words or number of tokens is less than n return empty vector
      character(0)
    } else {
      sapply(
        1:max(1, len - n + 1),
        function(i) stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
      )
    }
  }
}
##TOKENIZE
unigram <- content_transformer(ngram_tokenizer(1))
bigram <- content_transformer(ngram_tokenizer(2))
trigram <- content_transformer(ngram_tokenizer(3))
quadrigram <- content_transformer(ngram_tokenizer(4))

blogsUni <- TermDocumentMatrix(blogsC, control=list(tokenize=unigram, wordLengths=c(1, Inf)))
blogsBi <- TermDocumentMatrix(blogsC, control=list(tokenize=bigram, wordLengths=c(1, Inf)))
blogsTri <- TermDocumentMatrix(blogsC, control=list(tokenize=trigram, wordLengths=c(1, Inf)))
blogsQuadri <- TermDocumentMatrix(blogsC, control=list(tokenize=quadrigram, wordLengths=c(1, Inf)))

newsUni<- TermDocumentMatrix(newsC, control=list(tokenize=unigram, wordLengths=c(1, Inf)))
newsBi <- TermDocumentMatrix(newsC, control=list(tokenize=bigram, wordLengths=c(1, Inf)))
newsTri<- TermDocumentMatrix(newsC, control=list(tokenize=trigram, wordLengths=c(1, Inf)))
newsQuadri <- TermDocumentMatrix(newsC, control=list(tokenize=quadrigram, wordLengths=c(1, Inf)))

twitterUni<- TermDocumentMatrix(twitterC, control=list(tokenize=unigram, wordLengths=c(1, Inf)))
twitterBi <- TermDocumentMatrix(twitterC, control=list(tokenize=bigram, wordLengths=c(1, Inf)))
twitterTri <- TermDocumentMatrix(twitterC, control=list(tokenize=trigram, wordLengths=c(1, Inf)))
twitterQuadri <- TermDocumentMatrix(twitterC, control=list(tokenize=quadrigram, wordLengths=c(1, Inf)))

## Analysis of Frequent Terms

blogsUniM <- as.matrix(rollup(blogsUni, 2, FUN=sum));
blogsBiM<- as.matrix(rollup(blogsBi, 2, FUN=sum));
blogsTriM <- as.matrix(rollup(blogsTri, 2, FUN=sum));
blogsQuadriM<- as.matrix(rollup(blogsQuadri, 2, FUN=sum));

topblogsUniM <- sort(rowSums(blogsUniM), decreasing=TRUE)[1:10]
topblogsBiM<<- sort(rowSums(blogsBiM), decreasing=TRUE)[1:10]
topblogsTriM<- sort(rowSums(blogsTriM), decreasing=TRUE)[1:10]
topQuadriM<- sort(rowSums(blogsQuadriM), decreasing=TRUE)[1:10]

newsUniM <- as.matrix(rollup(newsUni, 2, FUN=sum));
newsBiM<- as.matrix(rollup(newsBi, 2, FUN=sum));
newsTriM <- as.matrix(rollup(newsTri, 2, FUN=sum));
newsQuadriM<- as.matrix(rollup(newsQuadri, 2, FUN=sum));

topnewsUniM <- sort(rowSums(newsUniM), decreasing=TRUE)[1:10]
topnewsBiM<<- sort(rowSums(newsBiM), decreasing=TRUE)[1:10]
topnewsTriM<- sort(rowSums(newsTriM), decreasing=TRUE)[1:10]
topnewsQuadriM<- sort(rowSums(newsQuadriM), decreasing=TRUE)[1:10]

twitterUniM <- as.matrix(rollup(twitterUni, 2, FUN=sum));
twitterBiM<- as.matrix(rollup(twitterBi, 2, FUN=sum));
twitterTriM <- as.matrix(rollup(twitterTri, 2, FUN=sum));
twitterQuadriM<- as.matrix(rollup(twitterQuadri, 2, FUN=sum));

toptwitterUniM <- sort(rowSums(twitterUniM), decreasing=TRUE)[1:10]
toptwitterBiM<<- sort(rowSums(twitterBiM), decreasing=TRUE)[1:10]
toptwitterTriM<- sort(rowSums(twitterTriM), decreasing=TRUE)[1:10]
toptwitterQuadriM<- sort(rowSums(twitterQuadriM), decreasing=TRUE)[1:10]

## Explanatory Analysis

## Entire File set
library(stringr)
countt <- str_length(twitter)
countn <- str_length(USnews)
countb <- str_length(Usblogs)

## Sample Data Analysis
Line_Length<- c(length(SampleBlogs), length(SampleNews), length(SampleTwitter))
Word_Length <- c(length(unlist(sapply(SampleBlogs, FUN=ngram_tokenizer(1), USE.NAMES=FALSE))), length(unlist(sapply(SampleNews, FUN=ngram_tokenizer(1), USE.NAMES=FALSE))), 
           length(unlist(sapply(SampleTwitter, FUN=ngram_tokenizer(1), USE.NAMES=FALSE))))

Term_Length <- c(length(blogsUniM ), length(newsUniM ), length(twitterUniM ))
count_Total <- data.frame(cbind(Line_Length, Word_Length, Term_Length), row.names=c("blogs", "news", "twitter"))
count_Total

#Frequencies

##Blogs

barplot(topblogsUniM, names.arg=names(topblogsUniM), main="Word Frequency Count for Blogs", xlab="Word", ylab="Frequency", col="blue")

barplot(topblogsBiM, names.arg=names(topblogsBiM), main="Bi-Gram Word Frequency Count for Blogs", xlab="Words", ylab="Frequency", col="green")

barplot(topblogsTriM, main="Tri-Gram Word Frequency Count for Blogs", xlab="Frequency", ylab="Words", col="orange")

barplot(topQuadriM, main="Quad-Gram Word Frequency Count for Blogs", xlab="Frequency", ylab="Words", col="purple")

##News

barplot(topnewsUniM, names.arg=names(topnewsUniM), main="Word Frequency Count for News", xlab="Word", ylab="Frequency", col="blue")

barplot(topnewsBiM, names.arg=names(topnewsBiM), main="Bi-Gram Word Frequency Count for News", xlab="Words", ylab="Frequency", col="green")

barplot(topnewsTriM, main="Tri-Gram Word Frequency Count for Newa", xlab="Frequency", ylab="Words", col="orange")

barplot(topnewsQuadriM, main="Quad-Gram Word Frequency Count for News", xlab="Frequency", ylab="Words", col="purple")



par(mfrow=c(2,2))
barplot(topblogsUniM, names.arg=names(topblogsUniM), main="Word Frequency Count for Blogs", xlab="Word", ylab="Frequency", col="blue")
barplot(topblogsBiM, names.arg=names(topblogsBiM), main="Bi-Gram Word Frequency Count for Blogs", xlab="Words", ylab="Frequency", col="green")
barplot(topblogsTriM, main="Tri-Gram Word Frequency Count for Blogs", xlab="Frequency", ylab="Words", col="orange")
barplot(topQuadriM, main="Quad-Gram Word Frequency Count for Blogs", xlab="Frequency", ylab="Words", col="purple")
