###  Damien Edwards
#### Capstone Project
### Task 0

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

twitter <- readLines('data/final/en_US/en_US.twitter.txt', encoding = 'UTF-8')
USnews<- readLines('data/final/en_US/en_US.news.txt', encoding = 'UTF-8')
Usblogs <- readLines('data/final/en_US/en_US.blogs.txt', encoding = 'UTF-8')


entwitter <- strsplit(twitter, " ")
enUsnews<-strsplit(USnews, " ")
enUsblogs<-strsplit(Usblogs, " ")

# Count Data

library(stringr)
countt <- str_length(twitter)
countn <- str_length(USnews)
countb <- str_length(Usblogs)

length(grep('love', twitter))
length(grep('hate', twitter))

twitter[grep('biostats', twitter)]

length(grep('A computer once beat me at chess, but it was no match for me at kickboxing', twitter))


