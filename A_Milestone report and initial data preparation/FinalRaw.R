library(tm)
library(RWeka)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(data.table)
library(stringr)
library(qdap)
library(SnowballC)

set.seed(256)
Sys.setlocale(category = "LC_ALL", locale = "US")

## Load data and create data sample, unless already done so. Note that I use 
## absolute paths due to knitr restrictions. Kind of quick-and-dirty approach,
## but good enough for the assignment. Please adjust to your needs in case
## you want to run it on your computer.

path<-getwd()
url <-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
filename<- "SwiftkeyDB.zip"
filenameNpath<-file.path(path, filename)
if (!file.exists(path)) {dir.create(path)}
if (!file.exists(filenameNpath)){
        download.file(url, destfile = filenameNpath, mode = "wb")
        unzip(zipfile=filenameNpath, exdir="./Capstone")
}
if(length(list.files("C:/Users/claus/Documents/Capstone/final/en_US/sample"))<3){
        blogs <- readLines("C:/Users/claus/Documents/Capstone/final/en_US/en_US.blogs.txt")
        news <- readLines(file("C:/Users/claus/Documents/Capstone/final/en_US/en_US.news.txt"))
        twitter <- readLines("C:/Users/claus/Documents/Capstone/final/en_US/en_US.twitter.txt")
        
        blogs<-blogs[rbinom(length(blogs)* 0.05, length(blogs), 0.5)]
        news<-news[rbinom(length(news)*0.05, length(news), 0.5)]
        twitter<-twitter[rbinom(length(twitter)*0.05, length(twitter), 0.5)]
        
        write.csv(blogs, file="C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleBlogs.txt", row.names=FALSE, col.names = FALSE)
        write.csv(news, file="C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleNews.txt", row.names=FALSE, col.names = FALSE)
        write.csv(twitter, file="C:/Users/claus/Documents/Capstone/final/en_US/sample/sampleTwitter.txt", row.names=FALSE, col.names = FALSE)
}

corpus<-VCorpus(DirSource("C:/Users/claus/Documents/Capstone/final/en_US/sample/"))

removeMostPunctuation<-function(text, keep=c("'", ";", "-"))
{
        m<-sub_holder(keep, text)
        m$unhold(strip(m$output))
}

## Now the real cleansing and transforming using standard tm functionlity;
## skipping some standard steps for now:

##      corpus<-tm_map(corpus, removePunctuation)
##      corpus<-tm_map(corpus, removeMostPunctuation)
##      corpus<-tm_map(corpus, removeWords, stopwords("english"))
##      corpus<-tm_map(corpus, removeWords, c("placeholder for bad words"))
##      corpus<-tm_map(corpus, content_transformer(tolower))
##      corpus<-tm_map(corpus, removeNumbers)
corpusUnstemmed<-corpus
corpus<-tm_map(corpus, stemDocument)

## The below is too slow
## unstemmingDictionary <- corpus
## writeCorpus(unstemmingDictionary, path="./Capstone/final/unstemmingDictionary/", filenames=NULL)
## stemCompletion(c("encour"), unstemmingDictionary, type=("first"))

tdm <- function(corpus, n){
        ngramTokens <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngramTokens))
        tdm
}

## Calculating the n-grams and saving as csv files for later use in text prediction

        matrixTdm1<-as.matrix(tdm(corpus,1))
##        write.table(matrixTdm1, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/matrixTdm1.csv", sep=";", col.names=FALSE)
        matrixTdm2<-as.matrix(tdm(corpus,2))
##        write.table(matrixTdm2, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/matrixTdm2.csv", sep=";", col.names=FALSE)
##        matrixTdm3<-as.matrix(tdm(corpus,3))
##        write.table(matrixTdm3, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/matrixTdm3.csv", sep=";", col.names=FALSE)
##        matrixTdm4<-as.matrix(tdm(corpus,4))
##        write.table(matrixTdm4, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/matrixTdm4.csv", sep=";", col.names=FALSE)
##        matrixTdm5<-as.matrix(tdm(corpus,5))
##        write.table(matrixTdm5, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/matrixTdm5.csv", sep=";", col.names=FALSE)

## Creating the necessary data for unstemming using 2-gram-data
##        matrixUnstemmed<-as.matrix(tdm(corpusUnstemmed,2))
##        write.table(matrixUnstemmed, file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/unstemming/unstemming.csv", sep=";", col.names=TRUE)
        
## Cleansing the matrixes
##        B = matrix( c("---> whatever", "%&ç Hancöck", "And I", "would  ", "difficult but not /nimportant", "7"),  nrow=3,  ncol=2) 
##        matrixTest<-gsub("[^[:alnum:][:space:]]", "", B)
        
        matrixTest1<-as.matrix(read.csv(file="./Capstone/DataScienceCapstone/ngrams/matrixTdm2.csv", sep=";"))
        matrixTest2<-as.matrix(head(matrixTest1, 50000))
        matrixTest3<-as.matrix(gsub("[^[:alnum:][:space:]]", "", matrixTest2))
        matrixTest4<-as.data.frame(matrixTest3)
        matrixTest4[,2]<-as.numeric(as.character(matrixTest4[,2]))
        matrixTest4[,3]<-as.numeric(as.character(matrixTest4[,3]))
        matrixTest4[,4]<-as.numeric(as.character(matrixTest4[,4]))
        matrixTest4[,5]<-rowSums(matrixTest4[,2:4])
        
        matrixTest5<-as.data.frame(str_split_fixed(matrixTest4[,1], " ", 2))
        matrixTest6<-cbind(matrixTest4, matrixTest5)
        matrixTest6[matrixTest6==""]<-NA
        matrixTest6<-matrixTest6[complete.cases(matrixTest6),]
        matrixTest7<-matrixTest6[matrixTest6$V5>=20,]
        
## Input evaluation
        x<-c("was ist das ?")
        y<-str_split(x, " ")
        z<-as.data.frame(y)
        nrow(z)
        a<-as.String(z[nrow(z),1])
        b<-as.String(z[(nrow(z)-1),1])
        
## Word-stemming (part of SnowballC):
        wordStem(c("win", "winning", "winner"))
        
## Ordering data frames:
        matrixTest8<-matrixTest7[order(-matrixTest7[,5]),]
        
## giving the right names:
        matrixTest6<-setNames(matrixTest6, c("1","2","3","4","5","6","7"))
        
## chevk for string index of hits (probably better done with grep if no exact match?-->time?)
        which(matrixTest6[,1] %in% c("jar of"))
        
## working if-else-statement
        a<-101
        if(a==100){print("if ok")} else {print("NOK")}
        
## definition of statistics data frame
                dfStatistics <- data.frame(ngram=c(""),
                                   success=c(""),
                                   frequency=c(""),
                                   line=c(""),
                                   field=c(""),
                                   word=c(""),
                                   stringsAsFactors = FALSE)
        dfStatistics=rbind(dfStatistics, c("2", "1", "567", "1", "89", "Whatever"))
        dfStatistics=rbind(dfStatistics, c("5", "1", "567", "1", "90", "itis"))
        dfStatistics <- dfStatistics[NULL,]

## definition of temporary personalized statistics data frame (only for currennt search/counting)
        x<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/Personalized/matrixTdm1Personal.csv", sep=";", header=TRUE)

## go down the n-grams and check
        for (i in 1:4){print(i)}
        
## paste strings (merge)
        paste("his", "balls", sep=" ")
        
## bad words
        profanity<-c("fuck", "ass", "arse", "whore", "dick")
        check<-c("fuck")
        check %in% profanity
        if(check %in% profanity) {print("+*ç%&")}
## Unique ngram statistics per tdm matrix:

tdmStats <- matrix(c(1:5),ncol=5,byrow=TRUE)
row.names(tdmStats) <- c("Unique ngrams:")
colnames(tdmStats) <- c("Unigram","Bigram","Trigram", "Quadgram", "Pentagram")
tdmStats[1,1]<-nrow(matrixTdm1)
tdmStats[1,2]<-nrow(matrixTdm2)
tdmStats[1,3]<-nrow(matrixTdm3)
tdmStats[1,4]<-nrow(matrixTdm4)
tdmStats[1,5]<-nrow(matrixTdm5)
tdmStats

## Preparing the graphical output:

output <- function(ngramMatrix){
        ngramFreq<-data.frame(Ngram=rownames(ngramMatrix), 
                              blogs=ngramMatrix[,1], 
                              news=ngramMatrix[,2], 
                              twitter=ngramMatrix[,3])
        ngramFreq["sum"]<- rowSums(ngramFreq[,2:4])
        ngramFreq<-ngramFreq[order(-ngramFreq$sum),]
        ngramFreqBlogs<-ngramFreq[order(-ngramFreq[,2]), c(1,2,5)]
        ngramFreqBlogs["Type"]=c("blogs")
        colnames(ngramFreqBlogs)<-c("Ngram", "Frequency", "Sum", "Type")
        ngramFreqNews<-ngramFreq[order(-ngramFreq[,3]), c(1,3,5)]
        ngramFreqNews["Type"]=c("news")
        colnames(ngramFreqNews)<-c("Ngram", "Frequency", "Sum", "Type")
        ngramFreqTwitter<-ngramFreq[order(-ngramFreq[,4]), c(1,4,5)]
        ngramFreqTwitter["Type"]=c("twitter")
        colnames(ngramFreqTwitter)<-c("Ngram", "Frequency", "Sum", "Type")
        ngramFreqRep<-rbind(ngramFreqBlogs, ngramFreqNews, ngramFreqTwitter)
        colnames(ngramFreqRep)<-c("Ngram", "Frequency", "Sum", "Type")
        ngramFreqRep<-ngramFreqRep[order(-ngramFreqRep[,3], ngramFreqRep[,1]),]
        
        wordcloud(ngramFreqRep[1:102,]$Ngram, ngramFreqRep[1:102,]$Frequency, colors=brewer.pal(6,"Dark2"))
        
        plot<-ggplot(ngramFreqRep[1:102,], aes(x= reorder(Ngram, Frequency), y=Frequency, fill=Type)) +
                theme(axis.text=element_text(size=12)) +
                scale_x_discrete(name="Ngram ordered by Frequency") +
                geom_bar(stat="identity") +
                coord_flip()
        plot
}

## And the final overview as highlight of the exploratory data analysis

output(matrixTdm1)
output(matrixTdm2)
output(matrixTdm3)
output(matrixTdm4)
output(matrixTdm5)