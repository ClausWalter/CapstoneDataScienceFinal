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

## Loading and cleansig the output matrixes:
cleansing <- function(ngramMatrix, ngram){
        matrixTest2<-as.matrix(read.csv(file=ngramMatrix, row.names=NULL, sep=";"))
     ##   matrixTest2<-as.matrix(head(matrixTest1, 5000))
        matrixTest3<-as.matrix(gsub("[^[:alnum:][:space:]]", "", matrixTest2))
        matrixTest4<-as.data.frame(matrixTest3)
        matrixTest4[,2]<-as.numeric(as.character(matrixTest4[,2]))
        matrixTest4[,3]<-as.numeric(as.character(matrixTest4[,3]))
        matrixTest4[,4]<-as.numeric(as.character(matrixTest4[,4]))
        matrixTest4[,5]<-rowSums(matrixTest4[,2:4])
        
        matrixTest5<-as.data.frame(str_split_fixed(matrixTest4[,1], " ", n=ngram))
        matrixTest6<-cbind(matrixTest4, matrixTest5)
        matrixTest6[matrixTest6==""]<-NA
        matrixTest6<-matrixTest6[complete.cases(matrixTest6),]
        matrixTest7<-matrixTest6 ##matrixTest6[matrixTest6$V5>=20,]
        upper<-5+ngram
        matrixTest8<-matrixTest7[, 5:upper]
}

matrix1cleansed<-cleansing(c("./Capstone/DataScienceCapstone/ngrams/matrixTdm1.csv"), 1)
for (i in 2:ncol(matrix1cleansed)){
        matrix1cleansed[,i]<-wordStem(matrix1cleansed[,i])
}
write.csv(matrix1cleansed, file="./Capstone/DataScienceCapstone/ngrams/cleansed/matrix1cleansed.csv")

matrix2cleansed<-cleansing(c("./Capstone/DataScienceCapstone/ngrams/matrixTdm2.csv"), 2)
for (i in 2:ncol(matrix2cleansed)){
        matrix2cleansed[,i]<-wordStem(matrix2cleansed[,i])
}
write.csv(matrix2cleansed, file="./Capstone/DataScienceCapstone/ngrams/cleansed/matrix2cleansed.csv")

matrix3cleansed<-cleansing(c("./Capstone/DataScienceCapstone/ngrams/matrixTdm3.csv"), 3)
for (i in 2:ncol(matrix2cleansed)){
        matrix3cleansed[,i]<-wordStem(matrix3cleansed[,i])
}
write.csv(matrix3cleansed, file="./Capstone/DataScienceCapstone/ngrams/cleansed/matrix3cleansed.csv")

matrix4cleansed<-cleansing(c("./Capstone/DataScienceCapstone/ngrams/matrixTdm4.csv"), 4)
for (i in 2:ncol(matrix4cleansed)){
        matrix4cleansed[,i]<-wordStem(matrix4cleansed[,i])
}
write.csv(matrix4cleansed, file="./Capstone/DataScienceCapstone/ngrams/cleansed/matrix4cleansed.csv")

matrix5cleansed<-cleansing(c("./Capstone/DataScienceCapstone/ngrams/matrixTdm5.csv"), 5)
for (i in 2:ncol(matrix5cleansed)){
        matrix5cleansed[,i]<-wordStem(matrix5cleansed[,i])
}
write.csv(matrix5cleansed, file="./Capstone/DataScienceCapstone/ngrams/cleansed/matrix5cleansed.csv")

## Cleansing the unstemming repository:
matrix1unstemmingCleansed<-cleansing(c("./Capstone/DataScienceCapstone/unstemming/unstemming1.csv"), 1)
matrix1unstemmingCleansed[,3]<-wordStem(as.character(matrix1unstemmingCleansed[,2]))
write.csv(matrix1unstemmingCleansed, file="./Capstone/DataScienceCapstone/unstemming/cleansed/matrix1unstemmingCleansed.csv")
matrix2unstemmingCleansed<-cleansing(c("./Capstone/DataScienceCapstone/unstemming/unstemming2.csv"), 2)
matrix2unstemmingCleansed[,4]<-wordStem(as.character(matrix2unstemmingCleansed[,2]))
matrix2unstemmingCleansed[,5]<-wordStem(as.character(matrix2unstemmingCleansed[,3]))
write.csv(matrix2unstemmingCleansed, file="./Capstone/DataScienceCapstone/unstemming/cleansed/matrix2unstemmingCleansed.csv")


