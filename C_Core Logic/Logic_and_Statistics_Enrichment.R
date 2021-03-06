
library(NLP)
library(data.table)
library(stringr)
library(SnowballC)
library(compiler)

enableJIT(1)

set.seed(256)
Sys.setlocale(category = "LC_ALL", locale = "US")

## Preparation:
        ## Load n-grams
        dfNgram2<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/cleansed/matrix2cleansed.csv", header=TRUE, sep = ",")
        dfNgram3<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/cleansed/matrix3cleansed.csv", header=TRUE, sep = ",")
        dfNgram4<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/cleansed/matrix4cleansed.csv", header=TRUE, sep = ",")
        dfNgram5<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/cleansed/matrix5cleansed.csv", header=TRUE, sep = ",")
        ## Load unstemming and punctuation tables
        dfUnstem<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/unstemming/cleansed/matrix1unstemmingCleansed.csv", header=TRUE, sep = ",")
        dfPunctuation<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/unstemming/cleansed/matrixPunctuationCleansed.csv", header=TRUE, sep = ";")
        ## Create statistics data frame
        
        dfStatistics <- data.frame(ngram=as.integer(0),
                                   personal=as.integer(0),
                                   frequency=as.integer(0),
                                   weight=as.numeric(0.00),
                                   weighted_frequency=as.numeric(0.00),
                                   line=as.integer(0),
                                   predicted_word=as.character(c("")),
                                   stringsAsFactors = FALSE)
        ## Create personalized tables:
        dfNgram2Personal<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/personalized/matrixTdm2Personal.csv", header=TRUE, sep = ";")
        dfNgram3Personal<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/personalized/matrixTdm3Personal.csv", header=TRUE, sep = ";")
        dfNgram4Personal<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/personalized/matrixTdm4Personal.csv", header=TRUE, sep = ";")
        dfNgram5Personal<-read.csv(file="C:/Users/claus/Documents/Capstone/DataScienceCapstone/ngrams/personalized/matrixTdm5Personal.csv", header=TRUE, sep = ";")
## Input evaluation
        ## Initialize statistics data frame:
        dfStatistics<-dfStatistics[NULL,]

        ## Take input and process (cutting words if input is >5 words, stemming etc.)
        input<-c("Enter text for prediction") 
        
        tableSplit<-as.data.table(str_split(input, " "))
        tableSplit[,1]<-lapply(tableSplit[,1], tolower)

        ## Chosing only the last four words in case in input is longer than 4 words:
        if (((nrow(tableSplit)) > 4)){
                tableSplit<-tableSplit[(nrow(tableSplit)-3):nrow(tableSplit),]
        }

        ## Input stemming and removal of special characters:
        nrows<-nrow(tableSplit)
        for (i in 1:nrows){
                tableSplit[i, 1] <- gsub("[[:punct:]]", "", tableSplit[i,1])
                tableSplit[i, 1] <- wordStem(tableSplit[i,1])
        }

## Fill statistics
        fill <- function(nrows){
                switch (nrows, 
                        "1"={ ##print("one")
                                if ( tableSplit[nrow(tableSplit),1] != c("")){
                                        personalHits<-which((dfNgram2Personal[,1] %in% tableSplit[nrow(tableSplit),1]))
                                        length<-length(personalHits)
                                        if (length > 0){
                                                for (i in 1:length){
                                                        dfStatistics[(length + i), 1]<-2
                                                        dfStatistics[(length + i), 2]<-1
                                                        dfStatistics[(length + i), 3]<-1
                                                        dfStatistics[(length + i), 4]<-0.97
                                                        dfStatistics[(length + i), 5]<-1000
                                                        dfStatistics[(length + i), 6]<-personalHits[i]
                                                        dfStatistics[(length + i), 7]<-as.String(dfNgram2Personal[personalHits[i],2])
                                                }
                                        }
                                        if (length == 0){
                                                hits<-which((dfNgram2[,3] %in%  tableSplit[nrow(tableSplit),1]))
                                                length<-length(hits)
                                                lengthStatistics<-nrow(dfStatistics)
                                                if(length > 0){
                                                        for (i in 1:length){
                                                                dfStatistics[(lengthStatistics + i), 1]<-2
                                                                dfStatistics[(lengthStatistics + i), 2]<-0
                                                                dfStatistics[(lengthStatistics + i), 3]<-dfNgram2[hits[i],2]
                                                                dfStatistics[(lengthStatistics + i), 4]<-0.15
                                                                dfStatistics[(lengthStatistics + i), 5]<-(dfStatistics[(lengthStatistics + i), 3] * dfStatistics[(lengthStatistics + i), 4])
                                                                dfStatistics[(lengthStatistics + i), 6]<-hits[i]
                                                                dfStatistics[(lengthStatistics + i), 7]<-as.String(dfNgram2[hits[i],4])
                                                        }
                                                } 
                                        }
                                } 
                                ## Setting a default value if no input
                                if (tableSplit[1,1] == c("")){
                                        length<-nrow(dfStatistics)
                                        dfStatistics[(length + 1), 1]<-2
                                        dfStatistics[(length + 1), 2]<-1
                                        dfStatistics[(length + 1), 3]<-1
                                        dfStatistics[(length + 1), 4]<-0.97
                                        dfStatistics[(length + 1), 5]<-1000
                                        dfStatistics[(length + 1), 6]<-0
                                        dfStatistics[(length + 1), 7]<-as.String(c("The"))
                                }
                                assign('dfStatistics', dfStatistics, envir=.GlobalEnv)
                        }, 
                        "2"={ ##print("two")
                                personalHits<-which((dfNgram3Personal[,1] %in% tableSplit[(nrow(tableSplit)-1),1]) & 
                                                    (dfNgram3Personal[,2] %in% tableSplit[ nrow(tableSplit)   ,1]))
                                length<-length(personalHits)
                                if (length > 0){
                                        for (i in 1:length){
                                                dfStatistics[(length + i), 1]<-3
                                                dfStatistics[(length + i), 2]<-1
                                                dfStatistics[(length + i), 3]<-1
                                                dfStatistics[(length + i), 4]<-0.98
                                                dfStatistics[(length + i), 5]<-1000
                                                dfStatistics[(length + i), 6]<-personalHits[i]
                                                dfStatistics[(length + i), 7]<-as.String(dfNgram3Personal[personalHits[i],3])
                                        }
                                }
                                if (length == 0){
                                        hits<-which((dfNgram3[,3] %in% tableSplit[(nrow(tableSplit)-1),1]) & 
                                                    (dfNgram3[,4] %in% tableSplit[nrow(tableSplit),1]))
                                        length<-length(hits)
                                        lengthStatistics<-nrow(dfStatistics)
                                        if(length > 0){
                                                for (i in 1:length){
                                                        dfStatistics[(lengthStatistics + i), 1]<-3
                                                        dfStatistics[(lengthStatistics + i), 2]<-0
                                                        dfStatistics[(lengthStatistics + i), 3]<-dfNgram3[hits[i],2]
                                                        dfStatistics[(lengthStatistics + i), 4]<-0.25
                                                        dfStatistics[(lengthStatistics + i), 5]<-(dfStatistics[(lengthStatistics + i), 3] * dfStatistics[(lengthStatistics + i), 4])
                                                        dfStatistics[(lengthStatistics + i), 6]<-hits[i]
                                                        dfStatistics[(lengthStatistics + i), 7]<-as.String(dfNgram3[hits[i], 5])
                                                }
                                        }
                                        ## Fill personalized hit list:
                                        nrowsNgram<-nrow(dfNgram2Personal)
                                        dfNgram2Personal[nrowsNgram+1, 1]<-tableSplit[1,1]
                                        dfNgram2Personal[nrowsNgram+1, 2]<-tableSplit[2,1]
                                }
                                assign('dfStatistics', dfStatistics, envir=.GlobalEnv)
                                assign('dfNgram2Personal', dfNgram2Personal, envir=.GlobalEnv)
                        },
                        "3"={ ##print("three")
                                personalHits<-which((dfNgram4Personal[,1] %in% tableSplit[(nrow(tableSplit) - 2),1]) & 
                                                    (dfNgram4Personal[,2] %in% tableSplit[(nrow(tableSplit) - 1),1]) &
                                                    (dfNgram4Personal[,3] %in% tableSplit[nrow(tableSplit)      , 1]))
                                length<-length(personalHits)
                                if (length > 0){
                                        for (i in 1:length){
                                                dfStatistics[(length + i), 1]<-4
                                                dfStatistics[(length + i), 2]<-1
                                                dfStatistics[(length + i), 3]<-1
                                                dfStatistics[(length + i), 4]<-0.99
                                                dfStatistics[(length + i), 5]<-1000
                                                dfStatistics[(length + i), 6]<-personalHits[i]
                                                dfStatistics[(length + i), 7]<-as.String(dfNgram4Personal[personalHits[i],4])
                                        }
                                }
                                if (length == 0){
                                        hits<-which((dfNgram4[,3] %in% tableSplit[(nrow(tableSplit) - 2),1]) &
                                                    (dfNgram4[,4] %in% tableSplit[(nrow(tableSplit) - 1),1]) & 
                                                    (dfNgram4[,5] %in% tableSplit[nrow(tableSplit)     , 1]))
                                        length<-length(hits)
                                        lengthStatistics<-nrow(dfStatistics)
                                        if(length > 0){
                                                for (i in 1:length){
                                                        dfStatistics[(lengthStatistics + i), 1]<-3
                                                        dfStatistics[(lengthStatistics + i), 2]<-0
                                                        dfStatistics[(lengthStatistics + i), 3]<-dfNgram4[hits[i],2]
                                                        dfStatistics[(lengthStatistics + i), 4]<-0.5
                                                        dfStatistics[(lengthStatistics + i), 5]<-(dfStatistics[(lengthStatistics + i), 3] * dfStatistics[(lengthStatistics + i), 4])
                                                        dfStatistics[(lengthStatistics + i), 6]<-hits[i]
                                                        dfStatistics[(lengthStatistics + i), 7]<-as.String(dfNgram4[hits[i],6])
                                                }
                                        }
                                        ## Fill personalized hit list:
                                        nrowsNgram<-nrow(dfNgram3Personal)
                                        dfNgram3Personal[nrowsNgram+1, 1]<-tableSplit[1,1]
                                        dfNgram3Personal[nrowsNgram+1, 2]<-tableSplit[2,1]
                                        dfNgram3Personal[nrowsNgram+1, 3]<-tableSplit[3,1]
                                }
                                assign('dfStatistics', dfStatistics, envir=.GlobalEnv)
                                assign('dfNgram3Personal', dfNgram3Personal, envir=.GlobalEnv)
                        },
                        "4"={ ##print("four")
                                personalHits<-which((dfNgram5Personal[,1] %in% tableSplit[(nrow(tableSplit) - 3),1]) & 
                                                    (dfNgram5Personal[,2] %in% tableSplit[(nrow(tableSplit) - 2),1]) &
                                                    (dfNgram5Personal[,3] %in% tableSplit[(nrow(tableSplit) - 1),1]) &
                                                    (dfNgram5Personal[,4] %in% tableSplit[nrow(tableSplit)      ,1]))
                                length<-length(personalHits)
                                if (length > 0){
                                        for (i in 1:length){
                                                dfStatistics[(length + i), 1]<-4
                                                dfStatistics[(length + i), 2]<-1
                                                dfStatistics[(length + i), 3]<-1
                                                dfStatistics[(length + i), 4]<-1
                                                dfStatistics[(length + i), 5]<-1000
                                                dfStatistics[(length + i), 6]<-personalHits[i]
                                                dfStatistics[(length + i), 7]<-as.String(dfNgram5Personal[personalHits[i],5])
                                        }
                                }
                                if (length == 0){
                                        hits<-which((dfNgram5[,3] %in% tableSplit[(nrow(tableSplit) - 3),1]) &
                                                    (dfNgram5[,4] %in% tableSplit[(nrow(tableSplit) - 2),1]) &
                                                    (dfNgram5[,5] %in% tableSplit[(nrow(tableSplit) - 1),1]) &
                                                    (dfNgram5[,6] %in% tableSplit[ nrow(tableSplit)     ,1]))
                                        length<-length(hits)
                                        lengthStatistics<-nrow(dfStatistics)
                                        if(length > 0){
                                                for (i in 1:length){
                                                        dfStatistics[(lengthStatistics + i), 1]<-5
                                                        dfStatistics[(lengthStatistics + i), 2]<-0
                                                        dfStatistics[(lengthStatistics + i), 3]<-dfNgram5[hits[i],2]
                                                        dfStatistics[(lengthStatistics + i), 4]<-0.75
                                                        dfStatistics[(lengthStatistics + i), 5]<-(dfStatistics[(lengthStatistics + i), 3] * dfStatistics[(lengthStatistics + i), 4])
                                                        dfStatistics[(lengthStatistics + i), 6]<-hits[i]
                                                        dfStatistics[(lengthStatistics + i), 7]<-as.String(dfNgram5[hits[i],7])
                                                }
                                        }
                                        ## Fill personalized hit list:
                                        nrowsNgram<-nrow(dfNgram4Personal)
                                        dfNgram4Personal[nrowsNgram + 1, 1]<-tableSplit[1,1]
                                        dfNgram4Personal[nrowsNgram + 1, 2]<-tableSplit[2,1]
                                        dfNgram4Personal[nrowsNgram + 1, 3]<-tableSplit[3,1]
                                        dfNgram4Personal[nrowsNgram + 1, 4]<-tableSplit[4,1]
                                }
                                assign('dfStatistics', dfStatistics, envir=.GlobalEnv)
                                assign('dfNgram4Personal', dfNgram4Personal, envir=.GlobalEnv)
                        }
               )
        }
        
        fill<-cmpfun(fill)
        ## Final determination of the prediction suggestion (filling dfStatistics)
        for (i in 1:nrows){
                fill(i)
        }
        
## Determine best result from dfStatistics
        dfStatistics<-dfStatistics[order(-dfStatistics[,1], 
                                         -dfStatistics[,2], 
                                         -dfStatistics[,5]),]
        prediction<-as.character(dfStatistics[1,7])

## Output generation
        
        ## Profanity check:
        profanity<-c("fuck", "ass", "arse", "whore", "dick", "pussy", "asshole", "assholes")
        if(prediction %in% profanity) {prediction<-as.character(c("*****"))}
        
        ## custom-made unstemming
        temp<-which(dfUnstem[,4] %in% prediction)
        if(length(temp)>0){
                prediction<-dfUnstem[temp[1],3]
                }
        ## Bring punctuation back:
        punctuation<-which(tolower(dfPunctuation[,1]) %in% prediction)
        if(length(punctuation)>0){prediction<-as.character(dfPunctuation[punctuation, 2])}

        ## In case prediction could not be determined, the most frequent word is set:
        if (prediction %in% c("", " ", NA)) {prediction <- as.String("the")}
        
        ## paste prediction to original input:
        if(input != c("")){
                output<-paste(input, prediction, sep=" ")
        } else {
                if(tableSplit(nrow(tablesplit))==c("the")){
                        prediction<-c("best")
                }
                output<-prediction
        }
        output
        
        dfStatistics<-dfStatistics[complete.cases(dfStatistics),]
        
        last_input<-input
        
##        matrixTest6<-setNames(matrixTest6, c("1","2","3","4","5","6","7"))

