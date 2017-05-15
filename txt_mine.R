setwd("C:/Users/Mayank/Desktop/text_mining")
getwd()

library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
text_data <- read.csv("C:/Users/Mayank/Desktop/text_mining/predicting-terrorism/attacks_data_UTF8.csv",stringsAsFactors = FALSE)
str(text_data)
names(text_data)
doc <- Corpus(VectorSource(text_data$Description))
summary(doc)
inspect(doc[1:5])

#preprocessing
doc <- tm_map(doc,removePunctuation)
doc <- tm_map(doc,removeNumbers)
doc <- tm_map(doc,removeWords,stopwords(kind = "english"))
doc <- tm_map(doc,stemDocument)
doc <- tm_map(doc,stripWhitespace)
doc <- tm_map(doc,PlainTextDocument)
doc <- tm_map(doc,tolower)


dtm <- DocumentTermMatrix(doc)
tdm <- TermDocumentMatrix(doc)
inspect(tdm[1:10,1:10])

freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)
m <- as.matrix(dtm)
dim(m)
freq[head(ord)]
freq[tail(ord)]


doc.imp <- removeSparseTerms(tdm,0.97)
doc.imp
inspect(doc.imp[1:10,1:10])

temp <- inspect(doc.imp[1:10,1:10])
wordFreq <- data.frame(apply(temp,1,sum))
wordFreq <- data.frame(ST=row.names(wordFreq),Freq=wordFreq[,1])
head(wordFreq)

wordFreq <- wordFreq[order(wordFreq$Freq,decreasing = T),]

#basic analysis
#finding most frequent terms/words
findFreqTerms(tdm,10)
findFreqTerms(tdm,1000)

#finding association between terms/words
findAssocs(tdm,"terrorist",0.1)
findAssocs(tdm,"death",0.5)
findAssocs(tdm,"attack",0.09)
findAssocs(tdm,"bomb",0.2)


#Building a wordcloud
display.brewer.all()
brewer.pal
display.brewer.pal(8,"Dark2")

pal2 <- brewer.pal(8,"Dark2")
wordcloud(doc,min.freq = 50,max.words = 150,random.order = T,colors = pal2)
wordcloud(doc,min.freq = 50,max.words = 150,random.order = T,colors = pal2,vfont=c("script","plain"))
