cname<-file.path("C://Users//breya//Desktop//STJUDE//Word_docs_Interviews//Word_docs_Interviews")
cname
dir(cname)
library(tm)
library(SnowballC)
library(lsa)
docs<-Corpus(DirSource(cname))
summary(docs)


docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,removeWords,stopwords("english"))
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,stemDocument,language="english")
docs<-tm_map(docs,PlainTextDocument)
docs

td.mat <- as.matrix(TermDocumentMatrix(docs))
dist.mat <-dist(t(as.matrix(td.mat)))
dist.mat

fit<-cmdscale(dist.mat, eig=TRUE, k=2)
fit
x <- fit$points[,1]
y <- fit$points[,2]
plot(x,y,pch=19,xlim=range(x)+ c(-40,60),xlab="Coordinate 1", ylab="Coordinate 2",main="Clustering of Interviews Based on Terms",type="n")
Inter.names<-c("12M","15F","15M","17M","25M","03F","03M","04F","04M","05","06F","06M","JV","VN")
text(x,y, pos=4,labels=Inter.names)

td.mat.lsa<-lw_bintf(td.mat)*gw_idf(td.mat)
lsaspace<-lsa(td.mat.lsa)
dist.mat.lsa<-dist(t(as.textmatrix(lsaspace)))
dist.mat.lsa

#MDS
fit<-cmdscale(dist.mat.lsa, eig=TRUE, k=2)
fit
x <- fit$points[,1]
y <- fit$points[,2]
plot(x,y,pch=19,xlim=range(x)+ c(-40,60),xlab="Coordinate 1", ylab="Coordinate 2",main="Clustering of Interviews Based on Semantic Make-up",type="n")
Inter.names<-c("12M","15F","15M","17M","25M","03F","03M","04F","04M","05","06F","06M","JV","VN")
text(x,y, pos=4,labels=Inter.names)

library(scatterplot3d)
fit<-cmdscale(dist.mat.lsa,eig=TRUE,k=3)
scatterplot3d(fit$points[,1],fit$points[,2],fit$points[,3],pch=19,main="Semantic space Scaled to 3D",xlab="x",ylab="y",zlab="z",type="h")
Inter.names<-c("12M","15F","15M","17M","25M","03F","03M","04F","04M","05","06F","06M","JV","VN")
text(x,y,pos=4,cex=.5,labels=Inter.names)

#createWordClouds based on freq appearing words in interviews
library(wordcloud)
dtm <- DocumentTermMatrix(docs)
dtm

freq<-colSums(as.matrix(dtm))
length(freq)

ord<-order(freq)
m<-as.matrix(dtm)
dim(m)
write.csv(m,file="dtm_LFCodes.csv")

dtms<-removeSparseTerms(dtm,0.1)
inspect(dtms)

freq[head(ord)]
freq[tail(ord)]

head(table(freq),20)
tail(table(freq),20)

findFreqTerms(dtm,lowfreq=300)#most frequently used words in interviews >=300x

set.seed(142)
wordcloud(names(freq),freq,min.freq=300, scale=c(5,.1),colors=brewer.pal(6,"Dark2"))







