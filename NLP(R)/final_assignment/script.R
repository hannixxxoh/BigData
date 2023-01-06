Steve <- vector()
Tim <- vector()

for (i in list.files(path="./TF", pattern="[.]txt$"))
{
file <- scan(file=paste("./TF", i, sep="/"), what="char", quote=NULL,
encoding="UTF-8")
j <- as.numeric(substring(i, nchar(i)-4,  nchar(i)-4))
if (j==1)
{Steve <- c(Steve,file)}
else
{Tim <- c(Tim, file)}
}



table(substring(Steve, nchar(Steve))) 

Steve <- gsub("^[[:punct:]]+|[[:punct:]]$", "", Steve)
Steve <- tolower(Steve[nchar(Steve)>0])

#빈도분석
Steve.Freq <- sort(table(Steve), decreasing=T)
Steve.Freq <- data.frame(row.names = names(Steve.Freq),
Freq = as.vector(Steve.Freq),
Rel.Freq = round(as.vector(Steve.Freq)/length(Steve), 3))
head(Steve.Freq, 10)

library(wordcloud)
wordcloud(rownames(Steve.Freq), Steve.Freq$Freq, scale=c(4, 1.2), min.freq=2,
max.words=100, random.order=F, rot.per=0.4, colors=brewer.pal(8, "Dark2"))
Steve<- Steve[!(Steve %in% "and")] #불용어제거
Steve<- Steve[!(Steve %in% "to")] #불용어제거
Steve<- Steve[!(Steve %in% "the")] #불용어제거





table(substring(Tim, nchar(Tim)))
Tim <- gsub("^[[:punct:]]+|[[:punct:]]$", "", Tim)
Tim <- tolower(Tim[nchar(Tim)>0])

#빈도분석
Tim.Freq <- sort(table(Tim), decreasing=T)
Tim.Freq <- data.frame(row.names = names(Tim.Freq),
Freq = as.vector(Tim.Freq),
Rel.Freq = round(as.vector(Tim.Freq)/length(Tim), 3))
head(Tim.Freq, 10)
library(wordcloud)
wordcloud(rownames(Tim.Freq), Tim.Freq$Freq, scale=c(4, 1.2), min.freq=2,
max.words=100, random.order=F, rot.per=0.4, colors=brewer.pal(8, "Dark2"))
Tim<- Tim[!(Tim%in% "and")] #불용어제거
Tim<- Tim[!(Tim%in% "to")] #불용어제거
Tim<- Tim[!(Tim%in% "the")] #불용어제거





#연어분석 1단계
node <- "\\bwe\\b"
index <- grep(node, Steve)
span <- vector()
for (i in index)
{
span <- c(span, c((i-4):(i-1), (i+1):(i+4)))
}
span <- span[span>0&span<=length(Steve)]
crc <- Steve[span]
head(crc, 20)
#연어분석 2단계
Freq.span <- sort(table(crc), decreasing=T)
Freq.all <- table(Steve)
Steve.Freq.co <- data.frame(t(sapply(names(Freq.span),
function(x) {c(length(index), Freq.all[x], Freq.span[x], length(Steve))})))
colnames(Steve.Freq.co) <- c('W1', 'W2', 'W1W2', 'N')
head(Steve.Freq.co)
#연어분석 3단계
collocates <- data.frame(Steve.Freq.co,
t.score = (Steve.Freq.co$W1W2 - ((Steve.Freq.co$W1*Steve.Freq.co$W2)/Steve.Freq.co$N))/sqrt(Steve.Freq.co$W1W2),
MI = log2((Steve.Freq.co$W1W2*Steve.Freq.co$N)/(Steve.Freq.co$W1*Steve.Freq.co$W2)))
head(collocates[order(collocates$t.score, decreasing=T),])
head(collocates[order(collocates$MI, decreasing=T),])
Steve.MI <- collocates[order(collocates$MI, decreasing=T),]
head(Steve.MI[Steve.MI$W1W2>3,], 15)




#연어분석 1단계
node <- "\\bwe\\b"
index <- grep(node, Tim)
span <- vector()
for (i in index)
{
span <- c(span, c((i-4):(i-1), (i+1):(i+4)))
}
span <- span[span>0&span<=length(Tim)]
crc <- Tim[span]
head(crc, 20)
#연어분석 2단계
Freq.span <- sort(table(crc), decreasing=T)
Freq.all <- table(Tim)
Tim.Freq.co <- data.frame(t(sapply(names(Freq.span),
function(x) {c(length(index), Freq.all[x], Freq.span[x], length(Tim))})))
colnames(Tim.Freq.co) <- c('W1', 'W2', 'W1W2', 'N')
head(Tim.Freq.co)
#연어분석 3단계
collocates <- data.frame(Tim.Freq.co,
t.score = (Tim.Freq.co$W1W2 - ((Tim.Freq.co$W1*Tim.Freq.co$W2)/Tim.Freq.co$N))/sqrt(Tim.Freq.co$W1W2),
MI = log2((Tim.Freq.co$W1W2*Tim.Freq.co$N)/(Tim.Freq.co$W1*Tim.Freq.co$W2)))
head(collocates[order(collocates$t.score, decreasing=T),])
head(collocates[order(collocates$MI, decreasing=T),])
Tim.MI <- collocates[order(collocates$MI, decreasing=T),]
head(Tim.MI[Tim.MI$W1W2>3,], 15)

#TDM
TDM <- data.frame(words=vector())
TDM <- merge(TDM,data.frame(table(Steve)), by.x="words", by.y="Steve", all=T)
TDM <- merge(TDM,data.frame(table(Tim)), by.x="words", by.y="Tim", all=T)
colnames(TDM)[c(2, 3)] <- c('Steve', 'Tim')
TDM[is.na(TDM)] <- 0
TDM <- data.frame(row.names=TDM$words, TDM[2:3])
colSums(TDM)

#comparision cloud
comparison.cloud(TDM[c(1,2)], random.order=FALSE, scale=c(2, 0.9), rot.per=0,
max.words=220, colors=brewer.pal(8, "Dark2"), title.size=1.1)

#chisq
CHI <- chisq.test(TDM[1:2])$residuals
CHI <- as.data.frame(CHI)
head(CHI[order(CHI$Steve, decreasing=T),],20)
head(CHI[order(CHI$Tim, decreasing=T),],20)

stop <- readLines('13_EnglishStopwords.txt')
NEW <- TDM[!(rownames(TDM) %in% stop), ]
#head(NEW)
TDM <- TDM[grepl("[[:alpha:]]", rownames(TDM)),]
head(sort(rowSums(TDM), decreasing=T), 12)
#keyword <- c("ipod", "revolutionary", "e-mail",
#"dear", "stories", "heart", "tell", "excited")


keyword <- c("revolutionary", "e-mail", "ipod",
"day", "stories", "ipad",
"people", "world", "design")

#PCA
library(AMR)
PCA <- prcomp(scale(TDM[keyword, 1:2]))
ggplot_pca(PCA, labels=rownames(TDM[keyword,]))
PCA <- prcomp(scale(t(TDM[keyword, 1:2])))
ggplot_pca(PCA, labels=rownames(t(TDM[keyword,])))