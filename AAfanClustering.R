library(tm)
library(RWeka)
library(ape)
library(RTextTools)
library(Rgraphviz)
home <- "/home/hinckley"
homePath = paste(home, "/Public/DFuncM", sep="")
setwd(paste(homePath, sep=""))
text <- system.file("texts", "txt", package="tm");
corpus <- Corpus(DirSource())
#corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, toupper)
#corpus <- tm_map(corpus, removeWords, c("MOVE")) #remove other functions
#corpus <- tm_map(corpus, stripWhitespace)
ngrams <- RWeka::NGramTokenizer(corpus, Weka_control(min=1, max=3))
dtm <- DocumentTermMatrix(corpus, control = list(ngrams, weighting = weightTfIdf))
#dtm <- create_matrix(cbind(as.vector(corpus)), language="english", minDocFreq=1, maxDocFreq=Inf,
# minWordLength=3, maxWordLength=Inf, ngramLength=3, originalMatrix=NULL,
# removeNumbers=FALSE, removePunctuation=TRUE, removeSparseTerms=0,
# removeStopwords=TRUE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE,
# weighting=weightTf)
#rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
#dtm <- dtm[rowTotals> 0] #remove all docs without words
#dtm <- removeSparseTerms(dtm, .99)
dtm_complete <- hclust(dist(dtm), members=NULL, method="centroid")
plot(hclust(dist(dtm), method="complete"), xlab="text from corpus", "ylab"="distance", main="Cluster Dendrogram \n of Various Russian Magic Tales")
op = par(bg="#DDE3CA")
plot(dtm_complete, col="#487AA1", col.main="#45ADA8", col.lab="#7C8071",
     col.axis="#F38630", lwd=1, lty=1, sub='', hang=-1, axes=FALSE,
     main = "Cluster Dendrogram Representing \n Magic Tale Similarity",
     xlab="Magic Tale Name", ylab = "Distance given absence/presence of Proppian Functions/Narremes")
par(op)
nplot(dtm_complete, hang=1, axes = TRUE, ann=TRUE, main = "Cluster Dendrogram Representing Magic Tale Similarity",
      xlab="Magic Tale Name", ylab = "Distance")
phyl <- as.phylo(hclust(dtm_distro))
plot(phyl, edge.col=c("blue", "green", "red")[c(TRUE, FALSE) + 1 + (phyl$edge.length > 20)])

#see how well particular functions correlate with each other above or beneath a threshold value
plot(dtm, corThreshold=.1)
tm::Zipf_plot(dtm)
tm::Heaps_plot(dtm)




