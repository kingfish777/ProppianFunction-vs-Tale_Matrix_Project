###############################################
# Author: Scott Alexander Malec
# Title: _AfanClust2.R
# Purpose: adventures in function/tale clustering
# with coded data from Appendix III
# of Vladimir Propp's Morphology of the Fairy Tale (1928)
#
# TO DO: create function to subdivide each function file at new line 
#  (which represents a move)
###############################################

###############################################
# load libraries
###############################################
library(lsa)  # latent semantic analysis aka Singular Value Decomposition (SVD)
library(tm) # R text mining module
library(RWeka) # Weka data mining
library(ape) # http://bioinformatics.oxfordjournals.org/content/20/2/289.abstract
# Analysis for Phylogenetic Evolution in R language
library(Rgraphviz) #bioConductor's interface to GraphViz, powerful data visualization tool


###############################################
# reset this directory to where you unzip/git pull your function texts
###############################################
home <- "/home/hinckley"
homePath = paste(home, "/Public/DFuncM", sep="") # git pull from github, delete junk, put R scripts in separate folder at higher level
setwd(paste(homePath, sep=""))
text <- system.file("texts", "txt", package="tm")
corpus <- Corpus(DirSource())
print(corpus[[13]])

###############################################
# use these to remove fine grained distinctions
###############################################
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
print(corpus[[13]])

###############################################
# use this to remove functions that are causing noise (if desired)
###############################################
corpus <- tm_map(corpus, removeWords, c("MOVE")) #, "return", "A")) #remove other functions

###############################################
# play around with this: sometimes whitespace is your friend
# but I'd say the verdict may be out on this step
###############################################
#corpus <- tm_map(corpus, stripWhitespace)
#print(corpus[[13]])

###############################################
# ngrams of functions, to decrease perplexity in our Proppian function model
# ... probably best used when snippet above to remove fine grained distinctions
# given the tiny size of this database
###############################################
ngrams <- RWeka::NGramTokenizer(corpus, Weka_control(min=2, max=4))
print(ngrams)
###############################################
# not sure if weighting function is appropriate yet
###############################################
# tf/idf with ngrams
#dtm <- DocumentTermMatrix(corpus, control = list(ngrams, weighting = weightTfIdf))
# tf/idf, no ngrams
dtm <- DocumentTermMatrix(corpus, control = list( control = list(ngrams, weighting = function (x)  weightTfIdf(x, normalize = TRUE))))
# tf weighting with ngrams
# dtm <- DocumentTermMatrix(corpus, control = list(ngrams, weighting = weightTf))
# tf weighting no ngrams
 dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
# binary weighting, no ngrams
dtm <- DocumentTermMatrix(corpus, control = list(ngrams, weighting = weightBin))
dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightBin))
dtm$dimnames$Terms
# smart weighting, with ngrams
dtm <- DocumentTermMatrix(corpus, control = list(ngrams, weighting = weightSMART))
dtm$dimnames$Terms
dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightSMART))
dtm$dimnames$Terms
# no weight, just ngrams
# dtm <- DocumentTermMatrix(corpus, control = list(ngrams))
# no weighting, no ngrams
dtm <- DocumentTermMatrix(corpus)
dtm$dimnames$Terms
dtm <- DocumentTermMatrix(corpus, control = list(ngrams))
library(RTextTools)
dtm <- t(RTextTools::create_matrix(corpus, ngramLength=2))
dtm$dimnames$Terms
###############################################
# use these lines to eliminate tales with no or NAN functions
###############################################
# use these lines with caution
###############################################
#rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
#dtm <- dtm[rowTotals> 0] #remove all docs without words
print(dtm)
###############################################
# eliminate hapax legomena (singleton) function-grams
# the lower the threshold, the less sparse the function-tale matrix becomes
###############################################
dtm <- removeSparseTerms(dtm, .98)
dtm$dimnames
print(dtm)
###############################################
# use distance method: "centroid", "ward", "complete", "mcquitty", etc.
###############################################
dtm_complete <- hclust(dist(dtm), method="ward")
dtm_distro <- hclust(dist(dtm), method="centroid")

###############################################
# plot hierarchical dendrogram of cluster of tale/function matrix
###############################################
plot(hclust(dist(dtm), method="complete"), xlab="text from corpus", "ylab"="distance", main="Cluster Dendrogram \n of Various Russian Magic Tales")
op = par(bg="#DDE3CA")
plot(dtm_complete, col="#487AA1", col.main="#45ADA8", col.lab="#7C8071",
     col.axis="#F38630", lwd=1, lty=1, sub='', hang=-1, axes=FALSE,
     main = "Cluster Dendrogram Representing \n Magic Tale Similarity",
     xlab="Magic Tale Name", ylab = "Distance given absence/presence of Proppian Functions/Narremes")

par(op)

plot(dtm_complete, hang=1, axes = TRUE, ann=TRUE, main = "Cluster Dendrogram Representing Magic Tale Similarity",
     xlab="Magic Tale Name", ylab = "Distance")

phyl <- as.phylo(hclust(dtm_distro))
plot(phyl, edge.col=c("blue", "green", "red")[c(TRUE, FALSE) + 1 + (phyl$edge.length > 20)])

################################################
# observe how particular functions are correlated or not
################################################
plot(dtm, corThreshold=.002)
################################################

################################################
# LSA fun
################################################
LSAspace <- lsa(dtm,dims=dimcalc_raw())
# svd(dtm)
# round(LSAspace$tk %*% diag(LSAspace$sk) %*% t(LSAspace$dk))

newLSAspace <- lsa(dtm, dims=2)
new_dtm <- round(as.textmatrix(newLSAspace),2)
#associate(dtm, "a_1")
#new_dtm$dimnames

####################################
# plot LSA analysis of tales 
###################################
t.locations <- newLSAspace$tk %*% diag(newLSAspace$sk)
plot(t.locations, type="n")
text(t.locations, labels=rownames(newLSAspace$tk))
t.locations
corpus[[13]]


