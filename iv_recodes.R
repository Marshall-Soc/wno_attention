##Other Code for Dissertation Project##
#Marshall A. Taylor
#Last updated: 1-04-18

#Packages
library(Hotelling)
library(rgr)
library(robCompositions)
library(compositions)
library(tm.plugin.sentiment)
library(tm)
install.packages('rJava','http://www.rforge.net/')
install.packages("RWeka")
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(rJava)
library(RWeka)
library(stringr)
library(igraph)
library(qgraph)
library(pluralize)
library(quanteda)
library(corclass)
library(gridExtra)
library(ggplot2)
library(statnet)
library(intergraph)
library(sentimentr)
library(tidytext)
library(Rstem)
library(RTextTools)
library(sentiment)
library(qdap)
library(GGally)

#Making the sentiment variables
  #Positive-negative
#sentimentr--sentence-level sentiment aggregrated to docs. "data" is only lowercased prior to analysis.
sent <- get_sentences(data$content)
pol <- sentiment_by(sent, by=NULL)
pol <- as.data.frame(pol)
tdm_freq2 <- as.data.frame(tdm_freq)
pol$doc_id <- rownames(tdm_freq2)
write.csv(pol, file="pol.csv")
highlight(pol, file="polarity.html", open=T)

#qdap: for polarity and subjectivity scores. "data" is only lowercases prior to analysis.
pol.qdap <- polarity(data$content)
pos.qdap <- pol.qdap$all$pos.words
neg.qdap <- pol.qdap$all$neg.words
df <- data.frame(matrix(ncol = 3, nrow = 384))
rownames(df) <- rownames(tdm_freq2)
colnames(df) <- c("pos","neg","wc")
for (i in 1:384){
  df[i,1] <- margin.table(table(pos.qdap[i]))
}
for (i in 1:384){
  df[i,2] <- margin.table(table(neg.qdap[i]))
}
for (i in 1:384){
  df[i,3] <- tdm_freq[i]
}

subj.qdap <- (df$pos+df$neg)/df$wc
df$subjqdap <- subj.qdap
df$polqdap <- pol.qdap$all$polarity

write.csv(df, file="subjq.csv")

#tidytext: for both positive and negative sentiment and specific emotions. "data" is entirely preprocessed
  #except for stemming.
nrc <- get_sentiments("nrc")
anger_nrc <- as.data.frame(nrc[which(nrc$sentiment=="anger"),])
fear_nrc <- as.data.frame(nrc[which(nrc$sentiment=="fear"),])
pos_nrc <- as.data.frame(nrc[which(nrc$sentiment=="positive"),])
neg_nrc <- as.data.frame(nrc[which(nrc$sentiment=="negative"),])
anger.nrc.n <- as.matrix(DocumentTermMatrix(data, list(dictionary=anger_nrc$word)))
fear.nrc.n <- as.matrix(DocumentTermMatrix(data, list(dictionary=fear_nrc$word)))
pos.nrc.n <- as.matrix(DocumentTermMatrix(data, list(dictionary=pos_nrc$word)))
neg.nrc.n <- as.matrix(DocumentTermMatrix(data, list(dictionary=neg_nrc$word)))

anger.nrc.sum <- rowSums(anger.nrc.n, na.rm=F)
fear.nrc.sum <- rowSums(fear.nrc.n, na.rm=F)
pos.nrc.sum <- rowSums(pos.nrc.n, na.rm=F)
neg.nrc.sum <- rowSums(neg.nrc.n, na.rm=F)

anger.nrc.total <- anger.nrc.sum/tdm_freq
fear.nrc.total <- fear.nrc.sum/tdm_freq
pos.nrc.total <- pos.nrc.sum/tdm_freq
neg.nrc.total <- neg.nrc.sum/tdm_freq
subj.nrc.total <- (pos.nrc.sum+neg.nrc.sum)/tdm_freq
pol.nrc.total <- (pos.nrc.sum-neg.nrc.sum)/(pos.nrc.sum+neg.nrc.sum)

tidysent <- cbind(anger.nrc.total,fear.nrc.total,pos.nrc.total,neg.nrc.total,subj.nrc.total, 
                  pol.nrc.total)
write.csv(tidysent,file="tidysent.csv")

#word-level sentiment (polarity here correlates at .78 with sentimentr, but I am more interested in the
  #subjectivity score from this word-level method). Also, subjectivity score here correlates well (.78)
  #with the subjectivity score generated from the -tidytext- package, and also correlates well (.70) with
  #subjectivity score generated from the -qdap- package. It correlates even better with -qdap- measure
  #after restricting correlations to cases valid in regression model #4 (.81), though a little less
  #with the -qdap- measure (.67). For both the -qdap- and -tidytext- scores, the denominator is 
  #tdm_freq--i.e., the doc-level word count after preprocessing and removal of sparse terms.
data("dic_gi") #Generated before removal of sparse terms, but otherwise preprocessed (with stemming).
positive <- as.matrix(DocumentTermMatrix(data, list(dictionary=dic_gi$positive)))
negative <- as.matrix(DocumentTermMatrix(data, list(dictionary=dic_gi$negative)))

possum <- rowSums(positive, na.rm=F) #Polarity scores
negsum <- rowSums(negative, na.rm=F)
diff <- possum - negsum
sum <- possum + negsum
polarity <- diff/sum
subj <- sum/tdm_freq #Note that the denominator here is word count POST SPARSE TERM REMOVAL. My version
                      #correlates much better with the qdap and tidytext subjectivity scores than the
                      #version with the denominator pre sparse term removal.

pos <- possum/tdm_freq #Positivity scores
neg <- negsum/tdm_freq #Negativity scores

pol <- cbind(pos,neg)
write.csv(polarity,file="pol.csv")
write.csv(subj,file="subj.csv")

  #Some specific emotions; NOTE: ALL OF THE EMOTION-SPECIFIC ANALYSES (tidytext, Jurka ML, syuzhet, 
      #and dictionary-based analysis using WordNet-Affect lexicon) CORRELATE TERRIBLY.
    #-sentiment- package by Jurka: Naive Bayes estimator. Words stemmed.
classify <- classify_emotion(data$content,algorithm="bayes",verbose=T)
classify

    #-sentiment- package by Jurka: Simple dictionary-based analysis. Words stemmed.
emotion_dict <- read.csv("emotions.csv",header=F) #From Tim Jurka's -sentiment- package. A list of emotion
                                                  #words from WordNet-Affect.
#Anger
anger_dict <- as.data.frame(emotion_dict[which(emotion_dict$V2=="anger"),])
#Disgust
disgust_dict <- as.data.frame(emotion_dict[which(emotion_dict$V2=="disgust"),])
#Joy
joy_dict <- as.data.frame(emotion_dict[which(emotion_dict$V2=="joy"),])
#Fear
fear_dict <- as.data.frame(emotion_dict[which(emotion_dict$V2=="fear"),])
#Sadness
sad_dict <- as.data.frame(emotion_dict[which(emotion_dict$V2=="sadness"),])
write.csv(anger_dict,file="anger_list.csv")
write.csv(fear_dict,file="fear_dict.csv")
anger.n <- as.matrix(DocumentTermMatrix(data, list(dictionary=as.matrix(anger_dict$V1))))
disgust.n <- as.matrix(DocumentTermMatrix(data, list(dictionary=as.matrix(disgust_dict$V1))))
joy.n <- as.matrix(DocumentTermMatrix(data, list(dictionary=as.matrix(joy_dict$V1))))
fear.n <- as.matrix(DocumentTermMatrix(data, list(dictionary=as.matrix(fear_dict$V1))))
sad.n <- as.matrix(DocumentTermMatrix(data, list(dictionary=as.matrix(sad_dict$V1))))

anger.sum <- rowSums(anger.n, na.rm=F)
disgust.sum <- rowSums(disgust.n, na.rm=F)
joy.sum <- rowSums(joy.n, na.rm=F)
fear.sum <- rowSums(fear.n, na.rm=F)
sad.sum <- rowSums(sad.n, na.rm=F)

anger <- anger.sum/tdm_freq
disgust <- disgust.sum/tdm_freq
joy <- joy.sum/tdm_freq
fear <- fear.sum/tdm_freq
sadness <- sad.sum/tdm_freq

emotion <- cbind(anger,disgust,joy,fear,sadness)
write.csv(emotion,file="emotion.csv") #For appending to dataset
write.csv(as.matrix(dtm2),file="dtm.csv")

#Concrete vs. Abstract Scores
con.abst <- read.csv("concreteness.csv", header=T)
dtm2 <- DocumentTermMatrix(data2) #data2 is the corpus w/o stemming or word removal
dtm.header <- singularize(unlist(tokenize(colnames(dtm2)))) #Singularize words
colnames(dtm2) <- dtm.header
dtm2list <- apply(dtm2, 1, function(x) {
   paste(rep(names(x), x), collapse=" ")
  })
data3 <- VCorpus(VectorSource(dtm2list))

options(mc.cores=1) #Same, but for bigrams
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm.2gram <- DocumentTermMatrix(data2, control=list(tokenize=BigramTokenizer))
dtm.header2 <- singularize(unlist(tokenize(colnames(dtm.2gram)))) #Singularize words
colnames(dtm.2gram) <- dtm.header2
colnames(dtm.2gram) <- gsub(" ","_",colnames(dtm.2gram),fixed=T)
dtm2list.2gram <- apply(dtm.2gram, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
  })
data4 <- VCorpus(VectorSource(dtm2list.2gram))

concrete.n <- as.matrix(DocumentTermMatrix(data3, list(dictionary=as.matrix(con.abst$concrete))))
abstract.n <- as.matrix(DocumentTermMatrix(data3, list(dictionary=as.matrix(con.abst$abstract))))
concrete.n2gram <- as.matrix(DocumentTermMatrix(data4, list(dictionary=as.matrix(con.abst$concrete))))
abstract.n2gram <- as.matrix(DocumentTermMatrix(data4, list(dictionary=as.matrix(con.abst$abstract))))
rownames(concrete.n) <- rownames(dtm2)
rownames(abstract.n) <- rownames(dtm2)
rownames(concrete.n2gram) <- rownames(dtm.2gram)
rownames(abstract.n2gram) <- rownames(dtm.2gram)

concrete.sum <- rowSums(concrete.n, na.rm=F)
abstract.sum <- rowSums(abstract.n, na.rm=F)
concrete.sum.2gram <- rowSums(concrete.n2gram,na.rm=F)
abstract.sum.2gram <- rowSums(abstract.n2gram,na.rm=F)
concrete.sum.total <- concrete.sum + concrete.sum.2gram
abstract.sum.total <- abstract.sum + abstract.sum.2gram
concrete.sum.total <- concrete.sum.total[-189] #Get rid of that weird ICON thing.
abstract.sum.total <- abstract.sum.total[-189]
concrete <- concrete.sum.total/tdm_freq
abstract <- abstract.sum.total/tdm_freq
conabst_pol <- (concrete - abstract)/(concrete+abstract)
conabst.total <- cbind(concrete, abstract, conabst_pol)
write.csv(conabst.total,file="concrete.csv") #For appending to dataset

#Getting an "attention to interorganizational field" variable
one_dict <- read.csv("one_dict.csv", header=T, stringsAsFactors = F)
one.dict.clean <- tolower(one_dict) #Removing capitalization in dictionary
one.dict.clean <- as.data.frame(sapply(one.dict.clean, function(x) gsub("\"", "", x)))
one.dict.clean <- one.dict.clean[,1]
one.dict.clean <- as.character(one.dict.clean)
one.dict.clean <- unlist(strsplit(one.dict.clean, ", "))
one.dict.clean <- gsub("\\(","", one.dict.clean)
one.dict.clean <- gsub("\\)","", one.dict.clean)
one.dict.clean <- gsub("cafbnp","afbnp", one.dict.clean)
OnegramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}

two_dict <- read.csv("two_dict.csv", header=T, stringsAsFactors = F)
two.dict.clean <- tolower(two_dict) #Removing capitalization in dictionary
two.dict.clean <- as.data.frame(sapply(two.dict.clean, function(x) gsub("\"", "", x)))
two.dict.clean <- two.dict.clean[,1]
two.dict.clean <- as.character(two.dict.clean)
two.dict.clean <- unlist(strsplit(two.dict.clean, ", "))
two.dict.clean <- gsub("\\(","", two.dict.clean)
two.dict.clean <- gsub("\\)","", two.dict.clean)
two.dict.clean <- gsub("cconfederate","confederate", two.dict.clean)
BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}

three_dict <- read.csv("three_dict.csv", header=T, stringsAsFactors = F)
three.dict.clean <- tolower(three_dict) #Removing capitalization in dictionary
three.dict.clean <- as.data.frame(sapply(three.dict.clean, function(x) gsub("\"", "", x)))
three.dict.clean <- three.dict.clean[,1]
three.dict.clean <- as.character(three.dict.clean)
three.dict.clean <- unlist(strsplit(three.dict.clean, ", "))
three.dict.clean <- gsub("\\(","", three.dict.clean)
three.dict.clean <- gsub("\\)","", three.dict.clean)
three.dict.clean <- gsub("camerican","american", three.dict.clean)
TrigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}

four_dict <- read.csv("four_dict.csv", header=T, stringsAsFactors = F)
four.dict.clean <- tolower(four_dict) #Removing capitalization in dictionary
four.dict.clean <- as.data.frame(sapply(four.dict.clean, function(x) gsub("\"", "", x)))
four.dict.clean <- four.dict.clean[,1]
four.dict.clean <- as.character(four.dict.clean)
four.dict.clean <- unlist(strsplit(four.dict.clean, ", "))
four.dict.clean <- gsub("\\(","", four.dict.clean)
four.dict.clean <- gsub("\\)","", four.dict.clean)
four.dict.clean <- gsub("canglo","anglo", four.dict.clean)
QuadgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))}

five_dict <- read.csv("five_dict.csv", header=T, stringsAsFactors = F)
five.dict.clean <- tolower(five_dict) #Removing capitalization in dictionary
five.dict.clean <- as.data.frame(sapply(five.dict.clean, function(x) gsub("\"", "", x)))
five.dict.clean <- five.dict.clean[,1]
five.dict.clean <- as.character(five.dict.clean)
five.dict.clean <- unlist(strsplit(five.dict.clean, ", "))
five.dict.clean <- gsub("\\(","", five.dict.clean)
five.dict.clean <- gsub("\\)","", five.dict.clean)
five.dict.clean <- gsub("calabama","alabama", five.dict.clean)
QuintgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 5, max = 5))}

seven_dict <- read.csv("seven_dict.csv", header=T, stringsAsFactors = F)
seven.dict.clean <- tolower(seven_dict) #Removing capitalization in dictionary
seven.dict.clean <- as.data.frame(sapply(seven.dict.clean, function(x) gsub("\"", "", x)))
seven.dict.clean <- seven.dict.clean[,1]
seven.dict.clean <- as.character(seven.dict.clean)
seven.dict.clean <- unlist(strsplit(seven.dict.clean, ", "))
seven.dict.clean <- gsub("\\(","", seven.dict.clean)
seven.dict.clean <- gsub("\\)","", seven.dict.clean)
seven.dict.clean <- gsub("ccarolina","carolina", seven.dict.clean)
SeptgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 7, max = 7))}

eight_dict <- read.csv("eight_dict.csv", header=T, stringsAsFactors = F)
eight.dict.clean <- tolower(eight_dict) #Removing capitalization in dictionary
eight.dict.clean <- as.data.frame(sapply(eight.dict.clean, function(x) gsub("\"", "", x)))
eight.dict.clean <- eight.dict.clean[,1]
eight.dict.clean <- as.character(eight.dict.clean)
eight.dict.clean <- unlist(strsplit(eight.dict.clean, ", "))
eight.dict.clean <- gsub("\\(","", eight.dict.clean)
eight.dict.clean <- gsub("\\)","", eight.dict.clean)
eight.dict.clean <- gsub("camerican","american", eight.dict.clean)
OctgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 8, max = 8))}

  #Only works with VCorpus, for some reason
data2 <- file.path("/Users/marshalltaylor/Google Drive/Dissertation Materials/PDFs and TXTs/TXT2")
data2 <- VCorpus(DirSource(data2))
data2 <- data2[-193] #To remove that weird "Icon" row that gets
                    #added from the -txtorg- function
data2 <- tm_map(data2, stripWhitespace)
data2 <- tm_map(data2, content_transformer(tolower))
data2 <- tm_map(data2, removeNumbers)
data2 <- tm_map(data2, removePunctuation)

  #Getting the N-grams
options(mc.cores=1)
onegrams <- as.matrix(DocumentTermMatrix(data2, control = list(tokenize=OnegramTokenizer,
                                                               dictionary=as.matrix(one.dict.clean))))
bigrams <- as.matrix(DocumentTermMatrix(data2, control = list(tokenize=BigramTokenizer,
                                                               dictionary=as.matrix(two.dict.clean))))
trigrams <- as.matrix(DocumentTermMatrix(data2, control = list(tokenize=TrigramTokenizer,
                                                               dictionary=as.matrix(three.dict.clean))))
quadgrams <- as.matrix(DocumentTermMatrix(data2, control = list(tokenize=QuadgramTokenizer,
                                                               dictionary=as.matrix(four.dict.clean))))
quintgrams <- as.matrix(DocumentTermMatrix(data2, control = list(tokenize=QuintgramTokenizer,
                                                               dictionary=as.matrix(five.dict.clean))))
septgrams <- as.matrix(DocumentTermMatrix(data2, control = list(tokenize=SeptgramTokenizer,
                                                               dictionary=as.matrix(seven.dict.clean))))
octgrams <- as.matrix(DocumentTermMatrix(data2, control = list(tokenize=OctgramTokenizer,
                                                               dictionary=as.matrix(eight.dict.clean))))
org.dict.mat <- cbind(onegrams,bigrams,trigrams,quadgrams,quintgrams,septgrams,octgrams)
write.csv(org.dict.mat,file="org_dict_mat2.csv")
org.dict.mat2 <- as.matrix(org.dict.mat)

  #Importing the directed square "attention matrix" after some moving around in Excel.
    #Specifically, I (1) adding together the N-grams referring to the same org, (2)
    #binarized the matrix, (3) added together the docs for each org so as to make this
    #an org-level covariate, and then (4) weighted each i-by-j cell by the sender's 
    #total number of documents in the corpus. I may be better off with an unweighted
    #matrix, though. ##Some of the raw counts for "League of the South" is mis-attributed
    # "League of the South" rather than "Alabama League of the South." This is not a 
    # concern, though, b/c each document where this happends ALSO mention "League of the
    # South," and, since the end matrix is binarized, the math works out the same.
attention.mat <- read.csv(file="attention_mat_weighted.csv",row.names=1,header=T)
attention.mat <- as.matrix(attention.mat)
t.att.mat <- t(attention.mat)
attention.net <- graph.adjacency(attention.mat, mode="directed", weighted=T, diag=F)
t.att.net <- graph.adjacency(t.att.mat, mode="directed", weighted=T, diag=F)
attention.unmat <- read.csv(file="attention_mat_unweighted.csv",row.names=1,header=T)
attention.unmat <- as.matrix(attention.unmat)
attention.unnet <- graph.adjacency(attention.unmat, mode="directed", weighted=T, diag=F)
attention.net

att.noduke <- read.csv(file="att_fullmat_woduke_w2.csv",row.names=1,header=T)
att.noduke <- as.matrix(att.noduke)
t.att.noduke <- t(att.noduke)
net.noduke <- graph.adjacency(att.noduke, mode="directed", weighted=T, diag=F)
t.net.noduke <- graph.adjacency(t.att.noduke, mode="directed", weighted=T, diag=F)

  #Getting centralities
indegree <- strength(attention.net, mode="in")
outdegree <- strength(attention.net, mode="out")
page <- page_rank(attention.net, algo="prpack", directed=T) 
t.page <- page_rank(t.att.net, algo="prpack", directed=T)
between <- betweenness(attention.net, directed=T, weights=NULL)
V(attention.net)$indegree <- indegree
V(attention.net)$outdegree <- outdegree
V(attention.net)$page <- page$vector  
V(attention.net)$between <- between
V(attention.net)$t_page <- t.page$vector

page.noduke <- page_rank(net.noduke, algo="prpack", directed=T)
t.page.noduke <- page_rank(t.net.noduke, algo="prpack", directed=T)
write.csv(page.noduke$vector,file="page_noduke.csv")
write.csv(t.page.noduke$vector,file="tpage_noduke.csv")

      #PageRank and betweenness.
att.8090.net <- graph.adjacency(as.matrix(att.8090[,5:28]), mode="directed", weighted=T, diag=F)
att.20.net <- graph.adjacency(as.matrix(att.20[,5:24]), mode="directed", weighted=T, diag=F)
page.8090 <- page_rank(att.8090.net, algo="prpack", directed=T) 
between.8090 <- betweenness(att.8090.net, directed=T, weights=NULL)
page.20 <- page_rank(att.20.net, algo="prpack", directed=T) 
between.20 <- betweenness(att.20.net, directed=T, weights=NULL)
V(att.8090.net)$page <- page.8090$vector  
V(att.8090.net)$between <- between.8090
V(att.20.net)$page <- page.20$vector  
V(att.20.net)$between <- between.20

  #Plot the interorganizational field attention networks (not organized right now)
colors <- colorRampPalette(c("#fdcf58","#800909"))
colors2 <- colorRampPalette(c("#011f4b","#b3cde0"))
colors3 <- colorRampPalette(c("#111111","#999999"))
fine=500
ind_col <- colors(fine)[as.numeric(cut(indegree,breaks=fine))]
outd_col <- colors(fine)[as.numeric(cut(outdegree,breaks=fine))]
page_col <- colors(fine)[as.numeric(cut(page$vector,breaks=fine))]
bet_col <- colors(fine)[as.numeric(cut(between,breaks=fine))]
page8090_col <- colors2(fine)[as.numeric(cut(page.8090$vector,breaks=fine))]
page20_col <- colors2(fine)[as.numeric(cut(page.20$vector,breaks=fine))]
bet8090_col <- colors(fine)[as.numeric(cut(between.8090,breaks=fine))]
bet20_col <- colors(fine)[as.numeric(cut(between.20,breaks=fine))]
edge_col <- colors3(fine)[as.numeric(cut(E(attention.net),breaks=fine))]

    #Indegree-weighted directed network
qgraph(attention.mat,
       mode="direct", layout = "spring", repulsion =.75,
       borders=T, shape = "circle", label.prop = 3, 
       curveAll=T, edge.labels=F, edge.label.cex = 0.45, esize=1,
       title="", color=ind_col, edge.color="#5A5255", diag=F, labels=colnames(attention.mat)
)

    #Outdegree-weighted directed network
qgraph(attention.mat,
       mode="direct", layout = "spring", repulsion =.75,
       borders=T, shape = "circle", label.prop = 3, 
       curveAll=T, edge.labels=F, edge.label.cex = 0.45, esize=1,
       title="", color=outd_col, edge.color="#5A5255", diag=F, labels=colnames(attention.mat)
)

  #PageRank-weighted directed network
g1 <- c(1,2,4,5,6,7,9,10,11,12,14,15,16,17,18,19,22,25,26,28,29,30,32,33,34)
g2 <- c(3,8,13,20,21,23,24,27,31)
groups <- list(g1,g2)
names(groups) <- c("No Entrepreneur","At Least One Entrepreneur")
shapes$shape[shapes$entrepreneur==0] <- "square"
shapes$shape[shapes$entrepreneur>0] <- "circle"
par(mfrow=c(2,1))
qgraph(attention.mat,
         directed=T, layout = "spring", repulsion=3,
         borders=T, shape="circle", groups=groups,
         curveAll=T, edge.labels=F, esize=5, edge.color="black", 
         transparency=F, fade=F, vsize=sqrt(page$vector*200),
         title="", color=c("#005b96","#b3cde0"), diag=F,
         border.width=2, legend=F, labels=rownames(attention.mat), label.cex=14
      )
qgraph(attention.mat,
       directed=T, layout = "spring", repulsion=1,
       borders=T, shape="circle", groups=groups,
       curveAll=T, edge.labels=F, esize=5, edge.color="black", 
       transparency=F, fade=F, vsize=sqrt(page$vector)*20,
       color=c("#005b96","#b3cde0"), diag=F,
       border.width=2, legend=F, labels=T
)
title("Interorganizational Attention scaled by PageRank centrality")

par(mfrow=c(1,1))
att.decades <- read.csv("attention.csv",header=T,row.names=1)
att.decades <- as.data.frame(att.decades)
  #1980-1990
attention.mat2 <- cbind(attention.mat,att.decades)
attention.mat2 <- cbind(attention.mat2,page$vector)
colnames(attention.mat2)[39] <- "page"
att.8090 <- attention.mat2[which(attention.mat2$X1980==1 | attention.mat2$X1990==1),]
att.80902 <- att.8090[which(attention.mat2$X1980==1 | attention.mat2$X1990==1)]
att.8090 <- merge(att.8090,att.80902, by="row.names")
att.8090 <- att.8090[c(-2:-38)]
  #2000
att.20 <- attention.mat2[which(attention.mat2$X2000==1),]
att.202 <- att.20[which(attention.mat2$X2000==1)]
att.20 <- merge(att.20,att.202, by="row.names")
att.20 <- att.20[c(-2:-38)]

att.8090 <- within(att.8090, {
  entrep2 <- factor(entrep2,
                  levels=c(0,1),
                  labels=c("No Entrepreneur","At Least One Entrepreneur"))
})

att.20 <- within(att.20, {
  entrep2 <- factor(entrep2,
                    levels=c(0,1),
                    labels=c("No Entrepreneur","At Least One Entrepreneur"))
})

par(mfrow=c(2,1))
qgraph(att.8090[4:27],
       directed=T, layout = "spring", repulsion=1,
       borders=T, shape="circle", groups=att.8090$entrep2,
       curveAll=F, edge.labels=F, esize=5, edge.color="black", 
       transparency=F, fade=F, vsize=sqrt(att.8090$page*200),
       color=c("#005b96","#b3cde0"), diag=F,
       border.width=2, legend=T, labels=F, legend.cex=.4,
       title="Interorganizational Attention, 1980s and 1990s"
      )
qgraph(att.20[4:23],
       directed=T, layout = "spring", repulsion=1,
       borders=T, shape="circle", groups=att.20$entrep2,
       curveAll=F, edge.labels=F, esize=5, edge.color="black", 
       transparency=F, fade=F, vsize=sqrt(att.20$page*200),
      color=c("#005b96","#b3cde0"), diag=F,
       border.width=2, legend=T, labels=F, legend.cex=.4,
      title="Interorganizational Attention, 2000s"
      )
par(mfrow=c(1,1))

  #Comparing PageRank to betweenness w/o differentiating nodes by time period.
par(mfrow=c(2,1))
qgraph(attention.mat,
       directed=T, layout = "spring", repulsion=1,
       borders=T, shape="circle", groups=groups,
       curveAll=T, edge.labels=F, esize=5, edge.color="black", 
       transparency=F, fade=F, vsize=scale(between)*2,
       color=c("#005b96","#b3cde0"), diag=F,
       border.width=2, legend=F, labels=F,
       title="Interorganizational Attention scaled by betweenness centrality"
      )
qgraph(attention.mat,
       directed=T, layout = "spring", repulsion=1,
       borders=T, shape="circle", groups=groups,
       curveAll=T, edge.labels=F, esize=5, edge.color="black", 
       transparency=F, fade=F, vsize=scale(page$vector)*2,
       color=c("#005b96","#b3cde0"), diag=F,
       border.width=2, legend=F, labels=F,
       title="Interorganizational Attention scaled by PageRank centrality"
      )
par(mfrow=c(1,1))

#Extra stuff post practice talk. Specifically a bipartite network where the rows are the org-year referee
#and columns are the org-year referents. Limited to those org-years that were mentioned at least once and
#org-years that mentioned at least once. How the matrix was constructed:
    # 1) N-gram dictionaries applied to docs.
    # 2) Columns "added together" for each organization.
    # 3) Binarized matrix.
    # 4) Added togehter rows for docs by an org in the same year.
    # 5) Created a new matrix where the columns were the org mentioned in a particular year. This is 
          # that matrix.
    # 6) Then used the column marginals (the indegrees for that org in that year) as the measure of 
          # attention-getting and matched them with the "observed" years for that org in the corpus.
          # This does mean that the measure is limited to name-drops for the org in the year for which
          # I have data on the mentioned org, but this is necessary, since one of my core IVs is a 
          # a doc-level measure.
new.attmat <- read.csv(file="new_attmat_tiesonly.csv",header=T,row.names=1)
new.attmat[is.na(new.attmat)] <- 0
new.attnet = network(new.attmat,
              matrix.type = "bipartite", ignore.eval=F, names.eval="weights")
groups <- c( "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	
             ">=1 Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	
             ">=1 Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	
             ">=1 Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	
             ">=1 Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	
             ">=1 Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	
             ">=1 Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	
             ">=1 Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	
             ">=1 Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	
             ">=1 Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	
             ">=1 Well-Known Leader",	">=1 Well-Known Leader",	">=1 Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader",	 "No Well-Known Leader",	 "No Well-Known Leader",	 
             "No Well-Known Leader")
new.attnet %v% "groups" <- groups
get.vertex.attribute(new.attnet, "groups")
set.seed(10052016)
ggnet2(new.attnet, color = "mode", color.palette = "Set2", alpha=1, size="degree", 
       label=T, label.size=2, edge.size = 1/(new.attnet %e% "weights")) +
  theme(legend.position="none")

set.seed(10052016)
ggnet2(new.attnet, color = "mode", color.palette = "Set2", alpha=1, size="degree", 
       label=T, label.size=2, shape="groups", edge.size = 1/(new.attnet %e% "weights")) +
  theme(legend.position="none")

# Seeing if PageRank changes drastically when matrix is 28x28 instead of 34x34
att.noduke2 <- read.csv(file="att_fullmat_woduke_w3.csv",row.names=1,header=T)
att.noduke2 <- as.matrix(att.noduke2)
t.att.noduke2 <- t(att.noduke2)
net.noduke2 <- graph.adjacency(att.noduke2, mode="directed", weighted=T, diag=F)
t.net.noduke2 <- graph.adjacency(t.att.noduke2, mode="directed", weighted=T, diag=F)

page.noduke2 <- page_rank(net.noduke2, algo="prpack", directed=T)
t.page.noduke2 <- page_rank(t.net.noduke2, algo="prpack", directed=T)
write.csv(page.noduke2$vector,file="page_noduke2.csv")
write.csv(t.page.noduke2$vector,file="tpage_noduke2.csv")

# Same as above, but for 27x27 after removing ANF (to see what happens when we limit cases to those post-1989)
att.noduke3 <- read.csv(file="att_fullmat_woduke_w4.csv",row.names=1,header=T)
att.noduke3 <- as.matrix(att.noduke3)
t.att.noduke3 <- t(att.noduke3)
net.noduke3 <- graph.adjacency(att.noduke3, mode="directed", weighted=T, diag=F)
t.net.noduke3 <- graph.adjacency(t.att.noduke3, mode="directed", weighted=T, diag=F)

page.noduke3 <- page_rank(net.noduke3, algo="prpack", directed=T)
t.page.noduke3 <- page_rank(t.net.noduke3, algo="prpack", directed=T)
write.csv(page.noduke3$vector,file="page_noduke3.csv")
write.csv(t.page.noduke3$vector,file="tpage_noduke3.csv")

#Line chart in Chapter 4
line <- read.csv(file="linechart2.csv", header=T)

line$org <- as.factor(line$org)

line <- line[which(line$imm_y!="NA"),]

ggplot(line, aes(x=year, y=imm_y)) +
  geom_point(shape=1) +    
  geom_smooth(method="lm", se=FALSE, color="black") +
  xlab("Year") + ylab(expression(paste("(Imm Focus)"[ij] - bar("(Imm Focus)")[j]))) +
  facet_wrap(~org) +
  xlim(1980, 2008) +
  theme_bw() 

#PPV Bar Chart in Chapter 4
ppv.bar <- read.csv(file="fe_estimates.csv", header=T)

ggplot(ppv.bar, aes(x=var, y=p, fill=var)) + 
  geom_hline(yintercept = .05, linetype="dashed", color = "black") +
  geom_text(aes(3.9,.05, label="p", vjust = -1, fontface=4)) +
  geom_text(aes(4.2,.05, label=" = .05", vjust = -1, fontface=2)) +
  geom_bar(position="dodge", color="black", stat="identity") +
  geom_errorbar(mapping=aes(x=var, ymin=ll, ymax=ul), color="black", width=0.2) +
  ylab(expression(paste(italic("P"), "-Values"))) + xlab("") +
  scale_y_continuous(limit=c(0,1)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.y=element_text(size=15)) +
  scale_x_discrete(limits=c("Constant","Construal Style (std)","Shock (log)",
                            "Shock * Const"),
                   labels=c(expression(paste(italic(beta), " = -4.124")), 
                            expression(paste(italic(beta), " = .193")),
                            expression(paste(italic(beta), " = .004")), 
                            expression(paste(italic(beta), " = .210")))) +
  scale_fill_manual(name=expression(paste(bold("Primary Predictors"))), 
                    values=c("white","gray75","gray25","black"),
                    breaks=c("Constant","Construal Style (std)","Shock (log)",
                             "Shock * Const"),
                    labels=c("Constant","Construal Style (std)","Shock (log)",
                             "Shock * Const"))

