##Dissertation Project##
#Marshall A. Taylor
#Last updated: 1-04-18

#Necessary packages.
library(NLP)
packageurl <- "http://cran.r-project.org/src/contrib/Archive/tm/tm_0.6-2.tar.gz"
install.packages(packageurl, repos=NULL, type="source") #Must use this version. Weird issue with
                                                        #smart quotes/dashes when using newer
                                                        #versions. But need 7-3 for other features.
library(tm)
library(stm)
library(SnowballC)
library(devtools)
library(tm.plugin.sentiment)
library(igraph)
library(corclass)
library(dplyr)
library(qgraph)
library(gridExtra)
library(ggplot2)
library(plotrix)
library(stmBrowser)
library(entropy)
library(LDAvis)
library(servr)
library(RJSONIO)
library(gistr)
library(pheatmap)
library(grid)
library(GGally)
library(ggpubr)
library(reshape2)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(mclust)
library(ggpubr)
library(cluster)

#Loading data.
data <- file.path("/Users/marshalltaylor/Google Drive/Dissertation Materials/PDFs and TXTs/TXT2")
length(dir(data))
dir(data)
data <- Corpus(DirSource(data))
data <- data[-189] #To remove that weird "Icon" row that gets
                          #added from the -txtorg- function
as.character(data[[3]])
class(data)
summary(data)

#Pre-processing in tm.
data <- tm_map(data, stripWhitespace)
data <- tm_map(data, content_transformer(tolower))
data <- tm_map(data, removeNumbers)
data <- tm_map(data, removePunctuation)
data <- tm_map(data, removeWords, stopwords("english"))
data <- tm_map(data, stemDocument)
sum(tdm_freq)
tdm <- DocumentTermMatrix(data)
tdm <- removeSparseTerms(tdm, 0.4) #Leaves us with 456 unique terms (the total count for each term varies 
                                  #slightly now, probably b/c of a change in one of the tm
                                  #dependencies for stemming from when I ran the analysis)
tdm_freq <- rowSums(as.matrix(tdm)) #Getting the doc lengths (post stop word removal)
write.csv(tdm_freq,file="tdm_freq.csv")
tdm_freq2 <- as.matrix(tdm_freq)

#Loading in for analysis in stm (with metadata). Metadata matrix
                                      #structured using -txtorg-
                                      #tool.
meta <- read.csv("meta.csv", row.names=1, header=T) #Data in proportion form.
out <- readCorpus(tdm, type="slam")
out <- prepDocuments(out$documents, out$vocab, meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta 
write.csv(as.matrix(tdm),file="tdm.csv")
#Generating stm.
wn_select <- selectModel(docs, vocab, K=20, #Taking too long to run right now.
                         prevalence =~ s(year)+ideo+state+population_y+white_d+black_d+
                         ai_na_d+asian_d+other_race_d+male_d+female_d+lhs_d+hs_d+
                         college_d+s(pcinc_d)+urban_d+rural_d+s(per_repub), content =~ ideo,
                         max.em.its=75, data=meta2, runs=20,
                         init.type="LDA", seed=9811)
plotModels(wn_select)
wn_stm <- wn_select$runout[[]]
wn_modelk <- searchK(docs, vocab, K=c(5,10,15,20,25,30), N=50, heldout.seed=1234,
                         init.type="LDA", seed=9811)
plot(wn_modelk) #10 seems like a happy medium between the HLL, resid, and s.c., leaning mostly on s.c.

meta$ideo <- as.factor(meta$ideo)
meta$admin42 <- as.factor(meta$admin42)
meta$decade <- as.factor(meta$decade)
meta$org_type <- as.factor(meta$org_type)
wn_modelk <- searchK(docs, vocab, K=c(5,10,15,20,25,30), N=50,
                     heldout.seed=1234, init.type="LDA", seed=9811, data=meta)
plot(wn_modelk)

wn_stm5 <- stm(docs, vocab, K=5, #Ran four earlier versions that I kept for comparison. 
               prevalence =~ s(year) + ideo,
               max.em.its=500, data=meta, init.type="LDA", seed=9811)

meta$factor <- with(meta, interaction(as.factor(year), as.factor(org), drop=T))
wn_stm.factor <- stm(docs, vocab, K=5,
                     prevalence =~ s(year) + ideo, content =~ factor,
                     max.em.its=500, data=meta, init.type="LDA", seed=9811)
factor.label <- sageLabels(wn_stm.factor, n=10)
factor.label$marginal
plot(wn_stm.factor, type="perspectives", topics=4, covarlevels=c("2001.The First Freedom",
                                                                 "2002.The First Freedom"),
     n=80, plabels=c("The First Freedom, 2001","The First Freedom, 2002"))

plot(wn_stm.factor, type="perspectives", topics=4, covarlevels=c("2003.The Knights of the Ku Klux Klan/The Knights Party",
                                                                 "2004.The Knights of the Ku Klux Klan/The Knights Party"),
    plabels=c("The Knights Party, 2003","The Knights Party, 2004"))

plot(wn_stm.factor, type="perspectives", topics=4, covarlevels=c("1998.National Alliance",
                                                                 "1999.National Alliance"),
     n=80, plabels=c("National Alliance, 1998","National Alliance, 1999"))

#Diagnostics
topicQuality(wn_stm5, documents=docs) #To get the s.c. for the model
checkResiduals(wn_stm5, docs)
checkBeta(wn_stm5, tol=.1)
sageLabels(wn_stm5, n=20)[1]
#Interpret stm.
par(mfrow=c(2,1))
plot(wn_stm5, type="summary", n=5, main="", labeltype="prob")
par(mfrow=c(1,1))
sageLabels(wn_stm5)
theta2 <- wn_stm6$theta
rownames(theta2) <- rownames(tdm)
write.csv(theta2,file="theta2.csv")

#Making an LDA visualization for topic interpretation.
  ##Need to marginalize the logbeta distribution across the ideo covarates.
    #Begin marginalization
logbeta <- wn_stm5$beta$logbeta
margbeta <- exp(logbeta[[1]])
if(length(logbeta) > 1) {
  weights <- wn_stm5$settings$covariates$betaindex
  tab <- table(weights)
  weights <- tab/sum(tab)
  margbeta <- margbeta*weights[1]
  for(i in 2:length(wn_stm5$beta$logbeta)) {
    margbeta <- margbeta + exp(wn_stm5$beta$logbeta[[i]])*weights[i]
  }
}
    #End marginalization
theta <- wn_stm5$theta
term.freq <- colSums(as.matrix(tdm))
json <- createJSON(phi=margbeta, theta=theta, doc.length=tdm_freq, vocab=vocab, 
                      term.frequency=term.freq)
serVis(json, out.dir='vis', open.browser=interactive(),  as.gist=T)
json2 <- fromJSON(json)
json2$topic.order

#Another visualization
stmBrowse(wn_stm5, data=meta2, text="doc_id", id="doc_id", covariates=c("ideo","year"))
stata.out <- cbind(meta,theta)
write.csv(stata.out,file="stata_out.csv")

#Heat map for avg. posterior probability distribution by org.
avg.topics <- read.csv("avg_topics.csv", row.names=1, header=T)
draw_colnames_45 <- function (coln, ...) {
  m = length(coln)
  x = (1:m)/m - 1/2/m
  grid.text(coln, x = x, y = unit(0.96, "npc"), vjust = .5, 
            hjust = 1.2, rot = 45, gp = gpar(...)) ## Was 'hjust=0' and 'rot=270'
}
assignInNamespace(x="draw_colnames",value="draw_colnames_45",
                  ns=asNamespace("pheatmap"))
pheatmap(avg.topics, show_rownames=T, show_colnames=T, digits=3, display_numbers=T, 
         treeheight_row=0, treeheight_col=0, cluster_rows=F, cluster_cols=F, 
         number_format="%.3f")

#PCA with heat map data
wno.pca <- PCA(avg.topics, graph = F)
print(wno.pca)
wno.pca$eig #Two meaningful factors
wno.coord <- wno.pca$ind$coord

#Cluster with maximum likelihood
set.seed(123)
clus.pca <- Mclust(wno.coord)
summary(clus.pca$BIC, data=wno.coord)
wno.pca$cluster.mclust <- clus.pca$classification
wno.pca$cluster.mclust[which(wno.pca$cluster==1)] <- "One"
wno.pca$cluster.mclust[which(wno.pca$cluster==2)] <- "Two"

#Cluster with k-means (silhouette width suggests k = 2, and they are
  #similar to those from Mclust)
set.seed(123)
clus.kmeans <- kmeans(wno.coord, 2, nstart = 25)
clus.sil <- silhouette(clus.kmeans$cluster, dist(wno.coord))
fviz_silhouette(clus.sil)

pca.plot <- fviz_pca_biplot(wno.pca, pointshape = 21, pointsize = "cos2", labelsize = 3,
                fill.ind = factor(wno.pca$cluster), col.ind = "black",
                col.var = factor(c("ABPM", "AGNWO", "BI", "SSRAS", "CIR")),
                legend.title = list(fill = "WNO\nClusters", color = "Grievances",
                                    size = "Cosine\nSquared"),
                repel = T) +
  labs(title = "", x = "Dimension #1 (49.4%)", y = "Dimension #2 (25.5%)") +
  ggpubr::fill_palette(c("#00AFBB", "#FC4E07")) +
  ggpubr::color_palette("npg") +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "right")

png("pca_biplot.png", width = 14, height = 8.5, units = "in", res = 300)
pca.plot
dev.off()

#Save data
save.image("diss_code.RData")
