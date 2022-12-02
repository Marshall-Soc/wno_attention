kkkk.terms <- read.csv(file="tdm.csv", header=T, row.names=1)

kkkk.terms <- prop.table(kkkk.terms)

kkkk.terms <- kkkk.terms*1000

kkkk.terms03 <- kkkk.terms[c(1:3),]
kkkk.terms04 <- kkkk.terms[c(4:6),]

kkkk.terms03 <- t(as.matrix(kkkk.terms03)) %*% as.matrix(kkkk.terms03)
kkkk.terms04 <- t(as.matrix(kkkk.terms04)) %*% as.matrix(kkkk.terms04)

#2003
kkkk.net03 <- graph.adjacency(kkkk.terms03, diag=F, mode="undirected", weighted=T)
weight <- E(kkkk.net03)$weight
quantile(weight, .99)
kkkk.terms03 <- ifelse(kkkk.terms03 < 4.088933, 0, kkkk.terms03)

kkkk.net03 <- graph.adjacency(kkkk.terms03, diag=F, mode="undirected", weighted=T)
isolates03 <- V(kkkk.net03)[degree(kkkk.net03) == 0]
kkkk.net03 <- delete_vertices(kkkk.net03, isolates03)
eigen03 <- eigen_centrality(kkkk.net03, directed=F) 
V(kkkk.net03)$eigen <- eigen03$vector

V(kkkk.net03)$color = V(kkkk.net03)$name
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "immigr", "#800000", "#a8e6cf")
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "american", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "law", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "america", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "group", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "countri", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "report", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "issu", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "million", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "job", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "legal", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "offici", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "hous", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "recent", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "social", "#ff8b94", V(kkkk.net03)$color)

V(kkkk.net03)$lab = NA 
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "immigr", "immigr", NA)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "american", "american", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "law", "law", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "america", "america", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "group", "group", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "countri", "countri", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "report", "report", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "issu", "issu", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "million", "million", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "job", "job", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "legal", "legal", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "offici", "offici", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "hous", "hous", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "recent", "recent", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "social", "social", V(kkkk.net03)$lab)

neighbors(kkkk.net03, V(kkkk.net03)$name=="immigr")

#2004
kkkk.net04 <- graph.adjacency(kkkk.terms04, diag=F, mode="undirected", weighted=T)
weight <- E(kkkk.net04)$weight
quantile(weight, .99)
kkkk.terms04 <- ifelse(kkkk.terms04 < 2.685528, 0, kkkk.terms04)

kkkk.net04 <- graph.adjacency(kkkk.terms04, diag=F, mode="undirected", weighted=T)
isolates04 <- V(kkkk.net04)[degree(kkkk.net04) == 0]
kkkk.net04 <- delete_vertices(kkkk.net04, isolates04)
eigen04 <- eigen_centrality(kkkk.net04, directed=F) 
V(kkkk.net04)$eigen <- eigen04$vector

V(kkkk.net04)$color = V(kkkk.net04)$name 
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "immigr", "#800000", "#a8e6cf")
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "american", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "law", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "america", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "group", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "countri", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "report", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "issu", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "million", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "job", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "legal", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "offici", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "hous", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "recent", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "social", "#ff8b94", V(kkkk.net04)$color)

V(kkkk.net04)$lab = NA 
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "immigr", "immigr", NA)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "american", "american", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "law", "law", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "america", "america", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "group", "group", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "countri", "countri", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "report", "report", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "issu", "issu", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "million", "million", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "job", "job", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "legal", "legal", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "offici", "offici", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "hous", "hous", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "recent", "recent", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "social", "social", V(kkkk.net04)$lab)

neighbors(kkkk.net04, V(kkkk.net04)$name=="immigr")

#Plotting
neighb03 <- neighbors(kkkk.net03, V(kkkk.net03)$name=="immigr")
kkkk.net03.simp <- induced_subgraph(kkkk.net03, vids=neighb03)
neighb04 <- neighbors(kkkk.net04, V(kkkk.net04)$name=="immigr")
kkkk.net04.simp <- induced_subgraph(kkkk.net04, vids=neighb04)

between03 <- as.data.frame(evcent(kkkk.net03, directed=F, weights=E(kkkk.net03)$weight)$vector)
between04 <- as.data.frame(evcent(kkkk.net04, directed=F, weights=E(kkkk.net04)$weight)$vector)
colnames(between03) <- "between03"
colnames(between04) <- "between04"
between03$names <- rownames(between03)
between04$names <- rownames(between04)
between <- join_all(list(between03,between04), by="names", type="full")

between <- melt(between, id="names")
between$logb <- log(between$value+1)
between$logb2 <- between$logb
between$logb2[between$logb2==0] <- NA
quantile(between$logb2, .5, na.rm=T)
between$color <- 0
between$color[between$names=="report"] <- 1
between$color[between$names=="hous"] <- 1
between$color[between$names=="america"] <- 1
between$color[between$names=="legal"] <- 1
between$color[between$names=="social"] <- 1
between$color[between$names=="american"] <- 1
between$color[between$names=="offici"] <- 1
between$color[between$names=="recent"] <- 1
between$color[between$names=="issu"] <- 1
between$color[between$names=="group"] <- 1
between$color[between$names=="job"] <- 1
between$color[between$names=="million"] <- 1
between$color[between$names=="countri"] <- 1

ggplot(between, aes(variable, logb2, label=names)) +
  geom_boxplot() + geom_point(data=between[which(between$logb2>=0.1312669),], aes(color=ifelse(color==1, "#800000", "#ff8b94"))) +
  geom_label_repel(aes(label=ifelse(between$logb2>=0.1312669,as.character(names),''),
                       color=ifelse(between$color==1, "#800000", "#ff8b94")), hjust=-.5, vjust=0) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.x=element_text(face="bold"), legend.position="none", 
        axis.text.x=element_text(size=12))+
  ylab("Betweenness Centrality (logged)") + xlab("") +
  scale_x_discrete(labels=c("between03"="2001","between04"="2002")) +
  scale_color_discrete(breaks=c(1,0), labels=c("#800000","#ff8b94"))

par(mfrow=c(2,2))
par(mar=c(0,0,1,0))
plot(kkkk.net03, vertex.size=5, edge.width=log(E(kkkk.net03)$weight/2), layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label=V(kkkk.net03)$lab, vertex.label.color="black",
     vertex.label.cex=.9, vertex.label.font=2, main="2003", margins=0)
par(mar=c(0,0,1,0))
plot(kkkk.net04, vertex.size=5, edge.width=log(E(kkkk.net04)$weight/2), layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label=V(kkkk.net04)$lab, vertex.label.color="black", 
     vertex.label.cex=.9, vertex.label.font=2, main="2004", margins=0)
par(mar=c(0,0,1,0))
plot(kkkk.net03.simp, vertex.size=5, edge.width=log(E(kkkk.net03.simp)$weight/2), layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label.color="black",
     vertex.label.cex=.9, vertex.label.font=2, main="2003, Subgraph", margins=0)
par(mar=c(0,0,1,0))
plot(kkkk.net04.simp, vertex.size=5, edge.width=log(E(kkkk.net04.simp)$weight/2), layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label.color="black", 
     vertex.label.cex=.9, vertex.label.font=2, main="2004, Subgraph", margins=0)
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))


par(mfrow=c(1,2))
plot(kkkk.net03, vertex.size=V(kkkk.net03)$eigen*12, edge.width=log(E(kkkk.net03)$weight/2), layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label=NA, main="2003")
plot(kkkk.net04, vertex.size=V(kkkk.net04)$eigen*12, edge.width=log(E(kkkk.net04)$weight/2), layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label=NA, main="2004")
par(mfrow=c(1,1))


#With FF instead
kkkk.terms <- read.csv(file="tdm2.csv", header=T, row.names=1)

kkkk.terms <- prop.table(kkkk.terms)

kkkk.terms <- kkkk.terms*1000

kkkk.terms03 <- kkkk.terms[c(1:6),]
kkkk.terms04 <- kkkk.terms[c(7:18),]

kkkk.terms03 <- t(as.matrix(kkkk.terms03)) %*% as.matrix(kkkk.terms03)
kkkk.terms04 <- t(as.matrix(kkkk.terms04)) %*% as.matrix(kkkk.terms04)
kkkk.terms03 <- kkkk.terms03/6
kkkk.terms04 <- kkkk.terms04/12

#2003
kkkk.net03 <- graph.adjacency(kkkk.terms03, diag=F, mode="undirected", weighted=T)
weight <- E(kkkk.net03)$weight
quantile(weight, .95)
kkkk.terms03 <- ifelse(kkkk.terms03 < 0.03097936, 0, kkkk.terms03)

kkkk.net03 <- graph.adjacency(kkkk.terms03, diag=F, mode="undirected", weighted=T)
isolates03 <- V(kkkk.net03)[degree(kkkk.net03) == 0]
kkkk.net03 <- delete_vertices(kkkk.net03, isolates03)
eigen03 <- eigen_centrality(kkkk.net03, directed=F) 
V(kkkk.net03)$eigen <- eigen03$vector

V(kkkk.net03)$color = V(kkkk.net03)$name
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "immigr", "#ff8b94", "#a8e6cf")
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "american", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "law", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "america", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "group", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "countri", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "report", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "issu", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "million", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "job", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "legal", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "offici", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "hous", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "recent", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "social", "#ff8b94", V(kkkk.net03)$color)

V(kkkk.net03)$lab = NA 
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "immigr", "immigr", NA)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "american", "american", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "law", "law", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "america", "america", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "group", "group", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "countri", "countri", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "report", "report", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "issu", "issu", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "million", "million", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "job", "job", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "legal", "legal", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "offici", "offici", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "hous", "hous", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "recent", "recent", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "social", "social", V(kkkk.net03)$lab)

neighbors(kkkk.net03, V(kkkk.net03)$name=="immigr")

#2004
kkkk.net04 <- graph.adjacency(kkkk.terms04, diag=F, mode="undirected", weighted=T)
weight <- E(kkkk.net04)$weight
quantile(weight, .95)
kkkk.terms04 <- ifelse(kkkk.terms04 < 0.05681064, 0, kkkk.terms04)

kkkk.net04 <- graph.adjacency(kkkk.terms04, diag=F, mode="undirected", weighted=T)
isolates04 <- V(kkkk.net04)[degree(kkkk.net04) == 0]
kkkk.net04 <- delete_vertices(kkkk.net04, isolates04)
eigen04 <- eigen_centrality(kkkk.net04, directed=F) 
V(kkkk.net04)$eigen <- eigen04$vector

V(kkkk.net04)$color = V(kkkk.net04)$name 
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "immigr", "#ff8b94", "#a8e6cf")
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "american", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "law", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "america", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "group", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "countri", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "report", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "issu", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "million", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "job", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "legal", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "offici", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "hous", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "recent", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "social", "#ff8b94", V(kkkk.net04)$color)

V(kkkk.net04)$lab = NA 
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "immigr", "immigr", NA)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "american", "american", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "law", "law", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "america", "america", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "group", "group", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "countri", "countri", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "report", "report", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "issu", "issu", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "million", "million", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "job", "job", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "legal", "legal", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "offici", "offici", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "hous", "hous", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "recent", "recent", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "social", "social", V(kkkk.net04)$lab)

neighbors(kkkk.net04, V(kkkk.net04)$name=="immigr")

#Plotting
neighb03 <- neighbors(kkkk.net03, V(kkkk.net03)$name=="immigr")
kkkk.net03.simp <- induced_subgraph(kkkk.net03, vids=neighb03)
neighb04 <- neighbors(kkkk.net04, V(kkkk.net04)$name=="immigr")
kkkk.net04.simp <- induced_subgraph(kkkk.net04, vids=neighb04)

between03 <- as.data.frame(strength(kkkk.net03, weights=E(kkkk.net03)$weight))
between04 <- as.data.frame(strength(kkkk.net04, weights=E(kkkk.net04)$weight))
colnames(between03) <- "between03"
colnames(between04) <- "between04"
between03$names <- rownames(between03)
between04$names <- rownames(between04)
between <- join_all(list(between03,between04), by="names", type="full")

df <- data.frame(matrix(ncol = 2, nrow = 26))
df[1,1] <- between$value[between$names=="report" & between$variable=="between03"] 
df[2,1] <- between$value[between$names=="hous" & between$variable=="between03"]
df[3,1] <- between$value[between$names=="america" & between$variable=="between03"]
df[4,1] <- between$value[between$names=="legal" & between$variable=="between03"]
df[5,1] <- between$value[between$names=="social" & between$variable=="between03"]
df[6,1] <- between$value[between$names=="american" & between$variable=="between03"]
df[7,1] <- between$value[between$names=="offici" & between$variable=="between03"]
df[8,1] <- between$value[between$names=="recent" & between$variable=="between03"]
df[9,1] <- between$value[between$names=="issu" & between$variable=="between03"]
df[10,1] <- between$value[between$names=="group" & between$variable=="between03"]
df[11,1] <- between$value[between$names=="job" & between$variable=="between03"]
df[12,1] <- between$value[between$names=="million" & between$variable=="between03"]
df[13,1] <- between$value[between$names=="countri" & between$variable=="between03"]
df[14,1] <- between$value[between$names=="immigr" & between$variable=="between03"] 
df[15,1] <- between$value[between$names=="presid" & between$variable=="between03"]
df[16,1] <- between$value[between$names=="time" & between$variable=="between03"]
df[17,1] <- between$value[between$names=="support" & between$variable=="between03"]
df[18,1] <- between$value[between$names=="public" & between$variable=="between03"]
df[19,1] <- between$value[between$names=="number" & between$variable=="between03"]
df[20,1] <- between$value[between$names=="work" & between$variable=="between03"]
df[21,1] <- between$value[between$names=="organ" & between$variable=="between03"]
df[22,1] <- between$value[between$names=="social" & between$variable=="between03"]
df[23,1] <- between$value[between$names=="washington" & between$variable=="between03"]
df[24,1] <- between$value[between$names=="secur" & between$variable=="between03"]
df[25,1] <- between$value[between$names=="foreign" & between$variable=="between03"]
df[26,1] <- between$value[between$names=="parti" & between$variable=="between03"]

df[1,2] <- between$value[between$names=="report" & between$variable=="between04"] 
df[2,2] <- between$value[between$names=="hous" & between$variable=="between04"]
df[3,2] <- between$value[between$names=="america" & between$variable=="between04"]
df[4,2] <- between$value[between$names=="legal" & between$variable=="between04"]
df[5,2] <- between$value[between$names=="social" & between$variable=="between04"]
df[6,2] <- between$value[between$names=="american" & between$variable=="between04"]
df[7,2] <- between$value[between$names=="offici" & between$variable=="between04"]
df[8,2] <- between$value[between$names=="recent" & between$variable=="between04"]
df[9,2] <- between$value[between$names=="issu" & between$variable=="between04"]
df[10,2] <- between$value[between$names=="group" & between$variable=="between04"]
df[11,2] <- between$value[between$names=="job" & between$variable=="between04"]
df[12,2] <- between$value[between$names=="million" & between$variable=="between04"]
df[13,2] <- between$value[between$names=="countri" & between$variable=="between04"]
df[14,2] <- between$value[between$names=="immigr" & between$variable=="between04"] 
df[15,2] <- between$value[between$names=="presid" & between$variable=="between04"]
df[16,2] <- between$value[between$names=="time" & between$variable=="between04"]
df[17,2] <- between$value[between$names=="support" & between$variable=="between04"]
df[18,2] <- between$value[between$names=="public" & between$variable=="between04"]
df[19,2] <- between$value[between$names=="number" & between$variable=="between04"]
df[20,2] <- between$value[between$names=="work" & between$variable=="between04"]
df[21,2] <- between$value[between$names=="organ" & between$variable=="between04"]
df[22,2] <- between$value[between$names=="social" & between$variable=="between04"]
df[23,2] <- between$value[between$names=="washington" & between$variable=="between04"]
df[24,2] <- between$value[between$names=="secur" & between$variable=="between04"]
df[25,2] <- between$value[between$names=="foreign" & between$variable=="between04"]
df[26,2] <- between$value[between$names=="parti" & between$variable=="between04"]

df$logX1 <- log(df$X1)
df$logX2 <- log(df$X2)
quantile(df$X1, .5, na.rm=T)
quantile(df$X2, .5, na.rm=T)
quantile(between$value[between$variable=="between03"], .5, na.rm=T)
quantile(between$value[between$variable=="between04"], .5, na.rm=T)
summary(between$value[between$variable=="between04"])
ecdf(between$value[between$variable=="between03"])(0.3448641)
between$value[between$names=="cultur"]

between <- melt(between, id="names")
between$logb <- log(between$value)
between$color <- 0
between$color[between$names=="report"] <- 1
between$color[between$names=="hous"] <- 1
between$color[between$names=="america"] <- 1
between$color[between$names=="legal"] <- 1
between$color[between$names=="social"] <- 1
between$color[between$names=="american"] <- 1
between$color[between$names=="offici"] <- 1
between$color[between$names=="recent"] <- 1
between$color[between$names=="issu"] <- 1
between$color[between$names=="group"] <- 1
between$color[between$names=="job"] <- 1
between$color[between$names=="million"] <- 1
between$color[between$names=="countri"] <- 1
between$color[between$names=="immigr"] <- 1
between$color[between$names=="presid"] <- 1
between$color[between$names=="time"] <- 1
between$color[between$names=="support"] <- 1
between$color[between$names=="public"] <- 1
between$color[between$names=="number"] <- 1
between$color[between$names=="work"] <- 1
between$color[between$names=="organ"] <- 1
between$color[between$names=="social"] <- 1
between$color[between$names=="washington"] <- 1
between$color[between$names=="secur"] <- 1
between$color[between$names=="foreign"] <- 1
between$color[between$names=="parti"] <- 1
summary(between$logb04)
between$logb03 <- ifelse(between$variable=="between03", between$logb, NA)
between$logb04 <- ifelse(between$variable=="between04", between$logb, NA)
between$between032 <- ifelse(between$variable=="between03", between$variable, NA)
between$between042 <- ifelse(between$variable=="between04", between$variable, NA)
quantile(between$logb03, .5, na.rm=T)
quantile(between$logb04, .5, na.rm=T)

box03 <- ggplot(between, aes(1, logb03, label=names)) +
  geom_boxplot() + geom_point(data=between[which(between$logb03>=-1.134125),], aes(color=ifelse(color==1, "#800000", "#ff8b94"))) +
  geom_label_repel(aes(label=ifelse(between$logb03>=-1.134125,as.character(names),''),
                   color=ifelse(between$color==1, "#800000", "#ff8b94"))) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"),
        legend.position="none", axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) +
  ylab("Weighted Degree Centrality (logged)") + xlab("2001") + scale_y_continuous(limits=c(-4,5))

box04 <- ggplot(between, aes(1, logb04, label=names)) +
  geom_boxplot() + geom_point(data=between[which(between$logb04>=-0.6899583),], aes(color=ifelse(color==1, "#800000", "#ff8b94"))) +
  geom_label_repel(aes(label=ifelse(between$logb04>=-0.6899583,as.character(names),''),
                       color=ifelse(between$color==1, "#800000", "#ff8b94"))) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"),
        legend.position="none", axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) +
  ylab("Weighted Degree Centrality (logged)") + xlab("2002") + scale_y_continuous(limits=c(-4,5))

ggarrange(box03, box04)

par(mfrow=c(1,2))
plot(kkkk.net03, vertex.size=3, edge.width=E(kkkk.net03)$weight*10, layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label=V(kkkk.net03)$lab, vertex.label.color="black",
     vertex.label.cex=.9, vertex.label.font=2, margins=-2)
title("2001", line=-3)
plot(kkkk.net04, vertex.size=3, edge.width=E(kkkk.net04)$weight*10, layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label=V(kkkk.net04)$lab, vertex.label.color="black", 
     vertex.label.cex=.9, vertex.label.font=2, margins=-2)
title("2002", line=-3)
par(mfrow=c(1,1))
plot(kkkk.net04.simp, vertex.size=3, edge.width=E(kkkk.net04)$weight*50, layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label.color="black", 
     vertex.label.cex=.9, vertex.label.font=2, main="2002, Subgraph")

#Bar plots #1
kkkk.terms <- read.csv(file="tdm.csv", header=T, row.names=1)

kkkk.terms03 <- kkkk.terms[c(1:3),]
kkkk.terms04 <- kkkk.terms[c(4:6),]

sums03 <- colSums(kkkk.terms03)
kkkk.terms03 <- rbind(kkkk.terms03, sums03)
sums04 <- colSums(kkkk.terms04)
kkkk.terms04 <- rbind(kkkk.terms04, sums04)

kkkk.terms03 <- kkkk.terms03[4,]
rownames(kkkk.terms03) <- "sum03"
kkkk.terms04 <- kkkk.terms04[4,]
rownames(kkkk.terms04) <- "sum04"

kkkk.terms03 <- prop.table(kkkk.terms03)
kkkk.terms04 <- prop.table(kkkk.terms04)

imm.terms <- c("immigr", "american", "law", "america", "group", "countri", "report", 
               "issu", "million", "job", "legal", "offici", "hous", "recent", "social")
imm.terms03 <- kkkk.terms03[imm.terms]
imm.terms04 <- kkkk.terms04[imm.terms]
imm.terms <- rbind(imm.terms03, imm.terms04)
imm.terms$imsums <- rowSums(imm.terms)

black.terms <- c("white", "peopl", "black", "media", "jewish", "like", "just", 
                 "book", "even", "year", "speech", "realli", "racial", "ago", "murder","hate")
black.terms03 <- kkkk.terms03[black.terms]
black.terms04 <- kkkk.terms04[black.terms]
black.terms <- rbind(black.terms03, black.terms04)
black.terms$blsums <- rowSums(black.terms)

govern.terms <- c("one", "govern", "nation", "new", "war", "now", "polit", 
                  "use", "mani", "make", "liber", "along", "interest", "articl", "well","around","seem")
govern.terms03 <- kkkk.terms03[govern.terms]
govern.terms04 <- kkkk.terms04[govern.terms]
govern.terms <- rbind(govern.terms03, govern.terms04)
govern.terms$gvsums <- rowSums(govern.terms)

sstate.terms <- c("state", "southern", "south", "right", "constitut", "school", "like", 
                  "back", "two", "feder", "event", "held", "donat", "pleas", "north","meet","movement")
sstate.terms03 <- kkkk.terms03[sstate.terms]
sstate.terms04 <- kkkk.terms04[sstate.terms]
sstate.terms <- rbind(sstate.terms03, sstate.terms04)
sstate.terms$sssums <- rowSums(sstate.terms)

god.terms <- c("god", "christian", "word", "peopl", "church", "time", "man", 
               "law", "upon", "one", "refer", "mind", "father", "truth")
god.terms03 <- kkkk.terms03[god.terms]
god.terms04 <- kkkk.terms04[god.terms]
god.terms <- rbind(god.terms03, god.terms04)
god.terms$gdsums <- rowSums(god.terms)

prop.mat <- cbind(imm.terms$imsums, black.terms$blsums, govern.terms$gvsums, sstate.terms$sssums,
                  god.terms$gdsums)
rownames(prop.mat) <- rownames(imm.terms)
colnames(prop.mat) <- c("immsums","blsums","gvsums","sssums","gdsums")

prop.mat <- melt(prop.mat, id=rownames(prop.mat))

ggplot(prop.mat, aes(x=Var2, y=value, fill=as.factor(Var1))) + 
  geom_bar(position="dodge", color="black", stat="identity") +
  ylab("Total Proportion of Words") + xlab("The Knights Party") +
  ylim(0, .09) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.x=element_text(face="bold"), axis.text.x=element_text(size=8)) +
  scale_x_discrete(limits=c("immsums","blsums","gvsums","sssums","gdsums"),
                   labels=c("BI","ABPM","AGNWO","SSRAS","CIR")) +
  scale_fill_manual(name=expression(paste(bold("Years"))), 
                    labels=c("2003","2004"),
                    values=c("#dcedc1","#ffd3b6"))

#Bart plots #2
kkkk.terms <- read.csv(file="tdm2.csv", header=T, row.names=1)

kkkk.terms03 <- kkkk.terms[c(1:6),]
kkkk.terms04 <- kkkk.terms[c(7:18),]

sums03 <- colSums(kkkk.terms03)
kkkk.terms03 <- rbind(kkkk.terms03, sums03)
sums04 <- colSums(kkkk.terms04)
kkkk.terms04 <- rbind(kkkk.terms04, sums04)

kkkk.terms03 <- kkkk.terms03[7,]
rownames(kkkk.terms03) <- "sum03"
kkkk.terms04 <- kkkk.terms04[13,]
rownames(kkkk.terms04) <- "sum04"

kkkk.terms03 <- prop.table(kkkk.terms03)
kkkk.terms04 <- prop.table(kkkk.terms04)

imm.terms <- c("immigr", "american", "law", "america", "group", "countri", "report", 
               "issu", "million", "job", "legal", "offici", "hous", "recent", "social")
imm.terms03 <- kkkk.terms03[imm.terms]
imm.terms04 <- kkkk.terms04[imm.terms]
imm.terms <- rbind(imm.terms03, imm.terms04)
imm.terms$imsums <- rowSums(imm.terms)

black.terms <- c("white", "peopl", "black", "media", "jewish", "like", "just", 
                 "book", "even", "year", "speech", "realli", "racial", "ago", "murder","hate")
black.terms03 <- kkkk.terms03[black.terms]
black.terms04 <- kkkk.terms04[black.terms]
black.terms <- rbind(black.terms03, black.terms04)
black.terms$blsums <- rowSums(black.terms)

govern.terms <- c("one", "govern", "nation", "new", "war", "now", "polit", 
                  "use", "mani", "make", "liber", "along", "interest", "articl", "well","around","seem")
govern.terms03 <- kkkk.terms03[govern.terms]
govern.terms04 <- kkkk.terms04[govern.terms]
govern.terms <- rbind(govern.terms03, govern.terms04)
govern.terms$gvsums <- rowSums(govern.terms)

sstate.terms <- c("state", "southern", "south", "right", "constitut", "school", "like", 
                  "back", "two", "feder", "event", "held", "donat", "pleas", "north","meet","movement")
sstate.terms03 <- kkkk.terms03[sstate.terms]
sstate.terms04 <- kkkk.terms04[sstate.terms]
sstate.terms <- rbind(sstate.terms03, sstate.terms04)
sstate.terms$sssums <- rowSums(sstate.terms)

god.terms <- c("god", "christian", "word", "peopl", "church", "time", "man", 
               "law", "upon", "one", "refer", "mind", "father", "truth")
god.terms03 <- kkkk.terms03[god.terms]
god.terms04 <- kkkk.terms04[god.terms]
god.terms <- rbind(god.terms03, god.terms04)
god.terms$gdsums <- rowSums(god.terms)

prop.mat <- cbind(imm.terms$imsums, black.terms$blsums, govern.terms$gvsums, sstate.terms$sssums,
                  god.terms$gdsums)
rownames(prop.mat) <- rownames(imm.terms)
colnames(prop.mat) <- c("immsums","blsums","gvsums","sssums","gdsums")

prop.mat <- melt(prop.mat, id=rownames(prop.mat))

p1 <- ggplot(prop.mat, aes(x=Var2, y=value, fill=as.factor(Var1))) + 
  geom_bar(position="dodge", color="black", stat="identity") +
  ylab("Total Proportion of Words") + xlab("The First Freedom") +
  ylim(0, .105) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.x=element_text(face="bold"), axis.text.x=element_text(size=8)) +
  scale_x_discrete(limits=c("immsums","blsums","gvsums","sssums","gdsums"),
                   labels=c("BI","ABPM","AGNWO","SSRAS","CIR")) +
  scale_fill_manual(name=expression(paste(bold("Years"))), 
                    labels=c("2001","2002"),
                    values=c("#dcedc1","#ffd3b6"))

kkkk.terms <- read.csv(file="tdm3.csv", header=T, row.names=1)

kkkk.terms03 <- kkkk.terms[c(1:5),]
kkkk.terms04 <- kkkk.terms[c(6:7),]

sums03 <- colSums(kkkk.terms03)
kkkk.terms03 <- rbind(kkkk.terms03, sums03)
sums04 <- colSums(kkkk.terms04)
kkkk.terms04 <- rbind(kkkk.terms04, sums04)

kkkk.terms03 <- kkkk.terms03[6,]
rownames(kkkk.terms03) <- "sum03"
kkkk.terms04 <- kkkk.terms04[3,]
rownames(kkkk.terms04) <- "sum04"

kkkk.terms03 <- prop.table(kkkk.terms03)
kkkk.terms04 <- prop.table(kkkk.terms04)

imm.terms <- c("immigr", "american", "law", "america", "group", "countri", "report", 
               "issu", "million", "job", "legal", "offici", "hous", "recent", "social")
imm.terms03 <- kkkk.terms03[imm.terms]
imm.terms04 <- kkkk.terms04[imm.terms]
imm.terms <- rbind(imm.terms03, imm.terms04)
imm.terms$imsums <- rowSums(imm.terms)

black.terms <- c("white", "peopl", "black", "media", "jewish", "like", "just", 
                 "book", "even", "year", "speech", "realli", "racial", "ago", "murder","hate")
black.terms03 <- kkkk.terms03[black.terms]
black.terms04 <- kkkk.terms04[black.terms]
black.terms <- rbind(black.terms03, black.terms04)
black.terms$blsums <- rowSums(black.terms)

govern.terms <- c("one", "govern", "nation", "new", "war", "now", "polit", 
                  "use", "mani", "make", "liber", "along", "interest", "articl", "well","around","seem")
govern.terms03 <- kkkk.terms03[govern.terms]
govern.terms04 <- kkkk.terms04[govern.terms]
govern.terms <- rbind(govern.terms03, govern.terms04)
govern.terms$gvsums <- rowSums(govern.terms)

sstate.terms <- c("state", "southern", "south", "right", "constitut", "school", "like", 
                  "back", "two", "feder", "event", "held", "donat", "pleas", "north","meet","movement")
sstate.terms03 <- kkkk.terms03[sstate.terms]
sstate.terms04 <- kkkk.terms04[sstate.terms]
sstate.terms <- rbind(sstate.terms03, sstate.terms04)
sstate.terms$sssums <- rowSums(sstate.terms)

god.terms <- c("god", "christian", "word", "peopl", "church", "time", "man", 
               "law", "upon", "one", "refer", "mind", "father", "truth")
god.terms03 <- kkkk.terms03[god.terms]
god.terms04 <- kkkk.terms04[god.terms]
god.terms <- rbind(god.terms03, god.terms04)
god.terms$gdsums <- rowSums(god.terms)

prop.mat <- cbind(imm.terms$imsums, black.terms$blsums, govern.terms$gvsums, sstate.terms$sssums,
                  god.terms$gdsums)
rownames(prop.mat) <- rownames(imm.terms)
colnames(prop.mat) <- c("immsums","blsums","gvsums","sssums","gdsums")

prop.mat <- melt(prop.mat, id=rownames(prop.mat))

p2 <- ggplot(prop.mat, aes(x=Var2, y=value, fill=as.factor(Var1))) + 
  geom_bar(position="dodge", color="black", stat="identity") +
  ylab("Total Proportion of Words") + xlab("Nationalist Free Press") +
  ylim(0, .105) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.x=element_text(face="bold"), axis.text.x=element_text(size=8)) +
  scale_x_discrete(limits=c("immsums","blsums","gvsums","sssums","gdsums"),
                   labels=c("BI","ABPM","AGNWO","SSRAS","CIR")) +
  scale_fill_manual(name=expression(paste(bold("Years"))), 
                    labels=c("2001","2002"),
                    values=c("#dcedc1","#ffd3b6"))

ggarrange(p1,p2, ncol=2, common.legend=T, legend="bottom")

#With NA
kkkk.terms <- read.csv(file="tdm_na.csv", header=T, row.names=1)

kkkk.terms <- prop.table(kkkk.terms)

kkkk.terms <- kkkk.terms*1000

kkkk.terms03 <- kkkk.terms[c(1:9),]
kkkk.terms04 <- kkkk.terms[c(10:20),]

kkkk.terms03 <- t(as.matrix(kkkk.terms03)) %*% as.matrix(kkkk.terms03)
kkkk.terms04 <- t(as.matrix(kkkk.terms04)) %*% as.matrix(kkkk.terms04)
kkkk.terms03 <- kkkk.terms03/9
kkkk.terms04 <- kkkk.terms04/11

#2003
kkkk.net03 <- graph.adjacency(kkkk.terms03, diag=F, mode="undirected", weighted=T)
weight <- E(kkkk.net03)$weight
quantile(weight, .95)
kkkk.terms03 <- ifelse(kkkk.terms03 < 0.0402896, 0, kkkk.terms03)

kkkk.net03 <- graph.adjacency(kkkk.terms03, diag=F, mode="undirected", weighted=T)
isolates03 <- V(kkkk.net03)[degree(kkkk.net03) == 0]
kkkk.net03 <- delete_vertices(kkkk.net03, isolates03)
eigen03 <- eigen_centrality(kkkk.net03, directed=F) 
V(kkkk.net03)$eigen <- eigen03$vector

V(kkkk.net03)$color = V(kkkk.net03)$name
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "immigr", "#800000", "#a8e6cf")
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "american", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "law", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "america", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "group", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "countri", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "report", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "issu", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "million", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "job", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "legal", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "offici", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "hous", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "recent", "#ff8b94", V(kkkk.net03)$color)
V(kkkk.net03)$color = ifelse(V(kkkk.net03)$name == "social", "#ff8b94", V(kkkk.net03)$color)

V(kkkk.net03)$lab = NA 
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "immigr", "immigr", NA)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "american", "american", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "law", "law", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "america", "america", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "group", "group", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "countri", "countri", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "report", "report", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "issu", "issu", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "million", "million", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "job", "job", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "legal", "legal", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "offici", "offici", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "hous", "hous", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "recent", "recent", V(kkkk.net03)$lab)
V(kkkk.net03)$lab = ifelse(V(kkkk.net03)$name == "social", "social", V(kkkk.net03)$lab)

neighbors(kkkk.net03, V(kkkk.net03)$name=="immigr")

#2004
kkkk.net04 <- graph.adjacency(kkkk.terms04, diag=F, mode="undirected", weighted=T)
weight <- E(kkkk.net04)$weight
quantile(weight, .95)
kkkk.terms04 <- ifelse(kkkk.terms04 < 0.04372235, 0, kkkk.terms04)

kkkk.net04 <- graph.adjacency(kkkk.terms04, diag=F, mode="undirected", weighted=T)
isolates04 <- V(kkkk.net04)[degree(kkkk.net04) == 0]
kkkk.net04 <- delete_vertices(kkkk.net04, isolates04)
eigen04 <- eigen_centrality(kkkk.net04, directed=F) 
V(kkkk.net04)$eigen <- eigen04$vector

V(kkkk.net04)$color = V(kkkk.net04)$name 
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "immigr", "#800000", "#a8e6cf")
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "american", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "law", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "america", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "group", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "countri", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "report", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "issu", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "million", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "job", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "legal", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "offici", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "hous", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "recent", "#ff8b94", V(kkkk.net04)$color)
V(kkkk.net04)$color = ifelse(V(kkkk.net04)$name == "social", "#ff8b94", V(kkkk.net04)$color)

V(kkkk.net04)$lab = NA 
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "immigr", "immigr", NA)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "american", "american", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "law", "law", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "america", "america", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "group", "group", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "countri", "countri", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "report", "report", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "issu", "issu", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "million", "million", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "job", "job", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "legal", "legal", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "offici", "offici", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "hous", "hous", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "recent", "recent", V(kkkk.net04)$lab)
V(kkkk.net04)$lab = ifelse(V(kkkk.net04)$name == "social", "social", V(kkkk.net04)$lab)

neighbors(kkkk.net04, V(kkkk.net04)$name=="immigr")

#Plotting
neighb03 <- neighbors(kkkk.net03, V(kkkk.net03)$name=="immigr")
kkkk.net03.simp <- induced_subgraph(kkkk.net03, vids=neighb03)
neighb04 <- neighbors(kkkk.net04, V(kkkk.net04)$name=="immigr")
kkkk.net04.simp <- induced_subgraph(kkkk.net04, vids=neighb04)

between03 <- as.data.frame(strength(kkkk.net03, weights=E(kkkk.net03)$weight))
between04 <- as.data.frame(strength(kkkk.net04, weights=E(kkkk.net04)$weight))
colnames(between03) <- "between03"
colnames(between04) <- "between04"
between03$names <- rownames(between03)
between04$names <- rownames(between04)
between <- join_all(list(between03,between04), by="names", type="full")

df <- data.frame(matrix(ncol = 2, nrow = 26))
df[1,1] <- between$value[between$names=="report" & between$variable=="between03"] 
df[2,1] <- between$value[between$names=="hous" & between$variable=="between03"]
df[3,1] <- between$value[between$names=="america" & between$variable=="between03"]
df[4,1] <- between$value[between$names=="legal" & between$variable=="between03"]
df[5,1] <- between$value[between$names=="social" & between$variable=="between03"]
df[6,1] <- between$value[between$names=="american" & between$variable=="between03"]
df[7,1] <- between$value[between$names=="offici" & between$variable=="between03"]
df[8,1] <- between$value[between$names=="recent" & between$variable=="between03"]
df[9,1] <- between$value[between$names=="issu" & between$variable=="between03"]
df[10,1] <- between$value[between$names=="group" & between$variable=="between03"]
df[11,1] <- between$value[between$names=="job" & between$variable=="between03"]
df[12,1] <- between$value[between$names=="million" & between$variable=="between03"]
df[13,1] <- between$value[between$names=="countri" & between$variable=="between03"]
df[14,1] <- between$value[between$names=="immigr" & between$variable=="between03"] 
df[15,1] <- between$value[between$names=="presid" & between$variable=="between03"]
df[16,1] <- between$value[between$names=="time" & between$variable=="between03"]
df[17,1] <- between$value[between$names=="support" & between$variable=="between03"]
df[18,1] <- between$value[between$names=="public" & between$variable=="between03"]
df[19,1] <- between$value[between$names=="number" & between$variable=="between03"]
df[20,1] <- between$value[between$names=="work" & between$variable=="between03"]
df[21,1] <- between$value[between$names=="organ" & between$variable=="between03"]
df[22,1] <- between$value[between$names=="social" & between$variable=="between03"]
df[23,1] <- between$value[between$names=="washington" & between$variable=="between03"]
df[24,1] <- between$value[between$names=="secur" & between$variable=="between03"]
df[25,1] <- between$value[between$names=="foreign" & between$variable=="between03"]
df[26,1] <- between$value[between$names=="parti" & between$variable=="between03"]

df[1,2] <- between$value[between$names=="report" & between$variable=="between04"] 
df[2,2] <- between$value[between$names=="hous" & between$variable=="between04"]
df[3,2] <- between$value[between$names=="america" & between$variable=="between04"]
df[4,2] <- between$value[between$names=="legal" & between$variable=="between04"]
df[5,2] <- between$value[between$names=="social" & between$variable=="between04"]
df[6,2] <- between$value[between$names=="american" & between$variable=="between04"]
df[7,2] <- between$value[between$names=="offici" & between$variable=="between04"]
df[8,2] <- between$value[between$names=="recent" & between$variable=="between04"]
df[9,2] <- between$value[between$names=="issu" & between$variable=="between04"]
df[10,2] <- between$value[between$names=="group" & between$variable=="between04"]
df[11,2] <- between$value[between$names=="job" & between$variable=="between04"]
df[12,2] <- between$value[between$names=="million" & between$variable=="between04"]
df[13,2] <- between$value[between$names=="countri" & between$variable=="between04"]
df[14,2] <- between$value[between$names=="immigr" & between$variable=="between04"] 
df[15,2] <- between$value[between$names=="presid" & between$variable=="between04"]
df[16,2] <- between$value[between$names=="time" & between$variable=="between04"]
df[17,2] <- between$value[between$names=="support" & between$variable=="between04"]
df[18,2] <- between$value[between$names=="public" & between$variable=="between04"]
df[19,2] <- between$value[between$names=="number" & between$variable=="between04"]
df[20,2] <- between$value[between$names=="work" & between$variable=="between04"]
df[21,2] <- between$value[between$names=="organ" & between$variable=="between04"]
df[22,2] <- between$value[between$names=="social" & between$variable=="between04"]
df[23,2] <- between$value[between$names=="washington" & between$variable=="between04"]
df[24,2] <- between$value[between$names=="secur" & between$variable=="between04"]
df[25,2] <- between$value[between$names=="foreign" & between$variable=="between04"]
df[26,2] <- between$value[between$names=="parti" & between$variable=="between04"]

df$logX1 <- log(df$X1)
df$logX2 <- log(df$X2)
quantile(df$X1, .5, na.rm=T)
quantile(df$X2, .5, na.rm=T)
quantile(between$value[between$variable=="between03"], .5, na.rm=T)
quantile(between$value[between$variable=="between04"], .5, na.rm=T)
summary(between$value[between$variable=="between04"])
ecdf(between$value[between$variable=="between03"])(0.3448641)
between$value[between$names=="cultur"]

between <- melt(between, id="names")
between$logb <- log(between$value)
between$color <- 0
between$color[between$names=="report"] <- 1
between$color[between$names=="hous"] <- 1
between$color[between$names=="america"] <- 1
between$color[between$names=="legal"] <- 1
between$color[between$names=="social"] <- 1
between$color[between$names=="american"] <- 1
between$color[between$names=="offici"] <- 1
between$color[between$names=="recent"] <- 1
between$color[between$names=="issu"] <- 1
between$color[between$names=="group"] <- 1
between$color[between$names=="job"] <- 1
between$color[between$names=="million"] <- 1
between$color[between$names=="countri"] <- 1
between$color[between$names=="immigr"] <- 1
between$color[between$names=="presid"] <- 1
between$color[between$names=="time"] <- 1
between$color[between$names=="support"] <- 1
between$color[between$names=="public"] <- 1
between$color[between$names=="number"] <- 1
between$color[between$names=="work"] <- 1
between$color[between$names=="organ"] <- 1
between$color[between$names=="social"] <- 1
between$color[between$names=="washington"] <- 1
between$color[between$names=="secur"] <- 1
between$color[between$names=="foreign"] <- 1
between$color[between$names=="parti"] <- 1
summary(between$logb04)
between$logb03 <- ifelse(between$variable=="between03", between$logb, NA)
between$logb04 <- ifelse(between$variable=="between04", between$logb, NA)
between$between032 <- ifelse(between$variable=="between03", between$variable, NA)
between$between042 <- ifelse(between$variable=="between04", between$variable, NA)
quantile(between$logb03, .5, na.rm=T)
quantile(between$logb04, .5, na.rm=T)

box03 <- ggplot(between, aes(1, logb03, label=names)) +
  geom_boxplot() + geom_point(data=between[which(between$logb03>=-1.134125),], aes(color=ifelse(color==1, "#800000", "#ff8b94"))) +
  geom_label_repel(aes(label=ifelse(between$logb03>=-1.134125,as.character(names),''),
                       color=ifelse(between$color==1, "#800000", "#ff8b94"))) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"),
        legend.position="none", axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) +
  ylab("Weighted Degree Centrality (logged)") + xlab("2001") + scale_y_continuous(limits=c(-4,5))

box04 <- ggplot(between, aes(1, logb04, label=names)) +
  geom_boxplot() + geom_point(data=between[which(between$logb04>=-0.6899583),], aes(color=ifelse(color==1, "#800000", "#ff8b94"))) +
  geom_label_repel(aes(label=ifelse(between$logb04>=-0.6899583,as.character(names),''),
                       color=ifelse(between$color==1, "#800000", "#ff8b94"))) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"),
        legend.position="none", axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) +
  ylab("Weighted Degree Centrality (logged)") + xlab("2002") + scale_y_continuous(limits=c(-4,5))

ggarrange(box03, box04)

par(mfrow=c(2,4))
par(mar=c(1,0,1,2))
plot.new()
plot.new()
plot(kkkk.net03, vertex.size=3, edge.width=E(kkkk.net03)$weight*10, layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label=V(kkkk.net03)$lab, vertex.label.color="black",
     vertex.label.cex=.9, vertex.label.font=2, margins=-5)
title("1996", line=0)
plot.new()
plot.new()
plot.new()
plot(kkkk.net04, vertex.size=3, edge.width=E(kkkk.net04)$weight*10, layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label=V(kkkk.net04)$lab, vertex.label.color="black", 
     vertex.label.cex=.9, vertex.label.font=2, margins=-5)
title("1997", line=0)
plot.new()
par(mfrow=c(1,1))
vp1 <- viewport(height=unit(.8, "npc"), width=unit(.5, "npc"), just=c("right","center"))
print(p2, vp=vp1)
par(mar=c(5.1,4.1,4.1,2.1))

layout(matrix(c(0,1,1,0,2,2), nrow = 2, ncol = 3, byrow = TRUE))
par(mar=c(1,0,1,0))
plot(kkkk.net03, vertex.size=3, edge.width=E(kkkk.net03)$weight*10, layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label=V(kkkk.net03)$lab, vertex.label.color="black",
     vertex.label.cex=.9, vertex.label.font=2, margins=-5)
title("1996", line=0)
plot(kkkk.net04, vertex.size=3, edge.width=E(kkkk.net04)$weight*10, layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label=V(kkkk.net04)$lab, vertex.label.color="black", 
     vertex.label.cex=.9, vertex.label.font=2, margins=-5)
title("1997", line=0)
par(mfrow=c(1,1))
vp1 <- viewport(height=unit(.8, "npc"), width=unit(.5, "npc"), just=c("right","center"))
print(p2, vp=vp1) #p2 is from NA bar plots in networks2.R
par(mar=c(5.1,4.1,4.1,2.1))

between03 <- as.data.frame(strength(kkkk.net03, weights=E(kkkk.net03)$weight))
between04 <- as.data.frame(strength(kkkk.net04, weights=E(kkkk.net04)$weight))
colnames(between03) <- "between03"
colnames(between04) <- "between04"
between03$names <- rownames(between03)
between04$names <- rownames(between04)
between <- join_all(list(between03,between04), by="names", type="full")
between <- melt(between, id="names")

df <- data.frame(matrix(ncol = 2, nrow = 26))
df[1,1] <- between$value[between$names=="report" & between$variable=="between03"] 
df[2,1] <- between$value[between$names=="hous" & between$variable=="between03"]
df[3,1] <- between$value[between$names=="america" & between$variable=="between03"]
df[4,1] <- between$value[between$names=="legal" & between$variable=="between03"]
df[5,1] <- between$value[between$names=="social" & between$variable=="between03"]
df[6,1] <- between$value[between$names=="american" & between$variable=="between03"]
df[7,1] <- between$value[between$names=="offici" & between$variable=="between03"]
df[8,1] <- between$value[between$names=="recent" & between$variable=="between03"]
df[9,1] <- between$value[between$names=="issu" & between$variable=="between03"]
df[10,1] <- between$value[between$names=="group" & between$variable=="between03"]
df[11,1] <- between$value[between$names=="job" & between$variable=="between03"]
df[12,1] <- between$value[between$names=="million" & between$variable=="between03"]
df[13,1] <- between$value[between$names=="countri" & between$variable=="between03"]
df[14,1] <- between$value[between$names=="immigr" & between$variable=="between03"] 
df[15,1] <- between$value[between$names=="presid" & between$variable=="between03"]
df[16,1] <- between$value[between$names=="time" & between$variable=="between03"]
df[17,1] <- between$value[between$names=="support" & between$variable=="between03"]
df[18,1] <- between$value[between$names=="public" & between$variable=="between03"]
df[19,1] <- between$value[between$names=="number" & between$variable=="between03"]
df[20,1] <- between$value[between$names=="work" & between$variable=="between03"]
df[21,1] <- between$value[between$names=="organ" & between$variable=="between03"]
df[22,1] <- between$value[between$names=="social" & between$variable=="between03"]
df[23,1] <- between$value[between$names=="washington" & between$variable=="between03"]
df[24,1] <- between$value[between$names=="secur" & between$variable=="between03"]
df[25,1] <- between$value[between$names=="foreign" & between$variable=="between03"]
df[26,1] <- between$value[between$names=="parti" & between$variable=="between03"]

df[1,2] <- between$value[between$names=="report" & between$variable=="between04"] 
df[2,2] <- between$value[between$names=="hous" & between$variable=="between04"]
df[3,2] <- between$value[between$names=="america" & between$variable=="between04"]
df[4,2] <- between$value[between$names=="legal" & between$variable=="between04"]
df[5,2] <- between$value[between$names=="social" & between$variable=="between04"]
df[6,2] <- between$value[between$names=="american" & between$variable=="between04"]
df[7,2] <- between$value[between$names=="offici" & between$variable=="between04"]
df[8,2] <- between$value[between$names=="recent" & between$variable=="between04"]
df[9,2] <- between$value[between$names=="issu" & between$variable=="between04"]
df[10,2] <- between$value[between$names=="group" & between$variable=="between04"]
df[11,2] <- between$value[between$names=="job" & between$variable=="between04"]
df[12,2] <- between$value[between$names=="million" & between$variable=="between04"]
df[13,2] <- between$value[between$names=="countri" & between$variable=="between04"]
df[14,2] <- between$value[between$names=="immigr" & between$variable=="between04"] 
df[15,2] <- between$value[between$names=="presid" & between$variable=="between04"]
df[16,2] <- between$value[between$names=="time" & between$variable=="between04"]
df[17,2] <- between$value[between$names=="support" & between$variable=="between04"]
df[18,2] <- between$value[between$names=="public" & between$variable=="between04"]
df[19,2] <- between$value[between$names=="number" & between$variable=="between04"]
df[20,2] <- between$value[between$names=="work" & between$variable=="between04"]
df[21,2] <- between$value[between$names=="organ" & between$variable=="between04"]
df[22,2] <- between$value[between$names=="social" & between$variable=="between04"]
df[23,2] <- between$value[between$names=="washington" & between$variable=="between04"]
df[24,2] <- between$value[between$names=="secur" & between$variable=="between04"]
df[25,2] <- between$value[between$names=="foreign" & between$variable=="between04"]
df[26,2] <- between$value[between$names=="parti" & between$variable=="between04"]

df$logX1 <- log(df$X1)
df$logX2 <- log(df$X2)
quantile(df$X1, .5, na.rm=T)
quantile(df$X2, .5, na.rm=T)
quantile(between$value[between$variable=="between03"], .5, na.rm=T)
quantile(between$value[between$variable=="between04"], .5, na.rm=T)
summary(between$value[between$variable=="between04"])
ecdf(between$value[between$variable=="between03"])(0.3448641)



