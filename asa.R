#Line graph
asa <- read.csv(file="asa_2018.csv", header=T)

asa$org <- as.factor(asa$org)

asa <- asa[which(asa$org=="The Knights of the Ku Klux Klan/The Knights Party"),]

ggplot(asa, aes(x=year, y=lo_immigrant_dup)) +
  geom_point(shape=1) +    
  geom_line(color="black") +
  xlab("Year") + ylab("Focus on Borders and Immigration") +
  theme_bw() +
  theme(plot.title=element_text(face="bold", hjust=0.5)) +
  scale_x_continuous(breaks = seq(2002, 2004, by = 1)) +
  ggtitle("The Knights Party")

#Indexicalizing Example
kkkk.terms <- read.csv(file="tdm_tkp2.csv", header=T, row.names=1)

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

par(mfrow=c(1,2))
par(mar=c(0,0,1,0))
plot(kkkk.net03.simp, vertex.size=5, edge.width=log(E(kkkk.net03.simp)$weight/2), layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label.color="black",
     vertex.label.cex=.9, vertex.label.font=2, main="2003, Subgraph", margins=0)
par(mar=c(0,0,1,0))
plot(kkkk.net04.simp, vertex.size=5, edge.width=log(E(kkkk.net04.simp)$weight/2), layout=layout.fruchterman.reingold,
     edge.color="gray", vertex.label.color="black", 
     vertex.label.cex=.9, vertex.label.font=2, main="2004, Subgraph", margins=0)
par(mfrow=c(1,1))

strength(kkkk.net03, weights=E(kkkk.net03)$weight)
strength(kkkk.net04, weights=E(kkkk.net04)$weight)

#Innovating Example
kkkk.terms <- read.csv(file="tdm_tkp.csv", header=T, row.names=1)

kkkk.terms <- prop.table(kkkk.terms)

kkkk.terms <- kkkk.terms*1000

kkkk.terms03 <- kkkk.terms[c(1:3),]
kkkk.terms04 <- kkkk.terms[c(4:6),]

kkkk.terms03 <- t(as.matrix(kkkk.terms03)) %*% as.matrix(kkkk.terms03)
kkkk.terms04 <- t(as.matrix(kkkk.terms04)) %*% as.matrix(kkkk.terms04)

between03 <- as.data.frame(strength(kkkk.net03, weights=E(kkkk.net03)$weight))
between04 <- as.data.frame(strength(kkkk.net04, weights=E(kkkk.net04)$weight))
colnames(between03) <- "between03"
colnames(between04) <- "between04"
between03$names <- rownames(between03)
between04$names <- rownames(between04)
between <- join_all(list(between03,between04), by="names", type="full")
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
between$logb03 <- ifelse(between$variable=="between03", between$logb, NA)
between$logb04 <- ifelse(between$variable=="between04", between$logb, NA)
between$between032 <- ifelse(between$variable=="between03", between$variable, NA)
between$between042 <- ifelse(between$variable=="between04", between$variable, NA)
quantile(between$logb03, .5, na.rm=T)
quantile(between$logb04, .5, na.rm=T)

box03 <- ggplot(between, aes(1, logb03, label=names)) +
  geom_boxplot() + geom_point(data=between[which(between$logb03>=3.288328),], aes(color=ifelse(color==1, "#800000", "#ff8b94"))) + 
  geom_label_repel(aes(label=ifelse(between$logb03>=3.288328,as.character(names),''),
                       color=ifelse(between$color==1, "#800000", "#ff8b94"))) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"),
        legend.position="none", axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) +
  ylab("Weighted Degree Centrality (logged)") + xlab("2002") + scale_y_continuous(limits=c(0,8))

box04 <- ggplot(between, aes(1, logb04, label=names)) +
  geom_boxplot() + geom_point(data=between[which(between$logb04>=3.277309),], aes(color=ifelse(color==1, "#800000", "#ff8b94"))) +
  geom_label_repel(aes(label=ifelse(between$logb04>=3.277309,as.character(names),''),
                       color=ifelse(between$color==1, "#800000", "#ff8b94"))) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"),
        legend.position="none", axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) +
  ylab("Weighted Degree Centrality (logged)") + xlab("2003") + scale_y_continuous(limits=c(0,8))

asa.box <- ggarrange(box03, box04)
annotate_figure(asa.box, text_grob("The Knights Party", face="bold"))

between03 <- as.data.frame(strength(kkkk.net03, weights=E(kkkk.net03)$weight))
between04 <- as.data.frame(strength(kkkk.net04, weights=E(kkkk.net04)$weight))
colnames(between03) <- "between03"
colnames(between04) <- "between04"
between03$names <- rownames(between03)
between04$names <- rownames(between04)
between <- join_all(list(between03,between04), by="names", type="full")
between <- melt(between, id="names")

df <- data.frame(matrix(ncol = 2, nrow = 13))
df[1,1] <- between$value[between$names=="hous" & between$variable=="between03"]
df[2,1] <- between$value[between$names=="america" & between$variable=="between03"]
df[3,1] <- between$value[between$names=="legal" & between$variable=="between03"]
df[4,1] <- between$value[between$names=="social" & between$variable=="between03"]
df[5,1] <- between$value[between$names=="american" & between$variable=="between03"]
df[6,1] <- between$value[between$names=="offici" & between$variable=="between03"]
df[7,1] <- between$value[between$names=="recent" & between$variable=="between03"]
df[8,1] <- between$value[between$names=="issu" & between$variable=="between03"]
df[9,1] <- between$value[between$names=="group" & between$variable=="between03"]
df[10,1] <- between$value[between$names=="job" & between$variable=="between03"]
df[11,1] <- between$value[between$names=="million" & between$variable=="between03"]
df[12,1] <- between$value[between$names=="countri" & between$variable=="between03"]
df[13,1] <- between$value[between$names=="immigr" & between$variable=="between03"] 

df[1,2] <- between$value[between$names=="hous" & between$variable=="between04"]
df[2,2] <- between$value[between$names=="america" & between$variable=="between04"]
df[3,2] <- between$value[between$names=="legal" & between$variable=="between04"]
df[4,2] <- between$value[between$names=="social" & between$variable=="between04"]
df[5,2] <- between$value[between$names=="american" & between$variable=="between04"]
df[6,2] <- between$value[between$names=="offici" & between$variable=="between04"]
df[7,2] <- between$value[between$names=="recent" & between$variable=="between04"]
df[8,2] <- between$value[between$names=="issu" & between$variable=="between04"]
df[9,2] <- between$value[between$names=="group" & between$variable=="between04"]
df[10,2] <- between$value[between$names=="job" & between$variable=="between04"]
df[11,2] <- between$value[between$names=="million" & between$variable=="between04"]
df[12,2] <- between$value[between$names=="countri" & between$variable=="between04"]
df[13,2] <- between$value[between$names=="immigr" & between$variable=="between04"] 

df$logX1 <- log(df$X1)
df$logX2 <- log(df$X2)
quantile(df$X1, .5, na.rm=T)
quantile(df$X2, .5, na.rm=T)
quantile(between$value[between$variable=="between03"], .5, na.rm=T)
quantile(between$value[between$variable=="between04"], .5, na.rm=T)

betweenness(kkkk.net03, directed=F, weights=E(kkkk.net03)$weight, normalized=T)
betweenness(kkkk.net04, directed=F, weights=E(kkkk.net04)$weight, normalized=T)