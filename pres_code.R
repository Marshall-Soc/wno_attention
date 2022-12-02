##Dissertation Project Visuals for Job Talk (and for later addition to papers)##
#Marshall A. Taylor
#Last updated: 1-04-18

#Baseline adjusted predictions from ZIP model (predictions from Stata).
margins <- read.csv("new_entrep.csv")

margins <- within(margins, {
  cat <- factor(cat,
                levels=c(1,2),
                labels=c("No E-I Leader",">=1 E-I Leader"))
})

ggplot(data=margins, aes(x=cat, y=est, fill=cat))+
  xlab("") + ylab("Predicted # of Name-Drops") +
  theme_classic()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")+
  scale_fill_manual(values=c("#559e83","#ae5a41"))+
  geom_errorbar(aes(ymin=lci, ymax=uci), width=.05, colour="black")+
  geom_point(aes(fill=cat), size=5, colour="black", pch=21)+
  geom_text(aes(label=cat, vjust=-2))+
  coord_flip()

#Scatter plots for standardized fear and anger variables
pres.data <- read.csv(file="pres_data.csv",row.names=1,header=T)

plot1 <- qplot(factor(org2), stdfear_pres, data=pres.data, geom=c("boxplot","jitter"),
                 main="", ylab="Fear Score (std)", xlab="Mean (unstandardized) = .006; Median (unstandarsized) = .005") +
  scale_y_continuous(limit=c(-2,8)) +
  geom_boxplot(fill="#1b85b8") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(), legend.position="none", 
        axis.text.x=element_blank(), axis.ticks.x=element_blank())
plot2 <- qplot(factor(org2), stdanger_pres, data=pres.data, geom=c("boxplot","jitter"),
      main="", ylab="Anger Score (std)", xlab="Mean (unstandardized) = .013; Median (unstandardized) = .012") +
  scale_y_continuous(limit=c(-2,8)) +
  geom_boxplot(fill="#ae5a41") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(), legend.position="none", 
        axis.text.x=element_blank(), axis.ticks.x=element_blank())

grid.arrange(plot1, plot2, ncol=1)

#Group differences test comparing fear and anger distributions across leaders vs. no leaders.
groups <- read.csv(file="margins_pres.csv")
groups <- within(groups, {
  Emotion <- factor(Emotion,
                levels=c(1,2),
                labels=c("Fear","Anger"))
})

ttest <- ggplot(data = groups, aes(x = Emotion, y = p, ymin = pci1, ymax = pci2, colour = Emotion)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  coord_flip() +
  scale_colour_manual(values = c("#1b85b8", "#ae5a41")) +
  xlab("") + ylab("P-Values") +
  geom_hline(yintercept=.05) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(), axis.text.y=element_blank()) +
  geom_text(aes(label=point), hjust = .5, vjust=-1.5)

df <- data.frame()
null <- ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)

grid.arrange(ttest, null, ncol=2)

#Adjusted predictions from full BE model (predictions from Stata).
margins2 <- read.csv(file="new_est.csv")

margins7 <- ggplot(data=margins2, aes(x=levels, y=m1)) +
  geom_line(aes(color="Fear")) +
  geom_line(data=margins2, aes(x=levels, y=m2, color="Anger")) +
  scale_y_continuous(limits=c(-2, 3)) +
  scale_colour_manual(name="", values=c("Fear"="#1b85b8", "Anger"="#ae5a41")) +  
  xlab("Fear/Anger (Standardized)") + ylab("Predicted PageRank Centrality (Standardized)") +
  ggtitle(">= 1 E-I Leader") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.y=element_text(size=10),
        title=element_text(size=8), legend.text=element_text(size=10))

margins8 <- ggplot(data=margins2, aes(x=levels, y=m3)) +
  geom_line(aes(color="Fear")) +
  geom_line(data=margins2, aes(x=levels, y=m4, color="Anger")) +
  scale_y_continuous(limits=c(-2, 3)) +
  scale_colour_manual(name="", values=c("Fear"="#1b85b8", "Anger"="#ae5a41")) +  
  xlab("Fear/Anger (Standardized)") + ylab("Predicted PageRank Centrality (Standardized)") +
  ggtitle("No E-I Leaders") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.y=element_text(size=10),
        title=element_text(size=8), legend.text=element_text(size=10))

ggarrange(margins7, margins8, ncol=2, common.legend = T,
          legend="bottom", align="hv")

#SPLC groups plot
desc9708 <- read.csv(file="group_desc.csv", header=T)

desc9708 <- melt(desc9708, id="Year")
ggplot(data=desc9708, aes(x=Year, y=value, group=variable, colour=variable)) +
  geom_line(size=1) +
  geom_point(size=2) +
  scale_x_continuous(breaks=seq(1997,2008,2)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.y=element_text(size=10),
        legend.position="bottom",
        legend.title=element_text(face="bold")) +
  xlab("Year") + ylab("Total # of Orgs in U.S.") +
  scale_colour_manual(name="Ideology",breaks=c("KKK","Neo.Nazi","Racist.Skinhead","Identity","Neo.Confederate","White.Nationalist"),
                             labels=c("KKK","Neo-Nazi","Racist Skinhead","Christian Identity","Neo-Confederate",
                             "White Nationalist"),
                        values=c("#1696d2","#fdbf11","#ec008b",
                                 "#55b748","#5c5859","#db2b27")) 

#Comparing sentiment specifications (fit stats from Stata).
sent.compare <- read.csv("sent_compare.csv", header=T)

sent.comp1 <- ggplot(data=sent.compare, aes(x=type, y=deviance, fill=type)) +
  geom_bar(stat="identity", color="black") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(), legend.position="none") +
  xlab("Sentiment Specification") + ylab("Deviance")

sent.comp2 <- ggplot(data=sent.compare, aes(x=type, y=adjr2, fill=type)) +
  geom_bar(stat="identity", color="black") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(), legend.position="none") +
  xlab("Sentiment Specification") + ylab("McFadden R-squared (adj)") 

sent.comp3 <- ggplot(data=sent.compare, aes(x=type, y=aic, fill=type)) +
  geom_bar(stat="identity", color="black") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(), legend.position="none") +
  xlab("Sentiment Specification") + ylab("AIC") 

sent.comp4 <- ggplot(data=sent.compare, aes(x=type, y=bic, fill=type)) +
  geom_bar(stat="identity", color="black") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(), legend.position="none") +
  xlab("Sentiment Specification") + ylab("BIC") 

grid.arrange(sent.comp1, sent.comp2, sent.comp3, sent.comp4, ncol=2)

#Binarizing the "ties only" org-year matrix and exporting it.
tiesonly <- read.csv("new_attmat_tiesonly.csv",header=T,row.names=1)
tiesonly[tiesonly > 0] <- 1
write.csv(tiesonly, file="new_attmat_tonlybinc.csv")

#Bar plot of groups across states.
states <- read.csv(file="states.csv",header=T)
states <- states[rev(order(states$count)),]
states$states <- factor(states$states, levels=states$states)

ggplot(data=states, aes(x=states, y=count)) +
  geom_bar(stat="identity", color="black", fill="#ffa500") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(), legend.position="none",
        axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("") + ylab("Total # of Sampled Organizations") 

#Predictions from full ZIP model (predictions from Stata).
poiss.est <- read.csv("poiss_estimates.csv",header=T)

pd <- position_dodge(0.1)
poiss.margins1 <- ggplot(data=poiss.est, aes(x=level, y=m1, fill=level))+
  geom_bar(stat="identity", colour="black", size=.5, fill="#1b85b8")+
  xlab("Fear Score (Standardized)") + ylab("Predicted # of References for Org with E-I Leader") +
  scale_y_continuous(limits=c(0, 2)) +
  theme_classic()+
  theme(legend.title=element_blank(), legend.position="none")+
  geom_errorbar(aes(ymin=m1-m1e, ymax=m1+m1e), width=.1, position=pd, colour="black") +
  coord_fixed(ratio=2.5)

poiss.margins2 <- ggplot(data=poiss.est, aes(x=levels, y=m2, fill=levels))+
  geom_bar(stat="identity", colour="black", size=.5)+
  xlab("Fear Score (Standardized)") + ylab("Predicted # of References for Org without Well-Known Leader")+
  scale_y_continuous(limits=c(0, 3.5)) +
  theme_classic()+
  theme(legend.title=element_blank(), legend.position="none")+
  geom_errorbar(aes(ymin=m2-m2e, ymax=m2+m2e), width=.1, position=pd, colour="black") 

grid.arrange(poiss.margins1, null, ncol=2)

sent.total <- read.csv(file="new_sentbar.csv",header=T)

sent.total1 <- ggplot(data=sent.total, aes(x=sent, y=mean, fill=sent)) +
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(values=c("#ae5a41","#1b85b8")) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(), legend.position="none") +
  ylab("Mean % of Document") 
