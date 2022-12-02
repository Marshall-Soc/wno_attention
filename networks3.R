library(ggnetwork)
library(ggplot2)
library(igraph)
library(ggpubr)

test <- ggnetwork(kkkk.net03, cell.jitter=.75, weights="weight")
test2 <- ggnetwork(kkkk.net04, cell.jitter=.75, weights="weight")

set.seed(123)
net1 <- ggplot(test, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.05, aes(color=color)) +
  geom_nodes(aes(x, y, color=color), size = 3) +
  geom_nodelabel_repel(aes(label = lab),
                       box.padding = unit(1, "lines")) +
  theme_blank() +
  theme(legend.position="none", plot.title=element_text(face="bold", hjust=.5)) +
  labs(title="2001")

set.seed(123)
net2 <- ggplot(test2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.05, aes(color=color)) +
  geom_nodes(aes(x, y, color=color), size = 3) +
  geom_nodelabel_repel(aes(label = lab),
                       box.padding = unit(1, "lines")) +
  theme_blank() +
  theme(legend.position="none", plot.title=element_text(face="bold", hjust=.5)) +
  labs(title="2002")

set.seed(123)
net1 <- ggplot(test[which(test$lab!="NA"),], aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.2, aes(color=color)) +
  geom_nodes(aes(x, y, color=color), size = 3) +
  geom_nodelabel_repel(aes(label = lab),
                       box.padding = unit(1, "lines")) +
  theme_blank() +
  theme(legend.position="none", plot.title=element_text(face="bold", hjust=.5)) +
  labs(title="2001")

set.seed(123)
net2 <- ggplot(test2[which(test2$lab!="NA"),], aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.2, aes(color=color)) +
  geom_nodes(aes(x, y, color=color), size = 3) +
  geom_nodelabel_repel(aes(label = lab),
                       box.padding = unit(1, "lines")) +
  theme_blank() +
  theme(legend.position="none", plot.title=element_text(face="bold", hjust=.5)) +
  labs(title="2002")

set.seed(123)
ggplot(test, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.05, aes(color=color)) +
  geom_nodes(aes(x, y, color=color), size=3) +
  geom_nodelabel_repel(aes(label = lab), segment.color="white",
                       box.padding = unit(1, "lines")) +
  labs(title="2001") +
  theme_blank() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey25"),
        panel.grid = element_blank(),
        legend.position="none", 
        plot.title=element_text(face="bold", hjust=.5))

set.seed(123)
ggplot(test2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.05, aes(color=color)) +
  geom_nodes(aes(x, y, color=color), size=3) +
  geom_nodelabel_repel(aes(label = lab),
                     box.padding = unit(1, "lines")) +
  labs(title="2002") +
  theme_blank() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey25"),
        panel.grid = element_blank(),
        legend.position="none", 
        plot.title=element_text(face="bold", hjust=.5))
