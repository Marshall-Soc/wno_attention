ggplot(top.terms[which(top.terms$Var2=="topic_1"),], aes(x = rev(order), y = value)) +
  geom_bar(stat = "identity", color = "black", fill = "#F8766D") +
  ylim(0, .02) +
  xlab("") + ylab("") +
  coord_flip() +
  guides(fill = F) +
  theme_bw() +
  scale_x_continuous(breaks = rev(top.terms$order),
                     labels = top.terms$Var1,
                     expand = c(0,0))


test$facet$map_data


