#Main
probs <- read.csv(file="probs.csv")

ggplot(data=probs, aes(x=pred, y=im1)) +
  geom_line(aes(linetype="-1 SD Construal Style")) +
  geom_line(aes(x=pred, y=im2, linetype="Mean Construal Style")) +
  geom_line(aes(x=pred, y=im3, linetype="+1 SD Construal Style")) +
  xlab("Ratio of Terror Events per 1,000 \n Non-Hispanic/Latino County Residents (logged)") + 
  ylab('"Borders and Immigration" Grievance Probability') +
  ylim(.1,.3) +
  scale_linetype_manual(name="", values=c("-1 SD Construal Style"="dashed", 
                                          "Mean Construal Style"="dotdash",
                                          "+1 SD Construal Style"="solid"),
                        breaks=c("-1 SD Construal Style", 
                                 "Mean Construal Style",
                                 "+1 SD Construal Style")) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.y=element_text(size=10))

#Margins plots for end of chapter 4
margins1 <- ggplot(data=probs, aes(x=pred, y=im1)) +
  geom_line(aes(linetype="-1 SD Construal Style")) +
  geom_line(aes(x=pred, y=im2, linetype="Mean Construal Style")) +
  geom_line(aes(x=pred, y=im3, linetype="+1 SD Construal Style")) +
  xlab("") + 
  ylab("") +
  ylim(.1,.3) +
  scale_linetype_manual(name="", values=c("-1 SD Construal Style"="dashed", 
                                          "Mean Construal Style"="dotdash",
                                          "+1 SD Construal Style"="solid"),
                        breaks=c("-1 SD Construal Style", 
                                 "Mean Construal Style",
                                 "+1 SD Construal Style")) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.y=element_text(size=10))
probs2 <- read.csv(file="probs2.csv")

margins2 <- ggplot(data=probs2, aes(x=pred, y=im1)) +
  geom_line(aes(linetype="-1 SD Construal Style")) +
  geom_line(aes(x=pred, y=im2, linetype="Mean Construal Style")) +
  geom_line(aes(x=pred, y=im3, linetype="+1 SD Construal Style")) +
  xlab("") + 
  ylab("") +
  ylim(.1,.3) +
  scale_linetype_manual(name="", values=c("-1 SD Construal Style"="dashed", 
                                          "Mean Construal Style"="dotdash",
                                          "+1 SD Construal Style"="solid"),
                        breaks=c("-1 SD Construal Style", 
                                 "Mean Construal Style",
                                 "+1 SD Construal Style")) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
        axis.line=element_line(), panel.border=element_blank(),
        axis.title.y=element_text(size=10))

ch4.margins <- ggarrange(margins1, margins2, ncol=2, align="h", legend="top",
          common.legend=T)
annotate_figure(ch4.margins, 
                left=text_grob('"Borders and Immigration" Grievance Probability', rot=90),
                bottom=text_grob("Ratio of Terror Events per 1,000 Non-Hispanic/Latino County Residents (logged)"))

#3rd marginsplot
probs3 <- read.csv(file="probs3.csv")

ggplot(data=probs3, aes(x=pred, y=im10)) +
  geom_line(aes(linetype="-1 SD Construal Style, before/including 2001", color="-1 SD Construal Style, before/including 2001")) +
  geom_line(aes(x=pred, y=im20, linetype="Mean Construal Style, before/including 2001", color="Mean Construal Style, before/including 2001")) +
  geom_line(aes(x=pred, y=im30, linetype="+1 SD Construal Style, before/including 2001", color="+1 SD Construal Style, before/including 2001")) +
  geom_line(aes(x=pred, y=im11, linetype="-1 SD Construal Style, post-2001", color="-1 SD Construal Style, post-2001")) +
  geom_line(aes(x=pred, y=im21, linetype="Mean Construal Style, post-2001", color="Mean Construal Style, post-2001")) +
  geom_line(aes(x=pred, y=im31, linetype="+1 SD Construal Style, post-2001", color="+1 SD Construal Style, post-2001")) +
  xlab("Ratio of Terror Events per 1,000 \n Non-Hispanic/Latino County Residents (logged)") + 
  ylab('"Borders and Immigration" Grievance Probability') +
  scale_linetype_manual(name="", values=c("-1 SD Construal Style, before/including 2001"="dashed", 
                                          "Mean Construal Style, before/including 2001"="dotdash",
                                          "+1 SD Construal Style, before/including 2001"="solid",
                                          "-1 SD Construal Style, post-2001"="dashed",
                                          "Mean Construal Style, post-2001"="dotdash",
                                          "+1 SD Construal Style, post-2001"="solid"),
                            breaks=c("-1 SD Construal Style, before/including 2001", 
                                     "Mean Construal Style, before/including 2001",
                                     "+1 SD Construal Style, before/including 2001",
                                     "-1 SD Construal Style, post-2001",
                                     "Mean Construal Style, post-2001",
                                     "+1 SD Construal Style, post-2001"),
                        labels=c("-1 SD Construal Style, before/including 2001", 
                                 "Mean Construal Style, before/including 2001",
                                 "+1 SD Construal Style, before/including 2001",
                                 "-1 SD Construal Style, post-2001",
                                 "Mean Construal Style, post-2001",
                                 "+1 SD Construal Style, post-2001")) +
  scale_color_manual(name="", values=c("-1 SD Construal Style, before/including 2001"="#cc181e",
                                       "Mean Construal Style, before/including 2001"="#cc181e",
                                       "+1 SD Construal Style, before/including 2001"="#cc181e",
                                       "-1 SD Construal Style, post-2001"="#2793e8",
                                       "Mean Construal Style, post-2001"="#2793e8",
                                       "+1 SD Construal Style, post-2001"="#2793e8"),
                     breaks=c("-1 SD Construal Style, before/including 2001", 
                              "Mean Construal Style, before/including 2001",
                              "+1 SD Construal Style, before/including 2001",
                              "-1 SD Construal Style, post-2001",
                              "Mean Construal Style, post-2001",
                              "+1 SD Construal Style, post-2001"),
                     labels=c("-1 SD Construal Style, before/including 2001", 
                              "Mean Construal Style, before/including 2001",
                              "+1 SD Construal Style, before/including 2001",
                              "-1 SD Construal Style, post-2001",
                              "Mean Construal Style, post-2001",
                              "+1 SD Construal Style, post-2001")) +
  geom_linerange(data=probs3[which(probs3$pred==0.5),], aes(x=pred, ymin=im10, ymax=im11), color="gray") +
  geom_linerange(data=probs3[which(probs3$pred==0.5),], aes(x=pred, ymin=im20, ymax=im21), color="gray") +
  geom_linerange(data=probs3[which(probs3$pred==0.5),], aes(x=pred, ymin=im30, ymax=im31), color="gray") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
            axis.line=element_line(), panel.border=element_blank(),
            axis.title.y=element_text(size=10)) +
  annotate("point", x=0.5, y=c(0.151890725,0.186897884,0.22999277), shape=4, color="black",
           size=4, stroke=2)

