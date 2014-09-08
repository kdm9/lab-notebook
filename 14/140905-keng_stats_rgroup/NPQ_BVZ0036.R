#install.packages("ggplot2")
library(ggplot2)
library(reshape2)

# 1st data set

npq1st <- read.csv("C:/Users/u4946796/Desktop/RWorkspace/BVZ0036/BVZ0036NPQ1st2cons.csv")


ggplot(npq1st, aes(x=TimePoint, y=NPQ, group=interaction(line, condition),colour=interaction(condition), )) +
       geom_line() + ggtitle("NPQ 1st Time Point")

lines = unique(as.character(npq1st$line))
for (geno in lines) {
  plt = ggplot(npq1st[npq1st$line == geno, ], aes(x=TimePoint, y=NPQ, group=condition, colour=condition)) +
    geom_line() +
    #geom_errorbar(aes(ymin=meanNPQ-sdNPQ,ymax=meanNPQ+sdNPQ)) + 
    scale_color_manual(values=c("#0000FF", "#FF0000")) +
    scale_x_continuous(breaks=seq(0,720,60)) +
    scale_y_continuous(limits=c(0,6)) +
    theme_classic() +
    theme(text = element_text(size=18)) +
    ylab("NPQ") + 
    xlab("Seconds") + 
    ggtitle(geno)
  png(paste0("C:/Users/u4946796/Desktop/RWorkspace/plots/npq1st/", geno, ".png"), width=700, height=500)
  print(plt)
  dev.off()
}

# 2nd data set

npq2nd <- read.csv("C:/Users/u4946796/Desktop/RWorkspace/BVZ0036/BVZ0036NPQ2nd2cons.csv")


ggplot(npq2nd, aes(x=TimePoint, y=NPQ, group=interaction(line, condition),colour=interaction(condition), )) +
  geom_line() + ggtitle("NPQ 2nd Time Point")

# 3rd data set

npq3rd <- read.csv("C:/Users/u4946796/Desktop/RWorkspace/BVZ0036/BVZ0036NPQ3rd2cons.csv")


ggplot(npq3rd, aes(x=TimePoint, y=NPQ, group=interaction(line, condition),colour=interaction(condition), )) +
  geom_line() + ggtitle("NPQ 3rd Time Point")

# 4th data set

npq4th <- read.csv("C:/Users/u4946796/Desktop/RWorkspace/BVZ0036/BVZ0036NPQ4th2cons.csv")


ggplot(npq4th, aes(x=TimePoint, y=NPQ, group=interaction(line, condition),colour=interaction(condition), )) +
  geom_line() + ggtitle("NPQ 4th Time Point")





