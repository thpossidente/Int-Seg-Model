library(ggplot2)
data <- read.csv("Python Scripts/Thesis Supervised Network Outcomes - Regression (2).csv")
data <- data[,c(2,4,5,6)]
data$Noise <- as.factor(data$Noise)

data <- data[!(data$Noise == 0.5),]

ggplot(data, aes(x=Loss.5, y=Percent.of.Maximum.MI.per.RF, color=Noise)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  facet_grid(. ~ Which.Loss)+
  labs(x = "Loss", y = "Percent of Maximum MI per RF")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


