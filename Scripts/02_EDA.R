################################################################################
###################### EDA  - MTT GSH1 HKO clones  #############################
################################################################################

library(dplyr)
library(ggplot2)

#Loading data

DataMTT <- read.csv(file = "Data/Processed/DataMTT")

# Checking data

sapply(DataMTT, class)
DataMTT$pop <- as.factor(DataMTT$pop)
DataMTT$experiment <- as.factor(DataMTT$experiment) 
sapply(DataMTT, class)

# Summarazing

DataMTT_sum <- DataMTT%>%
  group_by(pop,conc)%>%
  summarise(mean_value = mean(values), sd_value = sd(values))

MTT_plot  <- ggplot(DataMTT, aes(x = log(conc),  y = values,  
                                 group = experiment))+
  geom_point(aes(color = experiment))+
  geom_line(aes(color = experiment))+
  ggtitle("Promastigotes viability per SbIII dosage") +
  labs(x = " Conc [   ] μM", y = "Viability (%)")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  #facet_grid(MTT_SbIII_48h$pop)+
  facet_wrap(DataMTT$pop, nrow = 2)+
  theme_bw()

MTT_plot +  labs(color = "Populations")

ggsave("Figures/01_MTT_plot.png")

MTT_plot2  <- ggplot(DataMTT_sum, aes(x = log(conc),  y = mean_value,  group = pop))+
  geom_point(aes(color = pop))+
  geom_line(aes(color = pop))+
  ggtitle("Promastigotes viability per SbIII dosage") +
  labs(x = " Conc [   ] μM", y = "Viability (%)")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  #facet_grid(MTT_SbIII_48h$pop)+
  #facet_wrap(DataMTT$experiment, nrow = 2)+
  theme_bw()

MTT_plot2 +  labs(color = "Populations")
ggsave("Figures/02_MTT_plot.png")

