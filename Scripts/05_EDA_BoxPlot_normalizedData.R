################################################################################
############################### EDA-BoxPlot  ###################################
################################################################################

# Packages

library(dplyr)
library(ggplot2)

# Loading data 

DataMTT <-  read.csv(file = "Data/Processed/DataMTT_processed_normalized.csv",
                     header = TRUE, sep =",")

# Checking data
head(DataMTT)
sapply(DataMTT, class)
DataMTT$pop <- as.factor(DataMTT$pop )
DataMTT$experiment <- as.factor(DataMTT$experiment)
DataMTT$conc<- as.factor(DataMTT$conc)
sapply(DataMTT, class)


MTT_BP_pop <-  ggplot(DataMTT, aes(conc, viability_normalized))+
  geom_boxplot()+
  ggtitle("Box plot promastigotes viability per SbIII dosage") +
  labs(x = " Conc [   ] μM", y = "Viability (%)")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  #facet_grid(MTT_SbIII_48h$pop)+
  facet_wrap(~pop)+
  theme_bw()

MTT_BP_pop +  labs(color = "Populations")

ggsave("Figures/02_MTT_BoxPlot_normalized.png")
