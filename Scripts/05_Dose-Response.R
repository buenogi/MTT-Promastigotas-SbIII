################################################################################
################ SbIII promastigote Dose-response analisys #####################
################################################################################

library(dplyr)
library(ggplot2) 
library(drc)

# Loading data 

MTT_SbIII <- read.csv(file = "Data/Processed/DataMTT_processed_normalized.csv", 
                      header = TRUE, sep = ,)

# Checking data
head(MTT_SbIII)
sapply(MTT_SbIII, class)
MTT_SbIII$experiment <- as.factor(MTT_SbIII$experiment)
sapply(MTT_SbIII, class)

#Sumarizing 
MTT_SbIII_SUM <- MTT_SbIII%>%
  group_by(conc,pop)%>%
  summarise(mean_value = mean(viability_res), sd_value = sd(viability_res))

MTT_SbIII_SUM <- rename(MTT_SbIII_SUM, viability_res = "mean_value", 
                        conc = "conc", pop = "pop", sd_value = "sd_value", 
                        experiment = "experiment")


#Splitting populations
REF <- filter(MTT_SbIII_SUM, pop == "REF")
GSH1 <- filter(MTT_SbIII_SUM, pop == "GSH1")
C6 <- filter(MTT_SbIII_SUM, pop == "C6")
C7 <- filter(MTT_SbIII_SUM, pop == "C7")
C44 <- filter(MTT_SbIII_SUM, pop == "C44")
C58 <- filter(MTT_SbIII_SUM, pop == "C58")
C67 <- filter(MTT_SbIII_SUM, pop == "C67")
C73 <- filter(MTT_SbIII_SUM, pop == "C73")
C85 <- filter(MTT_SbIII_SUM, pop == "C85")
C89 <- filter(MTT_SbIII_SUM, pop == "C89")

# Adjusting the models

REF_SbIII.LL4 <- drm(viability_res ~ conc, data = REF , fct = LL.4())
plot(REF_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

GSH1_SbIII.LL4 <- drm(viability_res ~ conc, data = GSH1 , fct = LL.4())
plot(GSH1_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C6_SbIII.LL4 <- drm(viability_res ~ conc, data = C6 , fct = LL.4())
plot(C6_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C7_SbIII.LL4 <- drm(viability_res ~ conc, data = C7 , fct = LL.4())
plot(C7_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C44_SbIII.LL4 <- drm(viability_res ~ conc, data = C44 , fct = LL.4())
plot(C44_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C58_SbIII.LL4 <- drm(viability_res ~ conc, data = C58 , fct = LL.4())
plot(C58_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C67_SbIII.LL4 <- drm(viability_res ~ conc, data = C67 , fct = LL.4())
plot(C67_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C73_SbIII.LL4 <- drm(viability_res ~ conc, data = C73 , fct = LL.4())
plot(C73_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C85_SbIII.LL4 <- drm(viability_res ~ conc, data = C85 , fct = LL.4())
plot(C85_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C89_SbIII.LL4 <- drm(viability_res ~ conc, data = C89 , fct = LL.4())
plot(C89_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

# ED50

ED(REF_SbIII.LL4,50)
ED(GSH1_SbIII.LL4,50)
ED(C6_SbIII.LL4,50)
ED(C7_SbIII.LL4,50)
ED(C44_SbIII.LL4,50)
ED(C58_SbIII.LL4,50)
ED(C67_SbIII.LL4,50)
ED(C73_SbIII.LL4,50)
ED(C85_SbIII.LL4,50)
ED(C89_SbIII.LL4,50)

## Exploring predicted model
newdata_REF <- expand.grid(conc = seq(min(REF$conc), max(REF$conc), length.out = 200))
newdata_REF$mean_value <- predict(REF_SbIII.LL4, newdata_REF, type = 'response')
newdata_REF$pop <- "REF"

newdata_GSH1 <- expand.grid(conc = seq(min(GSH1$conc), max(GSH1$conc),
                                       length.out = 200))
newdata_GSH1$mean_value <- predict(GSH1_SbIII.LL4, newdata_GSH1, type = 'response')
newdata_GSH1$pop <- "GSH1"

newdata_C6 <- expand.grid(conc = seq(min(C6$conc), max(C6$conc),
                                     length.out = 200))
newdata_C6$mean_value <- predict(C6_SbIII.LL4, newdata_C6, type = 'response')
newdata_C6$pop <- "C6"

newdata_C7 <- expand.grid(conc = seq(min(C7$conc), max(C7$conc),
                                     length.out = 200))
newdata_C7$mean_value <- predict(C7_SbIII.LL4, newdata_C7, type = 'response')
newdata_C7$pop <- "C7"

newdata_C44 <- expand.grid(conc = seq(min(C44$conc), max(C44$conc),
                                      length.out = 200))
newdata_C44$mean_value <- predict(C44_SbIII.LL4, newdata_C44, type = 'response')
newdata_C44$pop <- "C44"

newdata_C58 <- expand.grid(conc = seq(min(C58$conc), max(C58$conc),
                                      length.out = 200))
newdata_C58$mean_value <- predict(C58_SbIII.LL4, newdata_C58, type = 'response')
newdata_C58$pop <- "C58"

newdata_C67 <- expand.grid(conc = seq(min(C67$conc), max(C67$conc),
                                      length.out = 200))
newdata_C67$mean_value <- predict(C67_SbIII.LL4, newdata_C67, type = 'response')
newdata_C67$pop <- "C67"

newdata_C73 <- expand.grid(conc = seq(min(C73$conc), max(C73$conc),
                                      length.out = 200))
newdata_C73$mean_value <- predict(C73_SbIII.LL4, newdata_C73, type = 'response')
newdata_C73$pop <- "C73"

newdata_C85 <- expand.grid(conc = seq(min(C85$conc), max(C85$conc),
                                      length.out = 200))
newdata_C85$mean_value <- predict(C85_SbIII.LL4, newdata_C85, type = 'response')
newdata_C85$pop <- "C85"

newdata_C89 <- expand.grid(conc = seq(min(C89$conc), max(C89$conc),
                                      length.out = 200))
newdata_C89$mean_value <- predict(C89_SbIII.LL4, newdata_C89, type = 'response')
newdata_C89$pop <- "C89"

MTT_SUM_full <- full_join(REF, C6)
MTT_SUM_full <- full_join(MTT_SUM_full, GSH1)
MTT_SUM_full <- full_join(MTT_SUM_full, C7)
MTT_SUM_full <- full_join(MTT_SUM_full, C44)
MTT_SUM_full <- full_join(MTT_SUM_full, C58)
MTT_SUM_full <- full_join(MTT_SUM_full, C67)
MTT_SUM_full <- full_join(MTT_SUM_full, C73)
MTT_SUM_full <- full_join(MTT_SUM_full, C85)
MTT_SUM_full <- full_join(MTT_SUM_full, C89)


newdata_full <- rbind.data.frame(newdata_REF, newdata_C6)
newdata_full <- rbind(newdata_full, newdata_GSH1)
newdata_full <- rbind(newdata_full, newdata_C7)
newdata_full <- rbind(newdata_full, newdata_C44)
newdata_full <- rbind(newdata_full, newdata_C58)
newdata_full <- rbind(newdata_full, newdata_C67)
newdata_full <- rbind(newdata_full, newdata_C73)
newdata_full <- rbind(newdata_full, newdata_C85)
newdata_full <- rbind(newdata_full, newdata_C89)


DoseResponseCurves <- ggplot(mapping = aes(x = log(conc,10) , 
                                           y = mean_value)) +
  geom_point(data = REF) +  geom_line(data = newdata_REF) +
  geom_point(data = GSH1) +  geom_line(data = newdata_GSH1) +
  geom_point(data = C6) +  geom_line(data = newdata_C6) +
  geom_point(data = C7) +  geom_line(data = newdata_C7) +
  geom_point(data = C44) +  geom_line(data = newdata_C44) +
  geom_point(data = C58) +  geom_line(data = newdata_C58) +
  geom_point(data = C67) +  geom_line(data = newdata_C67) +
  geom_point(data = C73) +  geom_line(data = newdata_C73) +
  geom_point(data = C85) +  geom_line(data = newdata_C85) +
  geom_point(data = C89) +  geom_line(data = newdata_C89) +
  ggtitle("Promastigotes dose response to SbIII ") +
  labs(x = " Log10 [   ] μM", y = "Viability (%)")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme_bw()

DoseResponseCurves


DoseResponseCurves1 <- ggplot(mapping = aes(x = log(conc,10) , 
                                            y = mean_value, 
                                            color = pop)) +
  geom_line(data = newdata_REF) +
  geom_line(data = newdata_GSH1) +
  geom_line(data = newdata_C6) +
  geom_line(data = newdata_C7) +
  geom_line(data = newdata_C44) +
  geom_line(data = newdata_C58) +
  geom_line(data = newdata_C67) +
  geom_line(data = newdata_C73) +
  geom_line(data = newdata_C85) +
  geom_line(data = newdata_C89) +
  ggtitle("Promastigotes dose response to SbIII") +
  labs(x = " Log10 [   ] μM", y = "Viability (%)")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  theme_bw()

DoseResponseCurves1 + labs(color = "Population")

ggsave("figs/03_DoseResponseCurve1.jpeg", dpi = 300 )

