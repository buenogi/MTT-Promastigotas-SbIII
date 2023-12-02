################################################################################
################ SbIII promastigote Dose-response analysis #####################
################################################################################

library(dplyr)
library(ggplot2) 
library(drc)
library(gridExtra)
# Loading data 

MTT_SbIII<- read.csv(file = "Data/Processed/DataMTT_processed_normalized.csv", 
                     header = TRUE, sep = ,)

# Checking data
head(MTT_SbIII)
sapply(MTT_SbIII, class)
MTT_SbIII$experiment <- as.factor(MTT_SbIII$experiment)
sapply(MTT_SbIII, class)

#Sumarizing 
#MTT_SbIII_SUM <- MTT_SbIII%>%
#group_by(conc,pop)%>%
#summarise(mean_value = mean(viability_res), sd_value = sd(viability_res))

MTT_SbIII_SUM <- rename(MTT_SbIII, 
                        #viability_res = "mean_value", 
                        conc = "conc", pop = "pop", 
                        #sd_value = "sd_value", 
                        experiment = "experiment")

# Adding mean values

sum_values <- MTT_SbIII%>%
  group_by(conc,pop)%>%
  summarise(mean_value_res = mean(viability_normalized), 
            sd_value_res = sd(viability_normalized))

MTT_SbIII_SUM <- full_join(MTT_SbIII_SUM, sum_values, by = c("conc", "pop"))


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
C76 <- filter(MTT_SbIII_SUM, pop == "C76")
C67p <- filter(MTT_SbIII_SUM, pop == "C67p")
C68 <- filter(MTT_SbIII_SUM, pop == "C68")


# Adjusting the models

REF_SbIII.LL4 <- drm(viability_normalized ~ conc, data = REF , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(REF_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")
summary(REF_SbIII.LL4)
GSH1_SbIII.LL4 <- drm(viability_normalized~ conc, data = GSH1 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(GSH1_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")
summary(GSH1_SbIII.LL4)
C6_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C6 , fct = LL.4(
  fixed = c(NA,0,100, NA), names = c("b","c", "d", "e")))
plot(C6_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C7_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C7 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C7_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C44_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C44 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C44_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C58_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C58 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C58_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C67_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C67 , fct = LL.4(
  fixed = c(NA,0,100, NA), names = c("b","c", "d", "e")))
plot(C67_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C73_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C73 , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(C73_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C85_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C85 , fct = LL.4(
  fixed = c(NA,0,100, NA), names = c("b","c", "d", "e")))
plot(C85_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C89_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C89 , fct = LL.4(
  fixed = c(NA,0,100, NA), names = c("b","c", "d", "e")))
plot(C89_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C76_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C76 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
plot(C76_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C67p_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C67p , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
plot(C67p_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C68_SbIII.LL4 <- drm(viability_normalized ~ conc, data = C68 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
plot(C68_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
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
ED(C76_SbIII.LL4,50)
ED(C67p_SbIII.LL4,50)
ED(C68_SbIII.LL4,50)

## Exploring predicted model
newdata_REF <- expand.grid(conc = seq(min(REF$conc), max(REF$conc), length.out = 200))
newdata_REF$viability_normalized <- predict(REF_SbIII.LL4, newdata_REF, type = 'response')
newdata_REF$pop <- "REF"
newdata_REF$gene <- "WT"

newdata_GSH1 <- expand.grid(conc = seq(min(GSH1$conc), max(GSH1$conc),
                                       length.out = 200))
newdata_GSH1$viability_normalized <- predict(GSH1_SbIII.LL4, newdata_GSH1, type = 'response')
newdata_GSH1$pop <- "GSH1"
newdata_GSH1$gene <- "GSH1"


newdata_C6 <- expand.grid(conc = seq(min(C6$conc), max(C6$conc),
                                     length.out = 200))
newdata_C6$viability_normalized <- predict(C6_SbIII.LL4, newdata_C6, type = 'response')
newdata_C6$pop <- "C6"
newdata_C6$gene <- "GSH1"
newdata_C7 <- expand.grid(conc = seq(min(C7$conc), max(C7$conc),
                                     length.out = 200))
newdata_C7$viability_normalized <- predict(C7_SbIII.LL4, newdata_C7, type = 'response')
newdata_C7$pop <- "C7"
newdata_C7$gene <- "GSH1"
newdata_C44 <- expand.grid(conc = seq(min(C44$conc), max(C44$conc),
                                      length.out = 200))
newdata_C44$viability_normalized <- predict(C44_SbIII.LL4, newdata_C44, type = 'response')
newdata_C44$pop <- "C44"
newdata_C44$gene <- "GSH1"

newdata_C58 <- expand.grid(conc = seq(min(C58$conc), max(C58$conc),
                                      length.out = 200))
newdata_C58$viability_normalized <- predict(C58_SbIII.LL4, newdata_C58, type = 'response')
newdata_C58$pop <- "C58"
newdata_C58$gene <- "GSH1"

newdata_C67 <- expand.grid(conc = seq(min(C67$conc), max(C67$conc),
                                      length.out = 200))
newdata_C67$viability_normalized <- predict(C67_SbIII.LL4, newdata_C67, type = 'response')
newdata_C67$pop <- "C67"
newdata_C67$gene <- "GSH1"

newdata_C73 <- expand.grid(conc = seq(min(C73$conc), max(C73$conc),
                                      length.out = 200))
newdata_C73$viability_normalized <- predict(C73_SbIII.LL4, newdata_C73, type = 'response')
newdata_C73$pop <- "C73"
newdata_C73$gene <- "GSH1"

newdata_C85 <- expand.grid(conc = seq(min(C85$conc), max(C85$conc),
                                      length.out = 200))
newdata_C85$viability_normalized <- predict(C85_SbIII.LL4, newdata_C85, type = 'response')
newdata_C85$pop <- "C85"
newdata_C85$gene <- "GSH1"

newdata_C89 <- expand.grid(conc = seq(min(C89$conc), max(C89$conc),
                                      length.out = 200))
newdata_C89$viability_normalized <- predict(C89_SbIII.LL4, newdata_C89, type = 'response')
newdata_C89$pop <- "C89"
newdata_C89$gene <- "GSH1"

newdata_C76 <- expand.grid(conc = seq(min(C76$conc), max(C76$conc),
                                      length.out = 200))
newdata_C76$viability_normalized <- predict(C76_SbIII.LL4, newdata_C76, type = 'response')
newdata_C76$pop <- "C76"
newdata_C76$gene <- "PGPA"


newdata_C67p <- expand.grid(conc = seq(min(C67p$conc), max(C67p$conc),
                                       length.out = 200))
newdata_C67p$viability_normalized  <- predict(C67p_SbIII.LL4, newdata_C67p, type = 'response')
newdata_C67p$pop <- "C67p"
newdata_C67p$gene <- "PGPA"

newdata_C68 <- expand.grid(conc = seq(min(C68$conc), max(C68$conc),
                                      length.out = 200))
newdata_C68$viability_normalized <- predict(C68_SbIII.LL4, newdata_C68, type = 'response')
newdata_C68$pop <- "C68"
newdata_C68$gene <- "PGPA"


MTT_SUM_full <- full_join(REF, C6)
MTT_SUM_full <- full_join(MTT_SUM_full, GSH1)
MTT_SUM_full <- full_join(MTT_SUM_full, C7)
MTT_SUM_full <- full_join(MTT_SUM_full, C44)
MTT_SUM_full <- full_join(MTT_SUM_full, C58)
MTT_SUM_full <- full_join(MTT_SUM_full, C67)
MTT_SUM_full <- full_join(MTT_SUM_full, C73)
MTT_SUM_full <- full_join(MTT_SUM_full, C85)
MTT_SUM_full <- full_join(MTT_SUM_full, C89)
MTT_SUM_full <- full_join(MTT_SUM_full, C76)
MTT_SUM_full <- full_join(MTT_SUM_full, C67p)
MTT_SUM_full <- full_join(MTT_SUM_full, C68)

newdata_full <- rbind.data.frame(newdata_REF, newdata_C6)
newdata_full <- rbind(newdata_full, newdata_GSH1)
newdata_full <- rbind(newdata_full, newdata_C7)
newdata_full <- rbind(newdata_full, newdata_C44)
newdata_full <- rbind(newdata_full, newdata_C58)
newdata_full <- rbind(newdata_full, newdata_C67)
newdata_full <- rbind(newdata_full, newdata_C73)
newdata_full <- rbind(newdata_full, newdata_C85)
newdata_full <- rbind(newdata_full, newdata_C89)
newdata_full <- rbind(newdata_full, newdata_C76)
newdata_full <- rbind(newdata_full, newdata_C67p)
newdata_full <- rbind(newdata_full, newdata_C68)

DoseResponseCurves_01 <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_REF, aes(x = log(conc, 10), y = newdata_REF$viability_normalized)) +
  geom_point(data = GSH1, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_GSH1, aes(x = log(conc, 10), y = newdata_GSH1$viability_normalized))+
  geom_point(data = C6, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C6, aes(x = log(conc, 10), y = newdata_C6$viability_normalized))+
  geom_point(data = C7, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C7, aes(x = log(conc, 10), y = newdata_C7$viability_normalized)) +
  geom_point(data = C44, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C44
            , aes(x = log(conc, 10), y = newdata_C44$viability_normalized))+
  geom_point(data = C58, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C58, aes(x = log(conc, 10), y = newdata_C58$viability_normalized))+
  geom_point(data = C67, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C67, aes(x = log(conc, 10), y =  newdata_C67$viability_normalized )) +
  geom_point(data = C73, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C73, aes(x = log(conc, 10), y =newdata_C73$viability_normalized )) +
  geom_point(data = C85, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C85, aes(x = log(conc, 10), y = newdata_C85$viability_normalized))+
  geom_point(data = C89, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C89, aes(x = log(conc, 10), y = newdata_C89$viability_normalized))+
  geom_point(data = C76, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C76, aes(x = log(conc, 10), y = newdata_C76$viability_normalized)) +
  geom_point(data = C67p, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C67p, aes(x = log(conc, 10), y =  newdata_C67p$viability_normalized )) +
  geom_point(data = C68, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C68, aes(x = log(conc, 10), y =newdata_C68$viability_normalized )) +
  ggtitle("Promastigotes dose response to SbIII") +
  labs(x = "Log10 [ ] μM", y = "Viabilidade (%)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)) +
  facet_wrap(~pop, ~gene)

DoseResponseCurves_01

ggsave("Figures/08_DoseResponseCurves_01.png")

# Gráfico definitivo 01 -----------------------
DR_REF <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = viability_normalized), size = 4, alpha = 0.7) +
  geom_line(data = newdata_REF, aes(x = log(conc, 10), y = newdata_REF$viability_normalized), linewidth = 2) +
  geom_errorbar(data = REF, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(8),
                                ymax = mean_value_res + sd_value_res/sqrt(8)),  size = 2,width = 0.1)+
  labs(x = "Log10 [   ] μM", y = "Viabilidade (%)") +
  
  theme_bw() +
  theme(text = element_text(size = 30, face = "bold"))
DR_REF 

ggsave("Figures/DR_REF.png", DR_REF)

# Gráfico definitivo 2 -----------------------

DR_Clones_GSH1 <- ggplot()+
  # geom_point(data = GSH1, aes(x = log(conc, 10), y = viability_normalized)) +
  # geom_line(data = newdata_GSH1, aes(x = log(conc, 10), y = newdata_GSH1$viability_normalized)) +
  # geom_errorbar(data = GSH1, aes(x = log(conc, 10),
  #                                ymin = mean_value_res - sd_value_res/sqrt(4),
  #                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C6, aes(x = log(conc, 10), y = viability_normalized), alpha = 0.7, size = 3) +
  geom_line(data = newdata_C6, aes(x = log(conc, 10), y = newdata_C6$viability_normalized), linewidth = 2) +
  geom_errorbar(data = C6, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C7, aes(x = log(conc, 10), y = viability_normalized),  alpha = 0.7,  size = 3) +
  geom_line(data = newdata_C7, aes(x = log(conc, 10), y = newdata_C7$viability_normalized), linewidth = 2) +
  geom_errorbar(data = C7, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C44, aes(x = log(conc, 10), y = viability_normalized),  alpha = 0.7,  size = 3) +
  geom_line(data = newdata_C44, aes(x = log(conc, 10), y = newdata_C44$viability_normalized), linewidth = 2) +
  geom_errorbar(data = C44, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C58, aes(x = log(conc, 10), y = viability_normalized),  alpha = 0.7,  size = 3) +
  geom_line(data = newdata_C58, aes(x = log(conc, 10), y = newdata_C58$viability_normalized), linewidth = 2) +
  geom_errorbar(data = C58, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C73, aes(x = log(conc, 10), y = viability_normalized), alpha = 0.7,  size = 3) +
  geom_line(data = newdata_C73, aes(x = log(conc, 10), y = newdata_C73$viability_normalized), linewidth = 2) +
  geom_errorbar(data = C73, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C85, aes(x = log(conc, 10), y = viability_normalized),  alpha = 0.7,  size = 3) +
  geom_line(data = newdata_C85, aes(x = log(conc, 10), y = newdata_C85$viability_normalized), linewidth = 2) +
  geom_errorbar(data = C85, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C67, aes(x = log(conc, 10), y = viability_normalized),  alpha = 0.7) +
  geom_line(data = newdata_C67, aes(x = log(conc, 10), y = newdata_C67$viability_normalized), linewidth = 2) +
  geom_errorbar(data = C67, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C89, aes(x = log(conc, 10), y = viability_normalized),  alpha = 0.7,  size = 3) +
  geom_line(data = newdata_C89, aes(x = log(conc, 10), y = newdata_C89$viability_normalized), linewidth = 2) +
  geom_errorbar(data = C89, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
   labs(x = "Log10 [   ] μM", y = "Viabilidade (%)") +
  theme_bw() +
  theme(text = element_text(size = 24, face = "bold"))+
  facet_wrap(~pop, labeller = labeller(pop = c("C6" = "LiGSH1 C6 (+/-)",
                                         "C7" = "LiGSH1 C7 (+/-)",
                                         "C44" = "LiGSH1 C44 (+/-)",
                                         "C58" = "LiGSH1 C58 (+/-)",
                                         "C67" = "LiGSH1 C67 (+/-)",
                                         "C73" = "LiGSH1 C73 (+/-)",
                                         "C85" = "LiGSH1 C85 (+/-)",
                                         "C89" = "LiGSH1 C89 (+/-)")), nrow = 2)


DR_Clones_GSH1

ggsave("Figures/DR_Clones_GSH1.png", DR_Clones_GSH1)



library(patchwork)

DR_Clones_PGPA <- ggplot()+
  geom_point(data = C76, aes(x = log(conc, 10), y = viability_normalized), alpha = 0.7, size = 3) +
  geom_line(data = newdata_C76, aes(x = log(conc, 10), y = newdata_C76$viability_normalized),  linewidth = 2) +
  geom_errorbar(data = C76, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C67p, aes(x = log(conc, 10), y = viability_normalized),alpha = 0.7, size = 3) +
  geom_line(data = newdata_C67p, aes(x = log(conc, 10), y = newdata_C67p$viability_normalized),  linewidth = 2) +
  geom_errorbar(data = C67p, aes(x = log(conc, 10),
                                 ymin = mean_value_res - sd_value_res/sqrt(4),
                                 ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C68, aes(x = log(conc, 10), y = viability_normalized),alpha = 0.7, size = 3) +
  geom_line(data = newdata_C68, aes(x = log(conc, 10), y = newdata_C68$viability_normalized),  linewidth = 2) +
  geom_errorbar(data = C68, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  labs(x = "Log10 [ ] μM", y = "Viabilidade (%)") +
  theme_bw() +
  theme(text = element_text(size = 24, face = "bold"))+
  facet_wrap(~pop, labeller = labeller(pop = c("C76" = "LiPGPA C76 (+/-)",
                                               "C67p" = "LiPGPA C67 (-/-)",
                                               "C68" = "LiPGPA C68 (-/-)")), nrow = 1)
DR_Clones_PGPA


ggsave("Figures/DR_Clones_PGPA.png", DR_Clones_PGPA)

library(patchwork)

DR_promastigotas_plot <- (DR_REF/DR_Clones_GSH1/DR_Clones_PGPA) + plot_annotation(tag_levels = "A")

ggsave(filename = "Figuras/DR_promastigotas_plot.png")  

DoseResponseCurves02_error <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_REF, aes(x = log(conc, 10), y = newdata_REF$viability_normalized)) +
  geom_errorbar(data = REF, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(8),
                                ymax = mean_value_res + sd_value_res/sqrt(8)), width = 0.2) +
  geom_point(data = GSH1, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_GSH1, aes(x = log(conc, 10), y = newdata_GSH1$viability_normalized)) +
  geom_errorbar(data = GSH1, aes(x = log(conc, 10),
                                 ymin = mean_value_res - sd_value_res/sqrt(4),
                                 ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C6, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C6, aes(x = log(conc, 10), y = newdata_C6$viability_normalized)) +
  geom_errorbar(data = C6, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C7, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C7, aes(x = log(conc, 10), y = newdata_C7$viability_normalized)) +
  geom_errorbar(data = C7, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C44, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C44, aes(x = log(conc, 10), y = newdata_C44$viability_normalized)) +
  geom_errorbar(data = C44, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C58, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C58, aes(x = log(conc, 10), y = newdata_C58$viability_normalized)) +
  geom_errorbar(data = C58, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C73, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C73, aes(x = log(conc, 10), y = newdata_C73$viability_normalized)) +
  geom_errorbar(data = C73, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C85, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C85, aes(x = log(conc, 10), y = newdata_C85$viability_normalized)) +
  geom_errorbar(data = C85, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C67, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C67, aes(x = log(conc, 10), y = newdata_C67$viability_normalized)) +
  geom_errorbar(data = C67, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C89, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C89, aes(x = log(conc, 10), y = newdata_C89$viability_normalized)) +
  geom_errorbar(data = C89, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C76, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C76, aes(x = log(conc, 10), y = newdata_C76$viability_normalized)) +
  geom_errorbar(data = C76, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C67p, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C67p, aes(x = log(conc, 10), y = newdata_C67p$viability_normalized)) +
  geom_errorbar(data = C67p, aes(x = log(conc, 10),
                                 ymin = mean_value_res - sd_value_res/sqrt(4),
                                 ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  geom_point(data = C68, aes(x = log(conc, 10), y = viability_normalized)) +
  geom_line(data = newdata_C68, aes(x = log(conc, 10), y = newdata_C68$viability_normalized)) +
  geom_errorbar(data = C68, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.2) +
  ggtitle("Promastigotes dose response to SbIII") +
  labs(x = "Log10 [ ] μM", y = "Viabilidade (%)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  facet_wrap(~pop)

DoseResponseCurves02_error
ggsave("Figures/09_DoseResponseCurves.png")


DoseResponseCurves03_error <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_REF, aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = REF, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(8),
                                ymax = mean_value_res + sd_value_res/sqrt(8)), width = 0.02, alpha = 0.3) +
  geom_point(data = GSH1, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_GSH1, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = GSH1, aes(x = log(conc, 10),
                                 ymin = mean_value_res - sd_value_res/sqrt(4),
                                 ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C6, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C6, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C6, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C7, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C7, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C7, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C44, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C44, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C44, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C58, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C58, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C58, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C73, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C73, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C73, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C85, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C85, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C85, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C67, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C67, aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C67, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C89, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C89, aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = C89, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3) +
  geom_point(data = C68, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C68, aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = C68, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3)+
  geom_point(data = C67p, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C67p, aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = C67p, aes(x = log(conc, 10),
                                 ymin = mean_value_res - sd_value_res/sqrt(4),
                                 ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3)+
  geom_point(data = C76, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C76, aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = C76, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4)), width = 0.02,alpha = 0.3)+
  ggtitle("Promastigotes dose response to SbIII") +
  labs(x = "Log10 [ ] μM", y = "Viability (%)") +
  theme_bw() +
  theme(    plot.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))
#facet_wrap(~pop)

DoseResponseCurves03_error + labs(color = "Populations")

ggsave("Figures/10_DoseResponseCurves.png")


# Extracting measures

COEFICIENTS <- c("Slope", "ED50")
modelREF <- summary(REF_SbIII.LL4)
confintREF <- confint(REF_SbIII.LL4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF", 2)

modelGSH1 <- summary(GSH1_SbIII.LL4)
confintGSH1 <- confint(GSH1_SbIII.LL4)
modelGSH1_coef <- as.data.frame(modelGSH1$coefficients)
modelGSH1_conf <- as.data.frame(confintGSH1)
modelGSH1_SUM <- cbind(modelGSH1_coef, modelGSH1_conf)
modelGSH1_SUM$coef <- COEFICIENTS 
modelGSH1_SUM$pop <- rep("GSH1", 2)

modelC6 <- summary(C6_SbIII.LL4)
confintC6 <- confint(C6_SbIII.LL4)
modelC6_coef <- as.data.frame(modelC6$coefficients)
modelC6_conf <- as.data.frame(confintC6)
modelC6_SUM <- cbind(modelC6_coef, modelC6_conf)
modelC6_SUM$coef <- COEFICIENTS 
modelC6_SUM$pop <- rep("C6", 2)

modelC7 <- summary(C7_SbIII.LL4)
confintC7 <- confint(C7_SbIII.LL4)
modelC7_coef <- as.data.frame(modelC7$coefficients)
modelC7_conf <- as.data.frame(confintC7)
modelC7_SUM <- cbind(modelC7_coef, modelC7_conf)
modelC7_SUM$coef <- COEFICIENTS 
modelC7_SUM$pop <- rep("C7", 2)

modelC44 <- summary(C44_SbIII.LL4)
confintC44 <- confint(C44_SbIII.LL4)
modelC44_coef <- as.data.frame(modelC44$coefficients)
modelC44_conf <- as.data.frame(confintC44)
modelC44_SUM <- cbind(modelC44_coef, modelC44_conf)
modelC44_SUM$coef <- COEFICIENTS 
modelC44_SUM$pop <- rep("C44", 2)

modelC58 <- summary(C58_SbIII.LL4)
confintC58 <- confint(C58_SbIII.LL4)
modelC58_coef <- as.data.frame(modelC58$coefficients)
modelC58_conf <- as.data.frame(confintC58)
modelC58_SUM <- cbind(modelC58_coef, modelC58_conf)
modelC58_SUM$coef <- COEFICIENTS 
modelC58_SUM$pop <- rep("C58", 2)

modelC67 <- summary(C67_SbIII.LL4)
confintC67 <- confint(C67_SbIII.LL4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67", 2)

modelC73 <- summary(C73_SbIII.LL4)
confintC73 <- confint(C73_SbIII.LL4)
modelC73_coef <- as.data.frame(modelC73$coefficients)
modelC73_conf <- as.data.frame(confintC73)
modelC73_SUM <- cbind(modelC73_coef, modelC73_conf)
modelC73_SUM$coef <- COEFICIENTS 
modelC73_SUM$pop <- rep("C73", 2)

modelC85 <- summary(C85_SbIII.LL4)
confintC85 <- confint(C85_SbIII.LL4)
modelC85_coef <- as.data.frame(modelC85$coefficients)
modelC85_conf <- as.data.frame(confintC85)
modelC85_SUM <- cbind(modelC85_coef, modelC85_conf)
modelC85_SUM$coef <- COEFICIENTS 
modelC85_SUM$pop <- rep("C85", 2)

modelC89 <- summary(C89_SbIII.LL4)
confintC89 <- confint(C89_SbIII.LL4)
modelC89_coef <- as.data.frame(modelC89$coefficients)
modelC89_conf <- as.data.frame(confintC89)
modelC89_SUM <- cbind(modelC89_coef, modelC89_conf)
modelC89_SUM$coef <- COEFICIENTS 
modelC89_SUM$pop <- rep("C89", 2)


modelC76 <- summary(C76_SbIII.LL4)
confintC76 <- confint(C76_SbIII.LL4)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76", 2)

modelC67p <- summary(C67p_SbIII.LL4)
confintC67p <- confint(C67p_SbIII.LL4)
modelC67p_coef <- as.data.frame(modelC67p$coefficients)
modelC67p_conf <- as.data.frame(confintC67p)
modelC67p_SUM <- cbind(modelC67p_coef, modelC67p_conf)
modelC67p_SUM$coef <- COEFICIENTS 
modelC67p_SUM$pop <- rep("C67p", 2)

modelC68 <- summary(C68_SbIII.LL4)
confintC68 <- confint(C68_SbIII.LL4)
modelC68_coef <- as.data.frame(modelC68$coefficients)
modelC68_conf <- as.data.frame(confintC68)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68", 2)

summary_DR <- rbind(modelREF_SUM,modelGSH1_SUM,
                    modelC7_SUM, modelC6_SUM, modelC44_SUM,
                    modelC58_SUM, modelC67_SUM, modelC73_SUM,
                    modelC85_SUM, modelC89_SUM, modelC76_SUM, modelC67p_SUM,
                    modelC68_SUM)

write.csv(summary_DR , file = "Docs/summary_DR.csv", row.names = FALSE)

# Models comparisons

m1<-drm(mean_value_res ~ conc, pop, data = MTT_SbIII_SUM, 
        fct =LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
m2<-drm(mean_value_res ~ conc, data = MTT_SbIII_SUM, 
        fct =LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))

anova(m1,m2) 

# There is difference between curves accordingly with populations 
# (m1 IS SIGNIFICANT).

compParm(m1, "ED50")
compParm(m1, "Slope")

# Differences was found between REF and C6, C7, C44, C76, C67 e 68*
# also between C73 C6*, C7** and C44* 

# Ploting meaningfull differences:

DoseResponseCurves04 <- ggplot() +
  geom_point(data = REF, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_REF, size = 2 ,aes(x = log(conc, 10), y = viability_normalized, color = pop, width = 0.5)) +
  geom_errorbar(data = REF, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(8),
                                ymax = mean_value_res + sd_value_res/sqrt(8), color = pop), 
                width = 0.05, alpha = 0.3) +
  geom_point(data = C6, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C6,size = 2 , aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C6, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  geom_point(data = C7, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C7,size = 2 , aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C7, aes(x = log(conc, 10),
                               ymin = mean_value_res - sd_value_res/sqrt(4),
                               ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  geom_point(data = C44, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C44,size = 2 , aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C44, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  geom_point(data = C68, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C68,size = 2 , aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C68, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  geom_point(data = C67p, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C67p,size = 2 , aes(x = log(conc, 10), y = viability_normalized, color = pop)) +
  geom_errorbar(data = C67p, aes(x = log(conc, 10),
                                 ymin = mean_value_res - sd_value_res/sqrt(4),
                                 ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  geom_point(data = C76, aes(x = log(conc, 10), y = mean_value_res, color = pop)) +
  geom_line(data = newdata_C76, size = 2 ,aes(x = log(conc, 10),  y = viability_normalized, color = pop)) +
  geom_errorbar(data = C76, aes(x = log(conc, 10),
                                ymin = mean_value_res - sd_value_res/sqrt(4),
                                ymax = mean_value_res + sd_value_res/sqrt(4), color = pop),
                width = 0.05, alpha = 0.3) +
  ggtitle(             ) +
  labs(x = "Log10 [ ] μM", y = "Viabilidade (%)", color = "Populations") +
  theme_bw()


DoseResponseCurves04 + labs(color = "Populações", size = 25)+
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25,  face = "bold"),
        legend.position = c(0.95,0.8),   
        legend.justification = "right",
        legend.box.background = element_blank())

ggsave("Figures/11_DoseResponseCurves.png")


#  Fit diagnostic
plot1 <- ggplot(REF, aes(x = fitted(REF_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " REF - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(REF, aes(x = fitted(REF_SbIII.LL4), y = residuals(REF_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "REF - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(REF, aes(sample = residuals(REF_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

REF_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
REF_PROM_SbIII
ggsave("Figuras/Fit_REF_PROM_SbIII.png", REF_PROM_SbIII)

plot1 <- ggplot(C6, aes(x = fitted(C6_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C6 - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C6, aes(x = fitted(C6_SbIII.LL4), y = residuals(C6_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C6 - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C6, aes(sample = residuals(C6_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

C6_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C6_PROM_SbIII
ggsave("Figuras/Fit_C6_PROM_SbIII.png", C6_PROM_SbIII)

plot1 <- ggplot(C7, aes(x = fitted(C7_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C7 - LL4 - Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C7, aes(x = fitted(C7_SbIII.LL4), y = residuals(C7_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C7 - LL4 - Resíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C7, aes(sample = residuals(C7_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

C7_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C7_PROM_SbIII
ggsave("Figuras/Fit_C7_PROM_SbIII.png", C7_PROM_SbIII)

plot1 <- ggplot(C44, aes(x = fitted(C44_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C44 - LL4\n Ajustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C44, aes(x = fitted(C44_SbIII.LL4), y = residuals(C44_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C44 - LL4\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C44, aes(sample = residuals(C44_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "C44 QQ - LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

C44_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C44_PROM_SbIII
ggsave("Figuras/Fit_C44_PROM_SbIII.png", C44_PROM_SbIII)

plot1 <- ggplot(C58, aes(x = fitted(C58_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C58 - LL4\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C58, aes(x = fitted(C58_SbIII.LL4), y = residuals(C58_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C58 - LL4\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C58, aes(sample = residuals(C58_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()


C58_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C58_PROM_SbIII
ggsave("Figuras/Fit_C58_PROM_SbIII.png", C58_PROM_SbIII)

plot1 <- ggplot(C67, aes(x = fitted(C67_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C67 - LL4 \nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C67, aes(x = fitted(C67_SbIII.LL4), y = residuals(C67_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C67 - LL4\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C67, aes(sample = residuals(C67_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

C67_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C67_PROM_SbIII
ggsave("Figuras/Fit_C67_PROM_SbIII.png", C67_PROM_SbIII)

plot1 <- ggplot(C73, aes(x = fitted(C73_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C73 - LL4\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C73, aes(x = fitted(C73_SbIII.LL4), y = residuals(C73_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C73 - LL4\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C73, aes(sample = residuals(C73_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

C73_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C73_PROM_SbIII
ggsave("Figuras/Fit_C73_PROM_SbIII.png", C73_PROM_SbIII)

plot1 <- ggplot(C85, aes(x = fitted(C85_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C85 - LL4\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C85, aes(x = fitted(C85_SbIII.LL4), y = residuals(C85_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C85 - LL4\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C85, aes(sample = residuals(C85_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

C85_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C85_PROM_SbIII
ggsave("Figuras/Fit_C85_PROM_SbIII.png", C85_PROM_SbIII)

plot1 <- ggplot(C76, aes(x = fitted(C76_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C76 - LL4\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C76, aes(x = fitted(C76_SbIII.LL4), y = residuals(C76_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C76 - LL4\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C76, aes(sample = residuals(C76_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

C76_PGPA_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C76_PGPA_PROM_SbIII
ggsave("Figuras/Fit_C76_PGPA_PROM_SbIII.png", C76_PGPA_PROM_SbIII)

plot1 <- ggplot(C67p, aes(x = fitted(C67p_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C67 PGPA - LL4\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C67p, aes(x = fitted(C67p_SbIII.LL4), y = residuals(C67p_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C67 PGPA - LL4\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C67p, aes(sample = residuals(C67p_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

C67_PGPA_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C67_PGPA_PROM_SbIII
ggsave("Figuras/Fit_C67_PGPA_PROM_SbIII.png", C67_PGPA_PROM_SbIII)

plot1 <- ggplot(C68, aes(x = fitted(C68_SbIII.LL4), y = viability)) +
  geom_point(pch = 16, cex = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C68 - LL4\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()

plot2 <- ggplot(C68, aes(x = fitted(C68_SbIII.LL4), y = residuals(C68_SbIII.LL4, type = "studentised"))) +
  geom_point(pch = 16, cex = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C68 - LL4\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()

plot3 <- ggplot(C68, aes(sample = residuals(C68_SbIII.LL4, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL4", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()

C68_PGPA_PROM_SbIII <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C68_PGPA_PROM_SbIII 
ggsave("Figuras/Fit_C68_PGPA_PROM_SbIII.png", C68_PGPA_PROM_SbIII )

