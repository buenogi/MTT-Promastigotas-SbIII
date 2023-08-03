################################################################################
################ SbIII promastigote Dose-response analisys #####################
################################################################################

library(dplyr)
library(ggplot2) 
library(drc)

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

REF_SbIII.LL4 <- drm(viability_res ~ conc, data = REF , fct = LL.4(
  fixed = c(NA, 0,100, NA), names = c("b","c", "d", "e")))
plot(REF_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

GSH1_SbIII.LL4 <- drm(viability_res~ conc, data = GSH1 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("b","c", "d", "e")))
plot(GSH1_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C6_SbIII.LL4 <- drm(viability_res ~ conc, data = C6 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("b","c", "d", "e")))
plot(C6_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C7_SbIII.LL4 <- drm(viability_res ~ conc, data = C7 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("b","c", "d", "e")))
plot(C7_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C44_SbIII.LL4 <- drm(viability_res ~ conc, data = C44 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("b","c", "d", "e")))
plot(C44_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C58_SbIII.LL4 <- drm(viability_res ~ conc, data = C58 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("b","c", "d", "e")))
plot(C58_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C67_SbIII.LL4 <- drm(viability_res ~ conc, data = C67 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("b","c", "d", "e")))
plot(C67_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C73_SbIII.LL4 <- drm(viability_res ~ conc, data = C73 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("b","c", "d", "e")))
plot(C73_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C85_SbIII.LL4 <- drm(viability_res ~ conc, data = C85 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("b","c", "d", "e")))
plot(C85_SbIII.LL4, xlim = c(0,1000), ylim = c(0,100), 
     broken = TRUE, gridsize = 100, xlab = "SbIII [  ] μM", 
     ylab = "Viability (%)", type = "bars")

C89_SbIII.LL4 <- drm(viability_res ~ conc, data = C89 , fct = LL.4(
  fixed = c(NA, 0, 100, NA), names = c("b","c", "d", "e")))
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

ggsave("Figures/03_DoseResponseCurve1.jpeg", dpi = 300 )


# Extracting measures


COEFICIENTS <- c("b", "c", "d", "e")
modelREF <- summary(REF_SbIII.LL4)
confintREF <- confint(REF_SbIII.LL4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF", 4)

modelGSH1 <- summary(GSH1_SbIII.LL4)
confintGSH1 <- confint(GSH1_SbIII.LL4)
modelGSH1_coef <- as.data.frame(modelGSH1$coefficients)
modelGSH1_conf <- as.data.frame(confintGSH1)
modelGSH1_SUM <- cbind(modelGSH1_coef, modelGSH1_conf)
modelGSH1_SUM$coef <- COEFICIENTS 
modelGSH1_SUM$pop <- rep("GSH1", 4)

modelC6 <- summary(C6_SbIII.LL4)
confintC6 <- confint(C6_SbIII.LL4)
modelC6_coef <- as.data.frame(modelC6$coefficients)
modelC6_conf <- as.data.frame(confintC6)
modelC6_SUM <- cbind(modelC6_coef, modelC6_conf)
modelC6_SUM$coef <- COEFICIENTS 
modelC6_SUM$pop <- rep("C6", 4)

modelC7 <- summary(C7_SbIII.LL4)
confintC7 <- confint(C7_SbIII.LL4)
modelC7_coef <- as.data.frame(modelC7$coefficients)
modelC7_conf <- as.data.frame(confintC7)
modelC7_SUM <- cbind(modelC7_coef, modelC7_conf)
modelC7_SUM$coef <- COEFICIENTS 
modelC7_SUM$pop <- rep("C7", 4)

modelC44 <- summary(C44_SbIII.LL4)
confintC44 <- confint(C44_SbIII.LL4)
modelC44_coef <- as.data.frame(modelC44$coefficients)
modelC44_conf <- as.data.frame(confintC44)
modelC44_SUM <- cbind(modelC44_coef, modelC44_conf)
modelC44_SUM$coef <- COEFICIENTS 
modelC44_SUM$pop <- rep("C44", 4)

modelC58 <- summary(C58_SbIII.LL4)
confintC58 <- confint(C58_SbIII.LL4)
modelC58_coef <- as.data.frame(modelC58$coefficients)
modelC58_conf <- as.data.frame(confintC58)
modelC58_SUM <- cbind(modelC58_coef, modelC58_conf)
modelC58_SUM$coef <- COEFICIENTS 
modelC58_SUM$pop <- rep("C58", 4)

modelC67 <- summary(C67_SbIII.LL4)
confintC67 <- confint(C67_SbIII.LL4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67", 4)

modelC73 <- summary(C73_SbIII.LL4)
confintC73 <- confint(C73_SbIII.LL4)
modelC73_coef <- as.data.frame(modelC73$coefficients)
modelC73_conf <- as.data.frame(confintC73)
modelC73_SUM <- cbind(modelC73_coef, modelC73_conf)
modelC73_SUM$coef <- COEFICIENTS 
modelC73_SUM$pop <- rep("C73", 4)

modelC85 <- summary(C85_SbIII.LL4)
confintC85 <- confint(C85_SbIII.LL4)
modelC85_coef <- as.data.frame(modelC85$coefficients)
modelC85_conf <- as.data.frame(confintC85)
modelC85_SUM <- cbind(modelC85_coef, modelC85_conf)
modelC85_SUM$coef <- COEFICIENTS 
modelC85_SUM$pop <- rep("C85", 4)

modelC89 <- summary(C89_SbIII.LL4)
confintC89 <- confint(C89_SbIII.LL4)
modelC89_coef <- as.data.frame(modelC89$coefficients)
modelC89_conf <- as.data.frame(confintC89)
modelC89_SUM <- cbind(modelC89_coef, modelC89_conf)
modelC89_SUM$coef <- COEFICIENTS 
modelC89_SUM$pop <- rep("C89", 4)

summary_DR <- rbind(modelREF_SUM,modelGSH1_SUM,
                    modelC7_SUM, modelC6_SUM, modelC44_SUM,
                    modelC58_SUM, modelC67_SUM, modelC73_SUM,
                    modelC85_SUM, modelC89_SUM)

write.csv(summary_DR , file = "Docs/summary_DR.csv", row.names = FALSE)


#  Fit diagnostic

par(mfrow=c(1,3), cex.lab=1.2)
plot(REF$viability, fitted(REF_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(REF_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(REF_SbIII.LL4),residuals(REF_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(REF_SbIII.LL4), 
                   residuals(REF_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(REF_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(REF_SbIII.LL4))

png("Figures/03_Fit_diagnostic_REF.png",  width = 6, height = 4, units = "in", res = 300)



par(mfrow=c(1,3), cex.lab=1.2)
plot(C6$viability, fitted(C6_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C6_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C6_SbIII.LL4),residuals(C6_SbIII.LL4, 
                                    typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C6_SbIII.LL4), 
                   residuals(C6_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C6_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C6_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C6.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C7$viability, fitted(C7_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C7_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C7_SbIII.LL4),residuals(C7_SbIII.LL4, 
                                    typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C7_SbIII.LL4), 
                   residuals(C7_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C7_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C7_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C7.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C44$viability, fitted(C44_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C44_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C44_SbIII.LL4),residuals(C44_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C44_SbIII.LL4), 
                   residuals(C44_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C44_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C44_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C44.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C58$viability, fitted(C58_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C58_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C58_SbIII.LL4),residuals(C58_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C58_SbIII.LL4), 
                   residuals(C58_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C58_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C58_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C58.png",  width = 6, height = 4, units = "in", res = 300)



par(mfrow=c(1,3), cex.lab=1.2)
plot(GSH1$viability, fitted(GSH1_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(GSH1_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(GSH1_SbIII.LL4),residuals(GSH1_SbIII.LL4, 
                                      typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(GSH1_SbIII.LL4), 
                   residuals(GSH1_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(GSH1_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(GSH1_SbIII.LL4))

png("Figures/03_Fit_diagnostic_GSH1.png",  width = 6, height = 4, units = "in", res = 300)



par(mfrow=c(1,3), cex.lab=1.2)
plot(C67$viability, fitted(C67_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C67_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C67_SbIII.LL4),residuals(C67_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C67_SbIII.LL4), 
                   residuals(C67_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C67_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C67_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C67.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C73$viability, fitted(C73_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C73_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C73_SbIII.LL4),residuals(C73_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C73_SbIII.LL4), 
                   residuals(C73_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C73_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C73_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C73.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C44$viability, fitted(C44_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C44_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C44_SbIII.LL4),residuals(C44_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C44_SbIII.LL4), 
                   residuals(C44_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C44_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C44_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C44.png",  width = 6, height = 4, units = "in", res = 300)


par(mfrow=c(1,3), cex.lab=1.2)
plot(C85$viability, fitted(C85_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C85_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C85_SbIII.LL4),residuals(C85_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C85_SbIII.LL4), 
                   residuals(C85_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C85_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C85_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C85.png",  width = 6, height = 4, units = "in", res = 300)

par(mfrow=c(1,3), cex.lab=1.2)
plot(C89$viability, fitted(C89_SbIII.LL4), main="LL4", 
     xlab ="Adjusted values", ylab="Observed values", pch=16, cex=.7)
abline(a = 0,b = 1)
residuals(C89_SbIII.LL4, typeRes = c("studentised"))
plot(fitted(C89_SbIII.LL4),residuals(C89_SbIII.LL4, 
                                     typeRes = c("studentised")), 
     main="LL4", xlab="Adjusted values", ylab="Residuals", pch=16, cex=.7)
lines(loess.smooth(fitted(C89_SbIII.LL4), 
                   residuals(C89_SbIII.LL4, typeRes = c("studentised"))))
abline(h=0, lty=2)
qqnorm(residuals(C89_SbIII.LL4), xlab="Theoretical quantiles", 
       ylab="Residuals", main = "LL4")
qqline(residuals(C89_SbIII.LL4))

png("Figures/03_Fit_diagnostic_C89.png",  width = 6, height = 4, units = "in", res = 300)



