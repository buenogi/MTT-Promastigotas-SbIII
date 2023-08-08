################################################################################
#################################### Viability #################################
################################################################################

# Packages

library(dplyr)
library(rstatix)

# Loading data 

DataMTT <-  read.csv(file = "Data/Processed/DataMTT", header = TRUE, sep =",")

# Outliers identification

outliers_multiple <- DataMTT%>%
  group_by(pop,experiment) %>%
  identify_outliers(values)

outliers <- c(39)

DataMTT <- DataMTT[-outliers,]

# Spliting data

EXP_piloto <- filter(DataMTT, experiment == "EXP_piloto")
EXP1 <- filter(DataMTT, experiment == "EXP1")
EXP2 <- filter(DataMTT, experiment == "EXP2")
EXP3 <- filter(DataMTT, experiment == "EXP3")

# Experiment piloto

# Calculating control's mean values to viability estimative

CNTRL_value <- EXP_piloto%>% 
  filter(conc == "0")%>%
  group_by(pop)%>%
  summarise(Cntrl_values = mean(values))

EXP_piloto <- inner_join(CNTRL_value, EXP_piloto, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP_piloto)){
  viability_est <- EXP_piloto$values[i]*100/EXP_piloto$Cntrl_values[i]
  viability[i] <- viability_est }

EXP_piloto$viability <- viability

# Experiment 1

# Calculating control's mean values to viability estimative

CNTRL_value <- EXP1%>% 
  filter(conc == "0")%>%
  group_by(pop)%>%
  summarise(Cntrl_values = mean(values))

EXP1 <- inner_join(CNTRL_value, EXP1, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP1)){
  viability_est <- EXP1$values[i]*100/EXP1$Cntrl_values[i]
  viability[i] <- viability_est }

EXP1$viability <- viability

# Experiment 2

# Calculating control's mean values to viability estimative

CNTRL_value <- EXP2%>% 
  filter(conc == "0")%>%
  group_by(pop)%>%
  summarise(Cntrl_values = mean(values))

EXP2 <- inner_join(CNTRL_value, EXP2, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP2)){
  viability_est <- EXP2$values[i]*100/EXP2$Cntrl_values[i]
  viability[i] <- viability_est }

EXP2$viability <- viability


# Experiment 3

# Calculating control's mean values to viability estimative

CNTRL_value <- EXP3%>% 
  filter(conc == "0")%>%
  group_by(pop)%>%
  summarise(Cntrl_values = mean(values))

EXP3 <- inner_join(CNTRL_value, EXP3, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP3)){
  viability_est <- EXP3$values[i]*100/EXP3$Cntrl_values[i]
  viability[i] <- viability_est }

EXP3$viability <- viability


# Binding data

DataMTT_Full  <- rbind(EXP_piloto,EXP1, EXP2, EXP3)


# Checking  and substituing oustanding values imputing mean values

# MeanValues

mean_values <- DataMTT_Full%>%
  group_by(pop,conc)%>%
  summarise(mean_value = mean(viability))

mean_values$conc <- as.factor(mean_values$conc)

DataMTT_Full$conc <- as.factor(DataMTT_Full$conc)
DataMTT_Full <- left_join( DataMTT_Full,mean_values, by = c("pop", "conc"))

# for (i in 1:nrow(DataMTT_Full)){
#   print(DataMTT_Full$viability[i])
#   if (DataMTT_Full$viability[i] > 100){
#     DataMTT_Full$viability[i] <- DataMTT_Full$mean_value[i]
#   }}


# CSV exportation

write.csv(DataMTT_Full , file = "Data/Processed/DataMTT_viability.csv", 
          sep = ",", row.names = F)

