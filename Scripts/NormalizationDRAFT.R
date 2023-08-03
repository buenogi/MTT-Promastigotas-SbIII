################################################################################
#################### Normalization and reescaling data #########################
################################################################################

# Packages

library(scales)
library(dplyr)
library(rstatix)

# Loading data 

DataMTT <-  read.csv(file = "Data/Processed/DataMTT", header = TRUE, sep =",")

# Outliers identification

outliers_multiple <- DataMTT%>%
  group_by(pop,experiment) %>%
  identify_outliers(values)

outliers <- c(39,206)

DataMTT <- DataMTT[-outliers,]

# Spliting data

EXP_piloto <- filter(DataMTT, experiment == "EXP_piloto")
EXP1 <- filter(DataMTT, experiment == "EXP_1")
EXP2 <- filter(DataMTT, experiment == "EXP_2")
EXP3 <- filter(DataMTT, experiment == "EXP_3")


# Experiment piloto

# Calculating control's mean values to viability estimative

CNTRL_value <- EXP_piloto%>% 
  group_by(pop)%>%
  summarize(Cntrl_values = max(values))

EXP_piloto <- inner_join(CNTRL_value, EXP_piloto, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP_piloto)){
  viability_est <- EXP_piloto$values[i]*100/EXP_piloto$Cntrl_values[i]
  viability[i] <- viability_est }

EXP_piloto$viability <- viability

# Capping to 100%

EXP_piloto$viability_cap <- viability

for (i in 1:nrow(EXP_piloto)){
  print(EXP_piloto$viability_cap[i])
  if (EXP_piloto$viability_cap[i] > 100){
    EXP_piloto$viability_cap[i] <- 100
  }}


EXP_piloto_REF <- EXP_piloto%>%
  filter(pop == "REF")

EXP_piloto_REF$viability_res <- rescale(EXP_piloto_REF$viability_cap, c(10,100))

EXP_piloto_GSH1 <- EXP_piloto%>%
  filter(pop == "GSH1")

EXP_piloto_GSH1$viability_res <- rescale(EXP_piloto_GSH1$viability_cap, c(10,100))

EXP_piloto_C6 <- EXP_piloto%>%
  filter(pop == "C6")

EXP_piloto_C6$viability_res <- rescale(EXP_piloto_C6$viability_cap, c(10,100))

EXP_piloto_C7 <- EXP_piloto%>%
  filter(pop == "C7")

EXP_piloto_C7$viability_res <- rescale(EXP_piloto_C7$viability_cap, c(10,100))

EXP_piloto_C44 <- EXP_piloto%>%
  filter(pop == "C44")

EXP_piloto_C44$viability_res <- rescale(EXP_piloto_C44$viability_cap, c(10,100))

EXP_piloto_C58 <- EXP_piloto%>%
  filter(pop == "C58")

EXP_piloto_C58$viability_res <- rescale(EXP_piloto_C58$viability_cap, c(10,100))

EXP_piloto_C67 <- EXP_piloto%>%
  filter(pop == "C67")

EXP_piloto_C67$viability_res <- rescale(EXP_piloto_C67$viability_cap, c(10,100))

EXP_piloto_C73 <- EXP_piloto%>%
  filter(pop == "C73")

EXP_piloto_C73$viability_res <- rescale(EXP_piloto_C73$viability_cap, c(10,100))

EXP_piloto_C85 <- EXP_piloto%>%
  filter(pop == "C85")

EXP_piloto_C85$viability_res <- rescale(EXP_piloto_C85$viability_cap, c(10,100))

EXP_piloto_C89 <- EXP_piloto%>%
  filter(pop == "C89")

EXP_piloto_C89$viability_res <- rescale(EXP_piloto_C89$viability_cap, c(10,100))

EXP_piloto <- rbind(EXP_piloto_GSH1,EXP_piloto_REF, EXP_piloto_C6,EXP_piloto_C7,EXP_piloto_C44, EXP_piloto_C58,
                    EXP_piloto_C67,EXP_piloto_C73, EXP_piloto_C85, EXP_piloto_C89 )

# Experiment 1

# Calculating control's mean values to viability estimative

CNTRL_value <- EXP1%>% 
  group_by(pop)%>%
  summarize(Cntrl_values = max(values))

EXP1 <- inner_join(CNTRL_value, EXP1, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP1)){
  viability_est <- EXP1$values[i]*100/EXP1$Cntrl_values[i]
  viability[i] <- viability_est }

EXP1$viability <- viability

# Capping to 100%

EXP1$viability_cap <- viability

for (i in 1:nrow(EXP_piloto)){
  print(EXP1$viability_cap[i])
  if (EXP1$viability_cap[i] > 100){
    EXP1$viability_cap[i] <- 100
  }}


EXP1_REF <- EXP1%>%
  filter(pop == "REF")

EXP1_REF$viability_res <- rescale(EXP1_REF$viability_cap, c(10,100))

EXP1_GSH1 <- EXP1%>%
  filter(pop == "GSH1")

EXP1_GSH1$viability_res <- rescale(EXP1_GSH1$viability_cap, c(10,100))

EXP1_C6 <- EXP1%>%
  filter(pop == "C6")

EXP1_C6$viability_res <- rescale(EXP1_C6$viability_cap, c(10,100))

EXP1_C7 <- EXP1%>%
  filter(pop == "C7")

EXP1_C7$viability_res <- rescale(EXP1_C7$viability_cap, c(10,100))

EXP1_C44 <- EXP1%>%
  filter(pop == "C44")

EXP1_C44$viability_res <- rescale(EXP1_C44$viability_cap, c(10,100))

EXP1_C58 <- EXP1%>%
  filter(pop == "C58")

EXP1_C58$viability_res <- rescale(EXP1_C58$viability_cap, c(10,100))

EXP1_C67 <- EXP1%>%
  filter(pop == "C67")

EXP1_C67$viability_res <- rescale(EXP1_C67$viability_cap, c(10,100))

EXP1_C73 <- EXP1%>%
  filter(pop == "C73")

EXP1_C73$viability_res <- rescale(EXP1_C73$viability_cap, c(10,100))

EXP1_C85 <- EXP1%>%
  filter(pop == "C85")

EXP1_C85$viability_res <- rescale(EXP1_C85$viability_cap, c(10,100))

EXP1_C89 <- EXP1%>%
  filter(pop == "C89")

EXP1_C89$viability_res <- rescale(EXP1_C89$viability_cap, c(10,100))

EXP1 <- rbind(EXP1_GSH1,EXP1_REF, EXP1_C6,EXP1_C7,EXP1_C44, EXP1_C58,
              EXP1_C67,EXP1_C73, EXP1_C85, EXP1_C89 )


# Experiment 2

# Calculating control's mean values to viability estimative

CNTRL_value <- EXP_piloto%>% 
  group_by(pop)%>%
  summarize(Cntrl_values = max(values))

EXP2 <- inner_join(CNTRL_value, EXP2, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP2)){
  viability_est <- EXP2$values[i]*100/EXP2$Cntrl_values[i]
  viability[i] <- viability_est }

EXP2$viability <- viability

# Capping to 100%

EXP2$viability_cap <- viability

for (i in 1:nrow(EXP2)){
  print(EXP2$viability_cap[i])
  if (EXP2$viability_cap[i] > 100){
    EXP2$viability_cap[i] <- 100
  }}




EXP2_REF <- EXP2%>%
  filter(pop == "REF")

EXP2_REF$viability_res <- rescale(EXP2_REF$viability_cap, c(10,100))

EXP2_GSH1 <- EXP2%>%
  filter(pop == "GSH1")

EXP2_GSH1$viability_res <- rescale(EXP2_GSH1$viability_cap, c(10,100))

EXP2_C6 <- EXP2%>%
  filter(pop == "C6")

EXP2_C6$viability_res <- rescale(EXP2_C6$viability_cap, c(10,100))

EXP2_C7 <- EXP2%>%
  filter(pop == "C7")

EXP2_C7$viability_res <- rescale(EXP2_C7$viability_cap, c(10,100))

EXP2_C44 <- EXP2%>%
  filter(pop == "C44")

EXP2_C44$viability_res <- rescale(EXP2_C44$viability_cap, c(10,100))

EXP2_C58 <- EXP2%>%
  filter(pop == "C58")

EXP2_C58$viability_res <- rescale(EXP2_C58$viability_cap, c(10,100))

EXP2_C67 <- EXP2%>%
  filter(pop == "C67")

EXP2_C67$viability_res <- rescale(EXP2_C67$viability_cap, c(10,100))

EXP2_C73 <- EXP2%>%
  filter(pop == "C73")

EXP2_C73$viability_res <- rescale(EXP2_C73$viability_cap, c(10,100))

EXP2_C85 <- EXP2%>%
  filter(pop == "C85")

EXP2_C85$viability_res <- rescale(EXP2_C85$viability_cap, c(10,100))

EXP2_C89 <- EXP2%>%
  filter(pop == "C89")

EXP2_C89$viability_res <- rescale(EXP2_C89$viability_cap, c(10,100))

EXP2 <- rbind(EXP2_GSH1,EXP2_REF, EXP2_C6,EXP2_C7,EXP2_C44, EXP2_C58,
              EXP2_C67,EXP2_C73, EXP2_C85, EXP2_C89 )

# Experiment 3

# Calculating control's mean values to viability estimative

CNTRL_value <- EXP_piloto%>% 
  group_by(pop)%>%
  summarize(Cntrl_values = max(values))

EXP3 <- inner_join(CNTRL_value, EXP3, by = "pop")

# Viability estimatives
viability <- c()
for (i in 1:nrow(EXP3)){
  viability_est <- EXP3$values[i]*100/EXP3$Cntrl_values[i]
  viability[i] <- viability_est }

EXP3$viability <- viability

# Capping to 100%

EXP3$viability_cap <- viability

for (i in 1:nrow(EXP3)){
  print(EXP3$viability_cap[i])
  if (EXP3$viability_cap[i] > 100){
    EXP3$viability_cap[i] <- 100
  }}




EXP3_REF <- EXP3%>%
  filter(pop == "REF")

EXP3_REF$viability_res <- rescale(EXP3_REF$viability_cap, c(10,100))

EXP3_GSH1 <- EXP3%>%
  filter(pop == "GSH1")

EXP3_GSH1$viability_res <- rescale(EXP3_GSH1$viability_cap, c(10,100))

EXP3_C6 <- EXP3%>%
  filter(pop == "C6")

EXP3_C6$viability_res <- rescale(EXP3_C6$viability_cap, c(10,100))

EXP3_C7 <- EXP3%>%
  filter(pop == "C7")

EXP3_C7$viability_res <- rescale(EXP3_C7$viability_cap, c(10,100))

EXP3_C44 <- EXP3%>%
  filter(pop == "C44")

EXP3_C44$viability_res <- rescale(EXP3_C44$viability_cap, c(10,100))

EXP3_C58 <- EXP3%>%
  filter(pop == "C58")

EXP3_C58$viability_res <- rescale(EXP3_C58$viability_cap, c(10,100))

EXP3_C67 <- EXP3%>%
  filter(pop == "C67")

EXP3_C67$viability_res <- rescale(EXP3_C67$viability_cap, c(10,100))

EXP3_C73 <- EXP3%>%
  filter(pop == "C73")

EXP3_C73$viability_res <- rescale(EXP3_C73$viability_cap, c(10,100))

EXP3_C85 <- EXP3%>%
  filter(pop == "C85")

EXP3_C85$viability_res <- rescale(EXP3_C85$viability_cap, c(10,100))

EXP3_C89 <- EXP3%>%
  filter(pop == "C89")

EXP3_C89$viability_res <- rescale(EXP3_C89$viability_cap, c(10,100))

EXP3 <- rbind(EXP3_GSH1,EXP3_REF, EXP3_C6,EXP3_C7,EXP3_C44, EXP3_C58,
              EXP3_C67,EXP3_C73, EXP3_C85, EXP3_C89 )


# Binding data

DataMTT_Full  <- rbind(EXP_piloto,EXP1, EXP2, EXP3)


# CSV exportation

write.csv(DataMTT_Full , file = "Data/Processed/DataMTT_processed_normalized.csv", 
          sep = ",", row.names = F)

