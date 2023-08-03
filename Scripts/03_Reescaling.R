################################################################################
###################### Normalization and reescaling data  #############################
################################################################################

library(scales)
library(dplyr)


#Loading data

DataMTT <- read.csv(file = "Data/Processed/DataMTT_viability.csv")

# Spliting data

EXP_piloto <- filter(DataMTT, experiment == "EXP_piloto")
EXP1 <- filter(DataMTT, experiment == "EXP_1")
EXP2 <- filter(DataMTT, experiment == "EXP_2")
EXP3 <- filter(DataMTT, experiment == "EXP_3")

# EXP Piloto

EXP_piloto_REF <- EXP_piloto%>%
  filter(pop == "REF")

EXP_piloto_REF$viability_res <- rescale(EXP_piloto_REF$viability, c(0,100))

EXP_piloto_GSH1 <- EXP_piloto%>%
  filter(pop == "GSH1")

EXP_piloto_GSH1$viability_res <- rescale(EXP_piloto_GSH1$viability, c(0,100))

EXP_piloto_C6 <- EXP_piloto%>%
  filter(pop == "C6")

EXP_piloto_C6$viability_res <- rescale(EXP_piloto_C6$viability, c(0,100))

EXP_piloto_C7 <- EXP_piloto%>%
  filter(pop == "C7")

EXP_piloto_C7$viability_res <- rescale(EXP_piloto_C7$viability, c(0,100))

EXP_piloto_C44 <- EXP_piloto%>%
  filter(pop == "C44")

EXP_piloto_C44$viability_res <- rescale(EXP_piloto_C44$viability, c(0,100))

EXP_piloto_C58 <- EXP_piloto%>%
  filter(pop == "C58")

EXP_piloto_C58$viability_res <- rescale(EXP_piloto_C58$viability, c(0,100))

EXP_piloto_C67 <- EXP_piloto%>%
  filter(pop == "C67")

EXP_piloto_C67$viability_res <- rescale(EXP_piloto_C67$viability, c(0,100))

EXP_piloto_C73 <- EXP_piloto%>%
  filter(pop == "C73")

EXP_piloto_C73$viability_res <- rescale(EXP_piloto_C73$viability, c(0,100))

EXP_piloto_C85 <- EXP_piloto%>%
  filter(pop == "C85")

EXP_piloto_C85$viability_res <- rescale(EXP_piloto_C85$viability, c(0,100))

EXP_piloto_C89 <- EXP_piloto%>%
  filter(pop == "C89")

EXP_piloto_C89$viability_res <- rescale(EXP_piloto_C89$viability, c(0,100))

EXP_piloto <- rbind(EXP_piloto_GSH1,EXP_piloto_REF, EXP_piloto_C6,EXP_piloto_C7,EXP_piloto_C44, EXP_piloto_C58,
                    EXP_piloto_C67,EXP_piloto_C73, EXP_piloto_C85, EXP_piloto_C89 )


# EXP 1

EXP1_REF <- EXP1%>%
  filter(pop == "REF")

EXP1_REF$viability_res <- rescale(EXP1_REF$viability, c(0,100))

EXP1_GSH1 <- EXP1%>%
  filter(pop == "GSH1")

EXP1_GSH1$viability_res <- rescale(EXP1_GSH1$viability, c(0,100))

EXP1_C6 <- EXP1%>%
  filter(pop == "C6")

EXP1_C6$viability_res <- rescale(EXP1_C6$viability, c(0,100))

EXP1_C7 <- EXP1%>%
  filter(pop == "C7")

EXP1_C7$viability_res <- rescale(EXP1_C7$viability, c(0,100))

EXP1_C44 <- EXP1%>%
  filter(pop == "C44")

EXP1_C44$viability_res <- rescale(EXP1_C44$viability, c(0,100))

EXP1_C58 <- EXP1%>%
  filter(pop == "C58")

EXP1_C58$viability_res <- rescale(EXP1_C58$viability, c(0,100))

EXP1_C67 <- EXP1%>%
  filter(pop == "C67")

EXP1_C67$viability_res <- rescale(EXP1_C67$viability, c(0,100))

EXP1_C73 <- EXP1%>%
  filter(pop == "C73")

EXP1_C73$viability_res <- rescale(EXP1_C73$viability, c(0,100))

EXP1_C85 <- EXP1%>%
  filter(pop == "C85")

EXP1_C85$viability_res <- rescale(EXP1_C85$viability, c(0,100))

EXP1_C89 <- EXP1%>%
  filter(pop == "C89")

EXP1_C89$viability_res <- rescale(EXP1_C89$viability, c(0,100))

EXP1 <- rbind(EXP1_GSH1,EXP1_REF, EXP1_C6,EXP1_C7,EXP1_C44, EXP1_C58,
              EXP1_C67,EXP1_C73, EXP1_C85, EXP1_C89 )

# EXP 2

EXP2_REF <- EXP2%>%
  filter(pop == "REF")

EXP2_REF$viability_res <- rescale(EXP2_REF$viability, c(0,100))

EXP2_GSH1 <- EXP2%>%
  filter(pop == "GSH1")

EXP2_GSH1$viability_res <- rescale(EXP2_GSH1$viability, c(0,100))

EXP2_C6 <- EXP2%>%
  filter(pop == "C6")

EXP2_C6$viability_res <- rescale(EXP2_C6$viability, c(0,100))

EXP2_C7 <- EXP2%>%
  filter(pop == "C7")

EXP2_C7$viability_res <- rescale(EXP2_C7$viability, c(0,100))

EXP2_C44 <- EXP2%>%
  filter(pop == "C44")

EXP2_C44$viability_res <- rescale(EXP2_C44$viability, c(0,100))

EXP2_C58 <- EXP2%>%
  filter(pop == "C58")

EXP2_C58$viability_res <- rescale(EXP2_C58$viability, c(0,100))

EXP2_C67 <- EXP2%>%
  filter(pop == "C67")

EXP2_C67$viability_res <- rescale(EXP2_C67$viability, c(0,100))

EXP2_C73 <- EXP2%>%
  filter(pop == "C73")

EXP2_C73$viability_res <- rescale(EXP2_C73$viability, c(0,100))

EXP2_C85 <- EXP2%>%
  filter(pop == "C85")

EXP2_C85$viability_res <- rescale(EXP2_C85$viability, c(0,100))

EXP2_C89 <- EXP2%>%
  filter(pop == "C89")

EXP2_C89$viability_res <- rescale(EXP2_C89$viability, c(0,100))

EXP2 <- rbind(EXP2_GSH1,EXP2_REF, EXP2_C6,EXP2_C7,EXP2_C44, EXP2_C58,
              EXP2_C67,EXP2_C73, EXP2_C85, EXP2_C89 )

# exp 3

EXP3_REF <- EXP3%>%
  filter(pop == "REF")

EXP3_REF$viability_res <- rescale(EXP3_REF$viability, c(0,100))

EXP3_GSH1 <- EXP3%>%
  filter(pop == "GSH1")

EXP3_GSH1$viability_res <- rescale(EXP3_GSH1$viability, c(0,100))

EXP3_C6 <- EXP3%>%
  filter(pop == "C6")

EXP3_C6$viability_res <- rescale(EXP3_C6$viability, c(0,100))

EXP3_C7 <- EXP3%>%
  filter(pop == "C7")

EXP3_C7$viability_res <- rescale(EXP3_C7$viability, c(0,100))

EXP3_C44 <- EXP3%>%
  filter(pop == "C44")

EXP3_C44$viability_res <- rescale(EXP3_C44$viability, c(0,100))

EXP3_C58 <- EXP3%>%
  filter(pop == "C58")

EXP3_C58$viability_res <- rescale(EXP3_C58$viability, c(0,100))

EXP3_C67 <- EXP3%>%
  filter(pop == "C67")

EXP3_C67$viability_res <- rescale(EXP3_C67$viability, c(0,100))

EXP3_C73 <- EXP3%>%
  filter(pop == "C73")

EXP3_C73$viability_res <- rescale(EXP3_C73$viability, c(0,100))

EXP3_C85 <- EXP3%>%
  filter(pop == "C85")

EXP3_C85$viability_res <- rescale(EXP3_C85$viability, c(0,100))

EXP3_C89 <- EXP3%>%
  filter(pop == "C89")

EXP3_C89$viability_res <- rescale(EXP3_C89$viability, c(0,100))

EXP3 <- rbind(EXP3_GSH1,EXP3_REF, EXP3_C6,EXP3_C7,EXP3_C44, EXP3_C58,
              EXP3_C67,EXP3_C73, EXP3_C85, EXP3_C89 )

# Binding data

DataMTT_Full  <- rbind(EXP_piloto,EXP1, EXP2, EXP3)
write.csv(DataMTT_Full , file = "Data/Processed/DataMTT_processed_normalized.csv", 
          sep = ",", row.names = F)

