################################################################################
################################  Data organization ############################
################################################################################

#Packages

#library(dplyr)
library(ggplot2)
library(tidyverse)


# Loading data 

EXP_piloto <- read.csv(file = "Data/Raw/Piloto.csv", sep = ",")

colnames(EXP_piloto) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", 
                           "C73", "C85", "C89", "x2")
EXP_piloto <- EXP_piloto[,-1]
EXP_piloto <- EXP_piloto[,-1]
EXP_piloto <- EXP_piloto[,-11]
EXP_piloto <- EXP_piloto[-1,]
EXP_piloto <- EXP_piloto[-7,]

EXP_piloto<- EXP_piloto %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='values')

EXP_piloto$conc <-  rep(c("0.0","4.0",               # tested concentrations
                          "25.6","160.0","400.0","1000.0"), each =  10)

EXP_piloto$pop <- rep(c("REF","GSH1", "C7", "C6","C44", "C58", "C67", "C73", "C85",
                        "C89"), # populations ID
                      times= 6)

EXP_piloto$experiment <- "EXP_piloto" 

EXP1 <- read.csv(file = "Data/Raw/EXP1.csv", sep = ",")

colnames(EXP1) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", 
                     "C73", "C85", "C89", "x2")
EXP1 <- EXP1[,-1]
EXP1 <- EXP1[,-1]
EXP1 <- EXP1[,-11]
EXP1 <- EXP1[-1,]
EXP1 <- EXP1[-7,]

EXP1<- EXP1 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='values')

EXP1$conc <-  rep(c("0.0","4.0",               # tested concentrations
                    "25.6","160.0","400.0","1000.0"), each =  10)

EXP1$pop <- rep(c("REF","GSH1", "C7", "C6","C44", "C58", "C67", "C73", "C85",
                  "C89"), # populations ID
                times= 6)

EXP1$experiment <- "EXP_1" 


# EXP 2

EXP2 <- read.csv(file = "Data/Raw/EXP2.csv", sep = ",")


colnames(EXP2) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67",
                     "C73", "C85", "C89", "x2")
EXP2 <- EXP2[,-1]
EXP2 <- EXP2[,-1]
EXP2 <- EXP2[,-11]
EXP2 <- EXP2[-1,]
EXP2 <- EXP2[-7,]

EXP2<- EXP2 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='values')

EXP2$conc <-  rep(c("0.0","4.0",               # tested concentrations
                    "25.6","160.0","400.0","1000.0"), each =  10)

EXP2$pop <- rep(c("REF","GSH1", "C7", "C6","C44", "C58", "C67", "C73", 
                  "C85", "C89"), # populations ID
                times= 6)

EXP2$experiment <- "EXP_2"                                      # Experiment ID 

EXP3 <- read.csv(file = "Data/Raw/EXP3.csv", sep = ",")


colnames(EXP3) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67",
                     "C73", "C85", "C89", "x2")
EXP3 <- EXP3[,-1]
EXP3 <- EXP3[,-1]
EXP3 <- EXP3[,-11]
EXP3 <- EXP3[-1,]
EXP3 <- EXP3[-7,]

EXP3<- EXP3 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='values')

EXP3$conc <-  rep(c("0.0","4.0",               # tested concentrations
                    "25.6","160.0","400.0","1000.0"), each =  10)

EXP3$pop <- rep(c("REF","GSH1", "C7", "C6","C44", "C58", "C67", "C73", "C85",
                  "C89"), # populations ID
                times= 6)

EXP3$experiment <- "EXP_3"                                      # Experiment ID 


#Binding data

DataMTT <- rbind(EXP_piloto,EXP1, EXP2, EXP3)

# Saving processed data

write.csv(DataMTT, file = "Data/Processed/DataMTT")
