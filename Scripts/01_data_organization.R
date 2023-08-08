################################################################################
################################  Data organization ############################
################################################################################

#Packages

#library(dplyr)
library(ggplot2)
library(tidyverse)


# Loading data 

GS_EXP_piloto <- read.csv(file = "Data/Raw/GSH1/Piloto.csv", sep = ",")

colnames(GS_EXP_piloto) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", 
                              "C73", "C85", "C89", "x2")
GS_EXP_piloto <- GS_EXP_piloto[,-1]
GS_EXP_piloto <- GS_EXP_piloto[,-1]
GS_EXP_piloto <- GS_EXP_piloto[,-11]
GS_EXP_piloto <- GS_EXP_piloto[-1,]
GS_EXP_piloto <- GS_EXP_piloto[-7,]

GS_EXP_piloto<- GS_EXP_piloto %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='values')

GS_EXP_piloto$conc <-  rep(c("0.0","4.0",               # tested concentrations
                             "25.6","160.0","400.0","1000.0"), each =  10)

GS_EXP_piloto$pop <- rep(c("REF","GSH1", "C7", "C6","C44", "C58", "C67", "C73", "C85",
                           "C89"), # populations ID
                         times= 6)

GS_EXP_piloto$experiment <- "EXP_piloto" 

GS_EXP_piloto$gene <- "GSH1" 

GS_EXP1 <- read.csv(file = "Data/Raw/GSH1/EXP1.csv", sep = ",")

colnames(GS_EXP1) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67", 
                        "C73", "C85", "C89", "x2")
GS_EXP1 <- GS_EXP1[,-1]
GS_EXP1 <- GS_EXP1[,-1]
GS_EXP1 <- GS_EXP1[,-11]
GS_EXP1 <- GS_EXP1[-1,]
GS_EXP1 <- GS_EXP1[-7,]

GS_EXP1<- GS_EXP1 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='values')

GS_EXP1$conc <-  rep(c("0.0","4.0",               # tested concentrations
                       "25.6","160.0","400.0","1000.0"), each =  10)

GS_EXP1$pop <- rep(c("REF","GSH1", "C7", "C6","C44", "C58", "C67", "C73", "C85",
                     "C89"), # populations ID
                   times= 6)

GS_EXP1$experiment <- "EXP1"

GS_EXP1$gene <- "GSH1" 


# GS_EXP 2

GS_EXP2 <- read.csv(file = "Data/Raw/GSH1/EXP2.csv", sep = ",")


colnames(GS_EXP2) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67",
                        "C73", "C85", "C89", "x2")
GS_EXP2 <- GS_EXP2[,-1]
GS_EXP2 <- GS_EXP2[,-1]
GS_EXP2 <- GS_EXP2[,-11]
GS_EXP2 <- GS_EXP2[-1,]
GS_EXP2 <- GS_EXP2[-7,]

GS_EXP2<- GS_EXP2 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='values')

GS_EXP2$conc <-  rep(c("0.0","4.0",               # tested concentrations
                       "25.6","160.0","400.0","1000.0"), each =  10)

GS_EXP2$pop <- rep(c("REF","GSH1", "C7", "C6","C44", "C58", "C67", "C73", 
                     "C85", "C89"), # populations ID
                   times= 6)

GS_EXP2$experiment <- "EXP2"

GS_EXP2$gene <- "GSH1" 



GS_EXP3 <- read.csv(file = "Data/Raw/GSH1/EXP3.csv", sep = ",")


colnames(GS_EXP3) <-  c("x","x1", "REF", "GSH1","C6", "C7", "C44", "C58", "C67",
                        "C73", "C85", "C89", "x2")
GS_EXP3 <- GS_EXP3[,-1]
GS_EXP3 <- GS_EXP3[,-1]
GS_EXP3 <- GS_EXP3[,-11]
GS_EXP3 <- GS_EXP3[-1,]
GS_EXP3 <- GS_EXP3[-7,]

GS_EXP3<- GS_EXP3 %>%
  pivot_longer(cols=c("REF", "GSH1","C6", "C7", "C44", "C58", "C67", "C73", 
                      "C85", "C89"),
               names_to='pop',
               values_to='values')

GS_EXP3$conc <-  rep(c("0.0","4.0",               # tested concentrations
                       "25.6","160.0","400.0","1000.0"), each =  10)

GS_EXP3$pop <- rep(c("REF","GSH1", "C7", "C6","C44", "C58", "C67", "C73", "C85",
                     "C89"), # populations ID
                   times= 6)

GS_EXP3$experiment <- "EXP3"

GS_EXP3$gene <- "GSH1" 


# PGPA data

PG_EXP_piloto <- read.csv(file = "Data/Raw/PGPA/EXP_piloto.csv", sep = ",")

colnames(PG_EXP_piloto) <-  c("x","x1", "x2", "x3","x4", "x5", "x6", "x7", 
                              "REF", "C76", "C67p", "C68", "x12")
PG_EXP_piloto <- PG_EXP_piloto[,-1]
PG_EXP_piloto <- PG_EXP_piloto[,-1]
PG_EXP_piloto <- PG_EXP_piloto[,-1]
PG_EXP_piloto <- PG_EXP_piloto[,-1]
PG_EXP_piloto <- PG_EXP_piloto[,-1]
PG_EXP_piloto <- PG_EXP_piloto[,-1]
PG_EXP_piloto <- PG_EXP_piloto[,-1]
PG_EXP_piloto <- PG_EXP_piloto[,-1]
PG_EXP_piloto <- PG_EXP_piloto[,-5]
PG_EXP_piloto <- PG_EXP_piloto[-1,]
PG_EXP_piloto <- PG_EXP_piloto[-7,]

PG_EXP_piloto<- PG_EXP_piloto %>%
  pivot_longer(cols=c("REF", "C76", "C67p", "C68"),
               names_to='pop',
               values_to='values')

PG_EXP_piloto$conc <-  rep(c("0.0","4.0",               # tested concentrations
                             "25.6","160.0","400.0","1000.0"), each =  4)

PG_EXP_piloto$experiment <- "EXP_piloto" 

PG_EXP_piloto$gene <- "PGPA" 


PG_EXP1 <- read.csv(file = "Data/Raw/PGPA/EXP1.csv", sep = ",")

colnames(PG_EXP1) <-  c("x","x1", "x2", "x3","x4", "x5", "x6", "x7", 
                        "REF", "C76", "C67p", "C68", "x12")
PG_EXP1 <- PG_EXP1[,-1]
PG_EXP1 <- PG_EXP1[,-1]
PG_EXP1 <- PG_EXP1[,-1]
PG_EXP1 <- PG_EXP1[,-1]
PG_EXP1 <- PG_EXP1[,-1]
PG_EXP1 <- PG_EXP1[,-1]
PG_EXP1 <- PG_EXP1[,-1]
PG_EXP1 <- PG_EXP1[,-1]
PG_EXP1 <- PG_EXP1[,-5]
PG_EXP1 <- PG_EXP1[-1,]
PG_EXP1 <- PG_EXP1[-7,]

PG_EXP1<- PG_EXP1 %>%
  pivot_longer(cols=c("REF", "C76", "C67p", "C68"),
               names_to='pop',
               values_to='values')

PG_EXP1$conc <-  rep(c("0.0","4.0",               # tested concentrations
                       "25.6","160.0","400.0","1000.0"), each =  4)

PG_EXP1$experiment <- "EXP1" 

PG_EXP1$gene <- "PGPA" 

PG_EXP2 <- read.csv(file = "Data/Raw/PGPA/EXP2.csv", sep = ",")

colnames(PG_EXP2) <-  c("x","x1", "x2", "x3","x4", "x5", "x6", "x7", 
                        "REF", "C76", "C67p", "C68", "x12")
PG_EXP2 <- PG_EXP2[,-1]
PG_EXP2 <- PG_EXP2[,-1]
PG_EXP2 <- PG_EXP2[,-1]
PG_EXP2 <- PG_EXP2[,-1]
PG_EXP2 <- PG_EXP2[,-1]
PG_EXP2 <- PG_EXP2[,-1]
PG_EXP2 <- PG_EXP2[,-1]
PG_EXP2 <- PG_EXP2[,-1]
PG_EXP2 <- PG_EXP2[,-5]
PG_EXP2 <- PG_EXP2[-1,]
PG_EXP2 <- PG_EXP2[-7,]

PG_EXP2<- PG_EXP2 %>%
  pivot_longer(cols=c("REF", "C76", "C67p", "C68"),
               names_to='pop',
               values_to='values')

PG_EXP2$conc <-  rep(c("0.0","4.0",               # tested concentrations
                       "25.6","160.0","400.0","1000.0"), each =  4)

PG_EXP2$experiment <- "EXP2" 

PG_EXP2$gene <- "PGPA" 

PG_EXP3 <- read.csv(file = "Data/Raw/PGPA/EXP3.csv", sep = ",")

colnames(PG_EXP3) <-  c("x","x1", "x2", "x3","x4", "x5", "x6", "x7", 
                        "REF", "C76", "C67p", "C68", "x12")
PG_EXP3 <- PG_EXP3[,-1]
PG_EXP3 <- PG_EXP3[,-1]
PG_EXP3 <- PG_EXP3[,-1]
PG_EXP3 <- PG_EXP3[,-1]
PG_EXP3 <- PG_EXP3[,-1]
PG_EXP3 <- PG_EXP3[,-1]
PG_EXP3 <- PG_EXP3[,-1]
PG_EXP3 <- PG_EXP3[,-1]
PG_EXP3 <- PG_EXP3[,-5]
PG_EXP3 <- PG_EXP3[-1,]
PG_EXP3 <- PG_EXP3[-7,]

PG_EXP3<- PG_EXP3 %>%
  pivot_longer(cols=c("REF", "C76", "C67p", "C68"),
               names_to='pop',
               values_to='values')

PG_EXP3$conc <-  rep(c("0.0","4.0",               # tested concentrations
                       "25.6","160.0","400.0","1000.0"), each =  4)

PG_EXP3$experiment <- "EXP3" 

PG_EXP3$gene <- "PGPA" 

#Binding data

DataMTT <- rbind(GS_EXP_piloto,GS_EXP1, GS_EXP2, GS_EXP3,
                 PG_EXP_piloto,PG_EXP1, PG_EXP2, PG_EXP3)

# Saving processed data

write.csv(DataMTT, file = "Data/Processed/DataMTT")

