## 2021-09-08
# Plots for 2021-experimental-evolution of U maydis under oxidative stress
# empty the environment
rm(list = ls())

#loading libraries
library(readxl, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
library(tidyverse, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
library(plyr, warn.conflicts = F)
library(scales, warn.conflicts = F)
library(plotly, warn.conflicts = F)
library(ggExtra, warn.conflicts = F)
library(viridis, warn.conflicts = F)
library(lemon, warn.conflicts = F)
library(grid, warn.conflicts = F)

# reading the dataframe

USMA_EE_2021 <- read_xlsx("2021_EE_Growth_data_DF.xlsx",
                          range = "EE_Dataframe!A1:M376")

# Plot: growth profile with the bottlenecks
data <- USMA_EE_2021[,c(1:5,8,9)]
data <- data[grep("Yes", data$Selected),]

data <- data %>% 
  pivot_longer(cols = c(CFU_mL,cells_mL ),
               names_to = "Cuantification",
               values_to = c("Cellular_concentration")) # works

data <- data %>% 
  mutate(Time = case_when(
    startsWith(Cuantification, "CFU") ~ "3",
    startsWith(Cuantification, "cells") ~ "45")) # works

df.temp <- data.frame(Condition = c(rep("Control",6),rep("H2O2",6)),
                      TreatmentNum = c("Treatment 01","Treatment 01"),
                      H2O2 = c(rep("w/o ROS 0 mM",6), rep("5 mM",6)),
                      Selected = c("Yes","Yes"),
                      Rep = c(rep("D",2), rep("E",2),rep("F",2),
                              rep("A",2), rep("B",2),rep("C",2)),
                      Cuantification = c("CFU_mL","cells_mL"),
                      Cellular_concentration = c(0,1000000),
                      Time = c(rep("0",12))) # works
data <- bind_rows(data, df.temp) #works


