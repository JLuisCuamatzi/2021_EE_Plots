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
                          range = "EE_Dataframe!A1:N394")

# Plot: growth profile with the bottlenecks
data <- USMA_EE_2021[,c(1:6,9,10)]
data <- data[grep("Yes", data$Selected),]

data <- data %>% 
  rowwise() %>% 
  mutate (Time = case_when(CFU_mL > 0 ~ "3",cells_mL > 1000000 ~ "45"))


data <- data %>% 
  pivot_longer(cols = c(CFU_mL,cells_mL ),
               names_to = "Cuantification",
               values_to = c("Cellular_concentration")) # works

data <- data %>% 
  mutate(Time = case_when(
    startsWith(Cuantification, "CFU") ~ "3",
    startsWith(Cuantification, "cells") ~ "45")) # works

data <- data %>% 
  mutate(Time = case_when(
    Cellular_concentration < 1 ~ "0"))




