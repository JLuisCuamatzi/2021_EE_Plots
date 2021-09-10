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
library(gtable, warn.conflicts = F)
library(grid, warn.conflicts = F)

# reading the dataframe
USMA_EE_2021 <- read_xlsx("2021_EE_Growth_data_DF.xlsx",
                          range = "EE_Dataframe!A1:M376") # works

# Plot: growth profile with the bottlenecks
data <- USMA_EE_2021[grep("Yes", USMA_EE_2021$Selected),]
data <- data[,c(1:5,9)]
data$Rep <- paste("Lineage ",data$Rep, sep = "") # works

data <- data %>% 
  pivot_longer(cols = c(CFU_mL,cells_mL ),
               names_to = "Cuantification",
               values_to = c("Cellular_concentration")) # works

df.3.temp <- data.frame(Condition = c(rep("Control",60),
                                      rep("H2O2",60)),
                        TreatmentNum = seq(1,20,1),
                        H2O2 = c(rep("w/o ROS 0 mM",60),
                                 rep("5 mM",2),rep("8 mM",2),rep("11 mM",2),rep("14 mM",2),
                                 rep("17 mM",2),rep("20 mM",2),rep("30 mM",2),rep("40 mM",2),
                                 rep("50 mM",2),rep("60 mM",2),rep("5 mM",2),rep("8 mM",2),
                                 rep("11 mM",2),rep("14 mM",2),rep("17 mM",2),rep("20 mM",2),
                                 rep("30 mM",2),rep("40 mM",2),rep("50 mM",2),rep("60 mM",2),
                                 rep("5 mM",2),rep("8 mM",2),rep("11 mM",2),rep("14 mM",2),
                                 rep("17 mM",2),rep("20 mM",2),rep("30 mM",2),rep("40 mM",2),
                                 rep("50 mM",2),rep("60 mM",2)),
                        Rep = c(rep("Lineage D", 20),
                                rep("Lineage E", 20),
                                rep("Lineage F", 20),
                                rep("Lineage A", 20),
                                rep("Lineage B", 20),
                                rep("Lineage C", 20)),
                        Cuantification = rep("cells_i",120),
                        Cellular_concentration = rep(1e6,120)) # works

df.3.temp$TreatmentNum <- sprintf("%02d", as.numeric(df.3.temp$TreatmentNum)) # works
df.3.temp$TreatmentNum <- paste("Treatment ", df.3.temp$TreatmentNum, sep ="") # works

data <- bind_rows(df.3.temp,data) # works
rm(list = ls(pattern = ".temp"))
data <- data %>% 
  mutate(Time = case_when(
    endsWith(Cuantification, "_i") ~ "1",
    startsWith(Cuantification, "CFU_mL") ~ "3",
    startsWith(Cuantification, "cells_mL") ~ "45")) # works

data <- data %>% 
  arrange(TreatmentNum) %>% 
  group_by(Rep) %>% 
  mutate(cumulative_time = cumsum(Time)) # works


scientific <- function(x){
  ifelse(x==0, "0", 
         parse(text=gsub("[+]", "",gsub("e", " %*% 10^", scientific_format()(x)))))
} # works

plot.1 <- ggplot(data, aes(x = cumulative_time, y = Cellular_concentration)) +
  geom_line(aes(color = Condition)) + 
  geom_hline(yintercept = 1e6,linetype='dotted', col = 'gray') +
  geom_hline(yintercept = 1e5,linetype='dotted', col = 'gray') +
  geom_hline(yintercept = 1e7,linetype='dotted', col = 'gray') +
  scale_y_log10(labels = scientific) +
  scale_x_continuous(limits = c(0,990), breaks = seq(0,990,48))+
  geom_point(data = data, aes(x = cumulative_time,
                              y = Cellular_concentration,
                              color = Cuantification, 
                              shape = Condition))+
  facet_grid(Rep~.) +
  labs(y = "cells/mL", x = "Time (h)") + 
  theme_classic() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size=15),
        strip.background = element_rect(colour="black",fill="green3"),
        axis.title.x = element_text(face = "bold",
                                    size = 18),
        axis.title.y = element_text(face = "bold",
                                    size = 18),
        axis.text.x = element_text(face = "bold",
                                   size = 14),
        axis.text.y = element_text(face = "bold",
                                   size = 14)) #works


plot.1 #works

ggsave("EE_Plot_01_Growth_profile.png", plot = plot.1, dpi = 300, width = 13, height = 9)

## Now, do plots for each evolutionary group (Control and H2O2)

data.control <- data[grep("Control",data$Condition),]
data.H2O2 <- data[grep("H2O2",data$Condition),]

plot.control <- ggplot(data.control, aes(x = cumulative_time, y = Cellular_concentration)) +
  geom_line() + 
  geom_hline(yintercept = 1e6,linetype='dotted', col = 'red')+
  geom_hline(yintercept = 1e5,linetype='dotted', col = 'blue') +
  geom_hline(yintercept = 1e7,linetype='dotted', col = 'orange') +
  scale_y_log10(labels = scientific) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,48)) +
  geom_point(aes(x = cumulative_time, y = Cellular_concentration,color = Cuantification))+
  facet_grid(Rep~.) +
  theme_classic()+
  labs(y = "cells/mL", x = "Time (h)") + 
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size=15),
        strip.background = element_rect(colour="black",
                                        fill="ivory"),
        axis.title.x = element_text(face = "bold",
                                    size = 18),
        axis.title.y = element_text(face = "bold",
                                    size = 18),
        axis.text.x = element_text(face = "bold",
                                   size = 14),
        axis.text.y = element_text(face = "bold",
                                   size = 14))
plot.control
ggsave("EE_Plot_02_Growth_profile_Control.png", plot = plot.control, dpi = 300, width = 13, height = 9)

## H2O2
plot.H2O2 <- ggplot(data.H2O2, aes(x = cumulative_time, y = Cellular_concentration)) +
  geom_line() + 
  geom_hline(yintercept = 1e6,linetype='dotted', col = 'red')+
  geom_hline(yintercept = 1e5,linetype='dotted', col = 'blue') +
  geom_hline(yintercept = 1e7,linetype='dotted', col = 'orange') +
  scale_y_log10(labels = scientific) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,48)) +
  geom_point(aes(x = cumulative_time, y = Cellular_concentration,color = Cuantification))+
  facet_grid(Rep~.) +
  theme_classic()+
  labs(y = "cells/mL", x = "Time (h)") + 
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size=15),
        strip.background = element_rect(colour="black",
                                        fill="ivory"),
        axis.title.x = element_text(face = "bold",
                                    size = 18),
        axis.title.y = element_text(face = "bold",
                                    size = 18),
        axis.text.x = element_text(face = "bold",
                                   size = 14),
        axis.text.y = element_text(face = "bold",
                                   size = 14))
plot.H2O2
ggsave("EE_Plot_03_Growth_profile_H2O2.png", plot = plot.H2O2, dpi = 300, width = 13, height = 9)



## trying to add a figure that indicates the magnitud of the shock
data.control.temp <- data.control
data.control.temp$H2O2 <- gsub('[a-zA-Z /]','', data.control.temp$H2O2)
data.control.temp$H2O2 <-as.numeric(data.control.temp$H2O2)

plot.control.temp <- ggplot(data.control.temp, aes(x = cumulative_time, y = Cellular_concentration)) +
  geom_line() + 
  geom_hline(yintercept = 1e6,linetype='dotted', col = 'red')+
  geom_hline(yintercept = 1e5,linetype='dotted', col = 'blue') +
  geom_hline(yintercept = 1e7,linetype='dotted', col = 'orange') +
  scale_y_log10(labels = scientific) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,48)) +
  geom_point(aes(x = cumulative_time, y = Cellular_concentration,color = Cuantification))+
  facet_grid(Rep~.) +
  theme_classic()+
  labs(y = "cells/mL", x = "Time (h)") + 
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size=15),
        strip.background = element_rect(colour="black",
                                        fill="ivory"),
        axis.title.x = element_text(face = "bold",
                                    size = 18),
        axis.title.y = element_text(face = "bold",
                                    size = 18),
        axis.text.x = element_text(face = "bold",
                                   size = 14),
        axis.text.y = element_text(face = "bold",
                                   size = 14))
plot.control.temp

## 2021-09-09
plot_ly(data, x = ~cumulative_time, 
        y = ~Cellular_concentration, 
        type = 'scatter',
        mode = 'lines+markers',
        color = ~Condition)%>% 
  layout(title = "Growth ",
         xaxis = list(title = "Time (h)"),
         yaxis = list(title = "cells/mL"))

data %>% 
  group_by(Rep) %>% 
  do(p=plot_ly(., x = ~cumulative_time,
               y = ~Cellular_concentration,
               color = ~Rep,
               type = 'scatter',
               mode = 'lines+markers'))  %>% 
  subplot(nrows = 6, shareX = T) %>% 
  layout(title = "Growth ",
         xaxis = list(title = "Time (h)"),
         yaxis = list(title = "cells/mL"))

rm(list = ls())
