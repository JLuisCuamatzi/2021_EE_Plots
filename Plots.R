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
df.temp <- data.frame(Condition = c(rep("Control",3),rep("H2O2",3)),
                      TreatmentNum = c("Treatment 01","Treatment 01"),
                      H2O2 = c(rep("w/o ROS 0 mM",3), rep("5 mM",3)),
                      Selected = c("Yes","Yes"),
                      Rep = c(rep("D",1), rep("E",1),rep("F",1),
                              rep("A",1), rep("B",1),rep("C",1)),
                      Cuantification = c("cells_mL"),
                      Cellular_concentration = c(1000000),
                      Time = c(rep("1",6))) # works

data <- bind_rows(df.temp,data)

data <- data %>% 
  arrange(TreatmentNum) %>% 
  group_by(Rep) %>% 
  mutate(cumulative_time = cumsum(Time)) # works
data$Rep <- paste("Lineage ",data$Rep, sep = "") # works
data$Rep <- factor(data$Rep, levels = c("Lineage A","Lineage B","Lineage C",
                                        "Lineage D","Lineage E","Lineage F")) #works
data$facet_fill_color <- c("blue","orange")[as.factor(data$Condition)] # works
# data.a <- data[grep("A",data$Rep),] # only to see one lineage
## temporal df
df.2.temp <- data.frame(Condition = c(rep("Control",20),rep("H2O2",20)),
                        #TreatmentNum = c(paste(rep("Treatment 0",42), seq(0,20,1), sep ="")),
                        Time = c(rep(seq(1,913,48),2)),
                        Cellular_concentration = c(1000000))

scientific <- function(x){
  ifelse(x==0, "0", 
         parse(text=gsub("[+]", "",gsub("e", " %*% 10^", scientific_format()(x)))))
} # works

plot.1 <- ggplot(data, aes(x = cumulative_time, y = Cellular_concentration)) +
  geom_line() + 
  geom_hline(yintercept = 1e6,linetype='dotted', col = 'red')+
  geom_hline(yintercept = 1e5,linetype='dotted', col = 'blue') +
  geom_hline(yintercept = 1e7,linetype='dotted', col = 'orange') +
  scale_y_log10(labels = scientific) +
  scale_x_continuous(limits = c(0,970), breaks = seq(0,970,48)) +
  geom_point(data = data, aes(x = cumulative_time, y = Cellular_concentration,color = Cuantification))+
  facet_grid(Rep~.) +
  geom_point(data = df.2.temp, aes(x = Time, y = Cellular_concentration))+
  labs(y = "cells/mL", x = "Time (h)") + 
  theme_classic() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size=15),
        strip.background = element_rect(colour="black",fill="ivory"),
        axis.title.x = element_text(face = "bold",
                                    size = 18),
        axis.title.y = element_text(face = "bold",
                                    size = 18),
        axis.text.x = element_text(face = "bold",
                                   size = 14),
        axis.text.y = element_text(face = "bold",
                                   size = 14)) #works

plot.1 #works
rm(list = ls(pattern = ".temp"))

ggsave("EE_Plot_01_Growth_profile.png", plot = plot.1, dpi = 300, width = 13, height = 9)

## Now, do plots for each evolutionary group (Control and H2O2)

data.control <- data[grep("Control",data$Condition),]
data.h2o2 <- data[grep("H2O2",data$Condition),]

plot.control <- ggplot(data.control, aes(x = cumulative_time, y = Cellular_concentration)) +
  geom_line() + 
  geom_hline(yintercept = 1e6,linetype='dotted', col = 'red')+
  geom_hline(yintercept = 1e5,linetype='dotted', col = 'blue') +
  geom_hline(yintercept = 1e7,linetype='dotted', col = 'orange') +
  scale_y_log10(labels = scientific) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,48)) +
  geom_point(data = data, aes(x = cumulative_time, y = Cellular_concentration,color = Cuantification))+
  facet_grid(Rep~.) +
  theme_classic()+
  geom_point(data = df.2.temp, aes(x = Time, y = Cellular_concentration))+
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



#########
dat <- mtcars
## Add in some colors based on the data
dat$facet_fill_color <- c("red", "green", "blue", "yellow", "orange")[dat$gear]
## Create main plot

p <- ggplot(dat, aes(x=cyl, y=wt)) + 
  geom_point(aes(fill=hp)) + facet_grid(gear+carb ~ .) +
  theme(strip.background=element_blank())
p
dummy <- p

dummy <- plot.1
dummy$layers <- NULL
dummy <- dummy + geom_rect(data=data, xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf,
                           aes(fill = facet_fill_color))

g1 <- ggplotGrob(plot.1)
g2 <- ggplotGrob(dummy)

gtable_select <- function (x, ...) 
{
  matches <- c(...)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  x
}

panels <- grepl(pattern="panel", g2$layout$name)
strips <- grepl(pattern="strip-right", g2$layout$name)
g2$grobs[strips] <- replicate(sum(strips), nullGrob(), simplify = FALSE)
g2$layout$l[panels] <- g2$layout$l[panels] + 1
g2$layout$r[panels] <- g2$layout$r[panels] + 2

new_strips <- gtable_select(g2, panels | strips)
grid.newpage()
grid.draw(new_strips)

gtable_stack <- function(g1, g2){
  g1$grobs <- c(g1$grobs, g2$grobs)
  g1$layout <- rbind(g1$layout, g2$layout)
  g1
}
## ideally you'd remove the old strips, for now they're just covered
new_plot <- gtable_stack(g1, new_strips)
grid.newpage()
grid.draw(new_plot)














