library(tidyverse)
library(svglite)
library(ggplot2)
library(readxl)

setwd("C:/Users/Ravichandranp/OneDrive - AGR-AGR/Documents/GitHub/DataVisualization")
data <- read_excel("./data/WheatBreedingSampleData.xlsx", sheet = WeedScience)

plt <- ggplot(mydata, aes(x, y, group = typ, color=typ)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width = 0.1) +
  theme_minimal()

ggsave('C:/Users/Ravichandranp/OneDrive - AGR-AGR/Desktop/plot.svg', plot=plt, width = 16, height = 9, units = "cm", dpi = 600, device = svglite)
