library(tidyverse)
library(svglite)
library(ggplot2)
library(readxl)


data <- read_excel("./data/WheatBreedingSampleData.xlsx", sheet = "Breeding")

plt <- ggplot(data, aes(Yield, Maturity)) + 
  geom_point() + 
  geom_smooth(method='lm')


ggsave('plot.svg', plot=plt, width = 16, height = 9, units = "cm", dpi = 600, device = svglite)



