library(tidyverse)
library(svglite)
library(ggplot2)
library(readxl)
library(reshape2)
library(forcats)

data <- read_excel("./data/WheatBreedingSampleData.xlsx", sheet = "WeedScience", col_names = TRUE)

#### spectal plot ####
data.long <-melt(data, id.vars="Wavelength")

plt <- ggplot(data.long) + 
  geom_line(aes(Wavelength, value, color=variable), show.legend = FALSE) +
  labs(x="Wavelength (nm)", y ="Absorbance") + 
  theme(text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
#### end ####

#### box plot ####

plt <- ggplot(data) + 
  geom_boxplot(aes(fct_inorder(Genotype), Yield, fill = Genotype), show.legend = FALSE) + 
  labs(x="Genotype", y ="Yield") + 
  theme(text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 
#### end ####

#### scatterplot ####

plt <- ggplot(data) + 
  geom_point(aes(Maturity, Yield, color = Genotype), show.legend = FALSE) + 
  labs(x="Maturity", y ="Yield") + 
  theme(text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 
#### end ####

#### heatmap ####

plt <- ggplot(data) + 
  geom_tile(aes(Rows, Column, fill= Yield)) +
  labs(x="Rows", y ="Columns") + 
  theme(text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank()) +
  scale_fill_gradient(low="white", high="red")

#### end ####

#### line plot ####

seldata <- data %>% 
  select(c(Name, DAT_1, DAT_3, DAT_7, DAT_10, DAT_14, DAT_21))

seldata.long <- melt(seldata, id.vars="Name")
  
seldata.summary <- seldata.long %>% 
  group_by(Name, variable) %>% 
  summarise(
    sd = sd(value, na.rm = TRUE),
    value = mean(value)
  )


plt <- ggplot(seldata.summary) + 
  geom_line(aes(fct_inorder(variable), value, group=Name, color=Name)) +
  geom_point(aes(variable, value, group=Name, color=Name)) +
  geom_errorbar(aes(x = variable, ymin = value - sd, ymax = value + sd, color=Name), width = 0.1,  alpha=0.5) +
  labs(x="Days after Treatment", y ="VR", color = "Active Ingredient") + 
  theme(text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#### end ####

ggsave('plot.svg', plot = plt, width = 12, height = 7, units = "in", dpi = 600, device = svglite)



