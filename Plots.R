library(tidyverse)
library(svglite)
library(ggplot2)
library(readxl)
library(reshape2)
library(forcats)
library(corrplot)
library(ggmap)
library(GGally)
library(viridis)
library(plotly)
library(metR)


data <- read_excel("./data/WheatBreedingSampleData.xlsx", sheet = "Volcano", col_names = TRUE)

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
  theme(text = element_text(size = 20),
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        panel.background = element_blank(), 
        axis.line.x = element_line(colour = "black"))

outlier <- ggplot_build(plt)[["data"]][[1]][["outliers"]]
names(outlier) <- levels(factor(fct_inorder(data$Genotype)))
tidyout <- purrr::map_df(outlier, tibble::as_tibble, .id = "Genotype")

plt <- plt + geom_text(data = tidyout, aes(Genotype, value, label = value), 
              hjust = -.3)

#### end ####

#### scatterplot ####

plt <- ggplot(data) + 
  geom_point(aes(Maturity, Yield, color = Genotype), show.legend = FALSE) + 
  geom_smooth(aes(x = Maturity, y = Yield), method = 'lm', se = FALSE, alpha =0.5) +
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
  select(c(Name, DAT_0, DAT_1, DAT_3, DAT_7, DAT_10, DAT_14, DAT_21))

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

#### corr plot ####
seldata <- na.omit(data %>% select(c(-"Name")))
cordata <- melt(round(cor(seldata), 2))
plt <- ggplot(cordata, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile(show.legend = FALSE) +
  scale_fill_viridis_c(option = "viridis") +
  theme(text = element_text(size = 28),
        legend.text = element_text(size = 20),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        #axis.line = element_blank()
        ) +
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
            #fontface = "bold",
            size = 6) +
  #scale_fill_gradient(low="gold", high="deepskyblue", limits = c(-1, +1)) +
  coord_equal()
#### end ####

#### bar plot - with title ####
seldata <- data %>% select(c("Genotype", "Yield"))
plt <- ggplot(data, aes(fct_inorder(Genotype), Yield, fill = Genotype)) +
  labs(x="Genotype", y ="Yield", caption = "Data Source: AAFC, 2022" ) + 
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean",
           show.legend = FALSE) +
  theme(text = element_text(size = 20, family = "Roboto"), #Futura, Roboto, Helvetica
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        panel.background = element_blank(), 
        axis.line.x = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(size = 32, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "plain"),
        plot.title.position = "plot", 
        plot.caption.position = "plot") +
  ggtitle("Breeding Trial Results",
          subtitle = "(Yield & Maturity for 2022)")
  
#### end ####

#### Density plot ####
seldata <- data %>% select(c("DAT_1", "DAT_3", "DAT_7", "DAT_10", "DAT_14", "DAT_21"))
densdata <- melt(seldata)
plt <- ggplot(densdata, aes(x=value)) + 
  geom_density(aes(fill=variable), alpha = 0.25) +
  theme(text = element_text(size = 20, family = "Roboto"), #Futura, Roboto, Helvetica
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        panel.background = element_blank(), 
        axis.line.x = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(size = 32, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "plain"),
        plot.title.position = "plot", 
        plot.caption.position = "plot") +
  facet_wrap(~variable, ncol = 3)


#### end ####

#### gg maps ####
register_google("AIzaSyDOqTnG8AJJa5dam58T5hgNKE-4glffOaA")
lisbon_map <- get_map(location = c(-112.762432, 49.699624), maptype='satellite', source='google', zoom = 17)
plt <- ggmap(lisbon_map) +
  labs(x="Longitude", y ="Latitude")

#### end ####

#### bubble plot ####
seldata <- data %>% na.omit(select(c("GNS", "GWS", "GW")))
plt <- ggplot(seldata, aes(x=GNS, y=GWS, size = GW, color = GW)) +
  geom_point(alpha=0.7) +
  scale_color_viridis(option = "magma") + #
  labs(x="Grains per spike", y ="Grain weight per spike", caption = "Data Source: AAFC, 2023" ) + 
  theme(text = element_text(size = 20, family = "Roboto"), #Futura, Roboto, Helvetica
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(size = 32, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "plain"),
        plot.title.position = "plot", 
        plot.caption.position = "plot")
#### end ####

#### lollipop plot ####
seldata <- na.omit(data[1:15,])
plt <- ggplot(seldata, aes(x=Name, y=AGB)) +
  geom_segment( aes(x=Name, xend=Name, y=0, yend=AGB)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
  labs(x="Genotype", y ="Above ground biomass", caption = "Data Source: AAFC, 2023" ) + 
  coord_flip() +
  theme(text = element_text(size = 20, family = "Roboto"), #Futura, Roboto, Helvetica
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(size = 32, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "plain"),
        plot.title.position = "plot", 
        plot.caption.position = "plot")
#### end ####

#### Parallel Coordinate plot ####
seldata <- data %>% select(c(4, 6:12))
seldata$Name <-  as.factor(seldata$Name)
plt <- ggparcoord(seldata,
                  columns = 2:8, groupColumn = 1, order = 2:8, scale = "center",
                  showPoints = TRUE, 
                  alphaLines = 0.5) +
  labs(x="Days after treatment", y ="Value", caption = "Data Source: AAFC, 2023" ) + 
  theme(text = element_text(size = 20, family = "Roboto"), #Futura, Roboto, Helvetica
        panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(size = 32, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "plain"),
        plot.title.position = "plot", 
        plot.caption.position = "plot")
#### end ####

#### surface plot ####
zmatrix <- as.matrix(unname(data))
plt <- plot_ly(z = zmatrix, type = "surface")
plt
#### end ####

#### contour plot ####
contourdata <- melt(as.matrix(unname(data)))
plt <- ggplot(df, aes(Var1, Var2, z= value, colour=stat(level))) +
  geom_contour() +
  scale_color_viridis(option = "plasma") +
  labs(color = "Height in \n m") +
  geom_text_contour(aes(z = value), stroke = 0.2) +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 16),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank()
  )
plt
#### end ####

#SVG GGPlot plots
ggsave('plot.svg', plot = plt, width = 12, height = 7, units = "in", dpi = 600, device = svglite)

#HTML Plotly plots
htmlwidgets::saveWidget(plt, file = "plot.html")
