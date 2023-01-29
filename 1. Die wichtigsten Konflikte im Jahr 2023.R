# Laden sämtlicher für die Datenanalyse relevanten R-Packete:
library(maps)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# Erstellung eines Datentabelle der Grenzen saemtlicher Länder der Welt mit map_data():
map <- map_data("world") %>%
  select(-subregion)

# Erstellung einer Datentabelle mit data.frame, die saemtliche Laendernamen der von der ICG in "10 Conflicts to Watch in 2023" gelisteten wichtigsten Konflikte für 2023 enthaelt mit (online unter: https://www.crisisgroup.org/global/10-conflicts-watch-2023):
icg_conflict <- data.frame(region = c("Ukraine","Armenia","Azerbaijan","Iran","Yemen",
                                      "Ethiopia","Democratic Republic of the Congo","Haiti","Burkina Faso","Mali",
                                      "Niger","Pakistan","Taiwan"),
                           conflict = rep(1,13)
)

# Vereinigung der beiden Datentabellen mit left_join():
map_icg_conflict <- left_join(map,icg_conflict, by = "region") %>%
  na.omit()

# Gestaltung des themes fuer die Karten von "War and Peace and Data Science":
theme_wpd_map <-  theme(plot.title = element_text(size = 17.5, face = "bold", hjust = 0.5),
                        plot.subtitle = element_text(size = 10),
                        axis.title = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        axis.line = element_blank(),
                        legend.title = element_text(face = "bold"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "black"),
                        panel.grid.major.x = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank()
)

# Visualisierung der wichtigsten Konflikte für 2023 nach International Crisis Group:
map1 <- map %>%
  ggplot( aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "gray90") +
  geom_polygon(data = map_icg_conflict, aes(x = long, y = lat, group = group), color = "black", fill = brewer.pal(9,"Reds")[8]) +
  labs(title = "Die zehn wichtigsten bewaffneten Konflikte und Konfliktherde 2023",
       caption = "Quelle: International Crisis Group") +
  theme_wpd_map +
  theme(legend.position = "none")

ggdraw() +
  draw_plot(map1) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)