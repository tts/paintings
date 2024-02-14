library(shiny)
library(tidyverse)
library(ggiraph)
library(ggtext)

halonen <- readRDS("halonen.RDS")
schjerfbeck <- readRDS("schjerfbeck.RDS")
danielsongambogi <- readRDS("danielsongambogi.RDS")

gg <- ggplot() + 
  geom_point(data = halonen, aes(x = w, y = h), colour = "cadetblue3", shape = 0) +
  geom_point(data = schjerfbeck, aes(x = w, y = h), colour = "darkkhaki", fill = "darkkhaki", shape = 0) +
  geom_point(data = danielsongambogi, aes(x = w, y = h), colour = "darkred", fill = "darkred", shape = 0) +
  geom_segment_interactive(aes(x = 0, y = h, 
                               xend = w, yend = h, 
                               tooltip = paste("Halonen:", title, year, paste0(h, " x ", w), sep = "\n"), 
                               data_id = title), colour = "cadetblue3", size = 1,
                           data = halonen) +
  geom_segment_interactive(aes(x = w, y = 0, 
                               xend = w, yend = h, 
                               tooltip = paste("Halonen:", title, year, paste0(h, " x ", w), sep = "\n"), 
                               data_id = title), colour = "cadetblue3", size = 1,
                           data = halonen) +
  geom_segment_interactive(aes(x = 0, y = h, 
                               xend = w, yend = h, 
                               tooltip = paste("Schjerfbeck:", title, year, paste0(h, " x ", w), sep = "\n"), 
                               data_id = title), colour = "darkkhaki", size = 1,
                           data = schjerfbeck) +
  geom_segment_interactive(aes(x = w, y = 0, 
                               xend = w, yend = h, 
                               tooltip = paste("Schjerfbeck:", title, year, paste0(h, " x ", w), sep = "\n"), 
                               data_id = title), colour = "darkkhaki", size = 1,
                           data = schjerfbeck) +
  geom_segment_interactive(aes(x = 0, y = h, 
                               xend = w, yend = h, 
                               tooltip = paste("Danielson-Gambogi:", title, year, paste0(h, " x ", w), sep = "\n"), 
                               data_id = title), colour = "darkred", size = 1,
                           data = danielsongambogi) +
  geom_segment_interactive(aes(x = w, y = 0, 
                               xend = w, yend = h, 
                               tooltip = paste("Danielson-Gambogi:", title, year, paste0(h, " x ", w), sep = "\n"), 
                               data_id = title), colour = "darkred", size = 1,
                           data = danielsongambogi) +
  geom_jitter_interactive(width = 0.45, height = 0.45) +
  labs(caption="Data: Wikipedia | Kaavio @ttso",
       subtitle="<span style='color:#7ac5cd;'>Pekka Halosen</span>, 
                 <span style='color:#bdb76b;'>Helene Schjerfbeckin</span> ja 
                 <span style='color:#8B0000;'>Elin Danielson-Gambogin</span> maalausten kokoja (cm)",
       title="Korkeus x leveys") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(face="bold", size = 17),
        plot.subtitle = element_markdown())

tooltip_css <- "background-color:white;color:black;font-family:sans-serif;padding:3px"

interactive <- girafe(
  ggobj = gg,
  options = list(
    opts_tooltip(css = tooltip_css, 
                 opacity = 1)),
  width_svg = 9, height_svg = 5.25
)

#preview
interactive

htmlwidgets::saveWidget(interactive, "maalaukset.html", selfcontained = T)
