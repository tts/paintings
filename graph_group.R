library(tidyverse)
library(ggiraph)
library(ggtext)

halonen <- readRDS("halonen.RDS") %>% 
  mutate(artist = "Halonen",
         col = "h")
schjerfbeck <- readRDS("schjerfbeck.RDS") %>% 
  mutate(artist = "Schjerfbeck",
         col = "s")
danielsongambogi <- readRDS("danielsongambogi.RDS") %>% 
  mutate(artist = "Danielson-Gambogi",
         col = "d")

# When rendered, the result graph looks untidier than the one produced by graph.R
# where line segments are added by artist. Perhaps I should actually group the data.
data <- rbind(halonen, danielsongambogi, schjerfbeck)

data <- data %>% 
  mutate(ratio = w / h) %>% 
        # artist = factor(artist, levels = c("Halonen", "Schjerfbeck", "Danielson-Gambogi"))) %>% 
  rowid_to_column("id")

halonen_max_ratio <- data %>% 
  filter(artist == "Halonen") %>% 
  arrange(desc(ratio)) %>% 
  top_n(1) %>% 
  select(h, w, col)

maxw <- halonen_max_ratio$w
maxh <- halonen_max_ratio$h

gg <- ggplot(data) + 
  geom_point(aes(x = w, y = h, color = col), shape = 0) +
  geom_segment_interactive(
    aes(x = 0, y = h, xend = w, yend = h, colour = col,
        tooltip = paste(artist, title, year, paste0(h, " x ", w), sep = "\n"),
        data_id = id), 
    linewidth = 1, alpha = 0.6) +
  geom_segment_interactive(
    aes(x = w, y = 0, xend = w, yend = h, colour = col,
        tooltip = paste(artist, title, year, paste0(h, " x ", w), sep = "\n"),
        data_id = id),
    linewidth = 1, alpha = 0.6) +
  # geom_segment_interactive(
  #   aes(x = 0, y = 0, xend = maxw, yend = maxh),
  #   linetype = "dashed", color = "cadetblue3") +
  scale_color_manual(values = c("h" = "cadetblue3", "s" = "darkkhaki", "d" = "darkred")) +
  labs(caption="Data: Wikipedia | Kaavio @ttso",
       subtitle="<span style='color:#7ac5cd;'>Pekka Halosen</span>, 
                 <span style='color:#bdb76b;'>Helene Schjerfbeckin</span> ja 
                 <span style='color:#8B0000;'>Elin Danielson-Gambogin</span><br/> maalausten kokoja (cm)",
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
                 opacity = 1))
)

#preview
interactive

htmlwidgets::saveWidget(interactive, "maalaukset.html", selfcontained = T)
