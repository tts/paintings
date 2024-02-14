library(rvest)
library(tidyverse)

get_works <- function(url) {
  
  # No text element in these
  image_cells <- url %>% 
    read_html() %>%
    html_nodes(xpath = "//td[1][not(@*)]") %>% # to get also the empty ones
    as.character() 
  images <- gsub("<td>\n</td>", NA, image_cells)
  images <- stringr::str_extract(string = images, pattern = "upload.wikimedia.org[^\\s]+")
  images <- gsub("\\\"", " ", images)
  
  titles <- url %>% 
    read_html() %>%
    html_nodes(xpath='//td[2]//a') %>% 
    html_text() %>% 
    {if(length(.) == 0) NA else .} 
  element_2_remove = c("lähde?")
  titles <-  titles[!(titles %in% element_2_remove)]
  
  years <- url %>% 
    read_html() %>%
    html_nodes(xpath='//td[3]') %>% 
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  heights <- url %>% 
    read_html() %>%
    html_nodes(xpath='//td[5]') %>% 
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  widths <- url %>% 
    read_html() %>%
    html_nodes(xpath='//td[6]') %>% 
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  df <- data.frame(images, titles, years, heights, widths)
  
  df <- df %>% 
    mutate(years = gsub("\\[.*\\]", "", years), # remove references
           heights = gsub("\\[.*\\]", "", heights),
           widths = gsub("\\[.*\\]", "", widths),
           heights = gsub("\\,", ".", heights),
           widths = gsub("\\,", ".", widths),
           widths = as.numeric(widths),
           heights = as.numeric(heights),
           images = ifelse(!is.na(images), paste0("https://", images), images),
           imagetitles = ifelse(!is.na(images), titles, images)) %>% 
    rename(year = years,
           h = heights,
           w = widths,
           link = images,
           link_name = imagetitles,
           title = titles) %>%
    mutate(year = trimws(year),
           link = trimws(link),
           link_name = trimws(link_name),
           title = trimws(title)) %>% 
    select(link, link_name, title, year, h, w)
  
  return(df)
}

halonen <- get_works("https://fi.wikipedia.org/wiki/Luettelo_Pekka_Halosen_maalauksista")
schjerfbeck <- get_works("https://fi.wikipedia.org/wiki/Luettelo_Helene_Schjerfbeckin_maalauksista")
danielsongambogi <- get_works("https://fi.wikipedia.org/wiki/Luettelo_Elin_Danielson-Gambogin_maalauksista")

write_rds(halonen, "halonen.RDS")
write_rds(schjerfbeck, "schjerfbeck.RDS")
write_rds(danielsongambogi, "danielsongambogi.RDS")


# See also https://stackoverflow.com/a/45901675
