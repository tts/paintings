library(httr)
library(tidyverse)
library(data.table)

# Finding out the widths of the paintings by Pekka Halonen at finna.fi
#
# Copy the URL given from the search performed in Swagger (imitating the search in GUI)
# https://api.finna.fi/swagger-ui/?url=%2Fapi%2Fv1%3Fswagger#/Search/get_search

get_paintings <- function(p) {
  url <- paste0("https://api.finna.fi/api/v1/search?lookfor=Halonen%2BPekka&type=Author&field%5B%5D=measurements&field%5B%5D=formats&field%5B%5D=nonPresenterAuthors&field%5B%5D=year&field%5B%5D=title&field%5B%5D=id&filter%5B%5D=format%3A%220%2FWorkOfArt%2F%22&filter%5B%5D=format_ext_str_mv%3A%221%2FWorkOfArt%2FPainting%2F%22&sort=relevance%2Cid%20asc&page=",
                p, "&limit=90&prettyPrint=false&lng=fi")
  resp <- GET(url)
  cont <- content(resp, as = "parsed", type = "application/json")
  
  # Measurements can be empty so keep only non-empty list elements at level 1, resulting in 5 sub-elements
  map_depth(cont$records, 1, function(x) keep(x, lengths(x) > 0)) -> cont_non_empty
  # And then discard its parent element
  map_depth(cont_non_empty, 0, function(x) discard(x, lengths(x)==5)) -> cont_ready
  
  return(cont_ready)  
}

# For simplicity, manually calling the two pages.
# Normally, would need to look at the resultCount element value and calculate the number of calls.
works1 <- get_paintings(1)
works1_dt <- lapply(works1, as.data.table) 
works1_df <- rbindlist(works1_dt, fill = TRUE)

works2 <- get_paintings(2)
works2_dt <- lapply(works2, as.data.table) 
works2_df <- rbindlist(works2_dt, fill = TRUE)

works <- bind_rows(works1_df, works2_df)

works %>% 
  bind_rows %>% 
  select(measurements, year, title, id) %>% 
  distinct() -> works_all

# The length of the work is given in four different ways
works_all %>% 
  filter(str_detect(measurements, '^leveys\\:')) %>% 
  mutate(measurements = gsub("leveys\\: ", "", measurements)) -> works_leveys

works_all %>% 
  filter(str_detect(measurements, "teosmitat\\:|koko\\:")) %>% 
  mutate(measurements = gsub("teosmitat\\: ", "", measurements),
         measurements = gsub("koko\\: ", "", measurements),
         measurements = gsub("Kuvan ", "", measurements),
         measurements = gsub("(.+?) x (.+?)", "\\2", measurements)) -> works_teosmitat

works_all %>% 
  filter(str_detect(measurements, "^\\d")) %>% 
  mutate(measurements = gsub("(.+?) x (.+?)", "\\2", measurements)) -> works_digit

works_measures <- rbind(works_leveys, works_teosmitat, works_digit)

works_measures %>% 
  mutate(measurements = gsub(" cm", "", measurements),
         measurements = gsub("\\,", ".", measurements),
         measurements = as.numeric(measurements),
         url = paste0("https://finna.fi/Record/", id)) %>% 
  rename(width = measurements) %>% 
  arrange(desc(width)) %>%   
  select(-id) -> works_ready 

saveRDS(works_ready, "works.RDS")

