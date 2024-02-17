library(httr)
library(tidyverse)

# Finding out the widths and heights of the paintings by Pekka Halonen at finna.fi
#
# Copy the URL given from the search performed in Swagger (imitating the search in GUI)
# https://api.finna.fi/swagger-ui/?url=%2Fapi%2Fv1%3Fswagger#/Search/get_search

get_resultcount <- function(p) {
  # Note limit=0 to get only the count
  url <- paste0("https://api.finna.fi/api/v1/search?lookfor=Halonen%2BPekka&type=Author&field%5B%5D=measurements&field%5B%5D=formats&field%5B%5D=nonPresenterAuthors&field%5B%5D=year&field%5B%5D=title&field%5B%5D=id&filter%5B%5D=format%3A%220%2FWorkOfArt%2F%22&filter%5B%5D=format_ext_str_mv%3A%221%2FWorkOfArt%2FPainting%2F%22&sort=relevance%2Cid%20asc&page=",
                p, "&limit=0&prettyPrint=false&lng=fi")
  resp <- GET(url)
  cont <- content(resp, as = "parsed", type = "application/json")
  rescount <- cont$resultCount
  return(rescount)
}

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

clean <- function(l) {
  d <- l %>% {
    tibble(
      id = map_chr(., "id"),
      title = map_chr(., "title"),
      year = map_chr(., "year"),
      size = map(., "measurements")
    )
  }
  
  d2 <- d %>% 
    unnest_longer(size) %>% 
    group_by(title, year) %>% 
    mutate(size_comb = paste0(size, collapse = " ")) %>% 
    ungroup() %>% 
    select(-size)
  
  d3 <- distinct(d2, id, .keep_all = TRUE)
  
  cleaned <- d3 %>% 
    # Different ways to tell the size:
    #
    # teosmitat: 21,5 x 22,0 cm
    # korkeus: 60,0 cm leveys: 42,0 cm
    # kehyksen korkeus 43,5 cm ; kehyksen leveys 44 cm
    # koko: 82,0 x 64,0 cm
    # 138 x 89 cm
    # 12,7 x 17,0 cm valoaukko: 15,5 x 12,0 kehys: 32,0 x 27,5
    mutate(h = str_extract(size_comb, "[\\d\\,]+"),
           # (?<=) is a positive lookbehind meaning before "[\\d\\,]+" is either "x ", "leveys" or "mitat",
           # the two last ones with optional ":" and space
           w = str_extract(size_comb, "(?<=x\\s|leveys\\:?\\s?|mitat\\:?)[\\d\\,]+")) %>% 
    select(-size_comb)

  return(cleaned)
}

# Call the first page, and return the result count
count <- get_resultcount(1)
times <- c(1, ceiling(count/90))

# Fetch all items and clean them
d <- map(.x = times,
         .f = get_paintings)

c <- map_df(.x = d,
            .f = clean)

