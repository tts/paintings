library(httr)
library(tidyverse)

# Finding out the dimensions of paintings by selected artists at finna.fi
#
# Copy the URL given from the search performed in Swagger (imitating the search made in GUI)
# https://api.finna.fi/swagger-ui/?url=%2Fapi%2Fv1%3Fswagger#/Search/get_search

get_resultcount <- function(lookfor) {
  url <- paste0("https://api.finna.fi/api/v1/search?lookfor=", lookfor, 
                "&type=Author&field%5B%5D=measurements&field%5B%5D=formats&field%5B%5D=nonPresenterAuthors&field%5B%5D=year&field%5B%5D=title&field%5B%5D=id&filter%5B%5D=format%3A%220%2FWorkOfArt%2F%22&filter%5B%5D=format_ext_str_mv%3A%221%2FWorkOfArt%2FPainting%2F%22&sort=relevance%2Cid%20asc&page=1&limit=0&prettyPrint=false&lng=fi")
  resp <- GET(url)
  cont <- content(resp, as = "parsed", type = "application/json")
  rescount <- cont$resultCount
  return(rescount)
}

get_paintings <- function(lookfor, p) {
  url <- paste0("https://api.finna.fi/api/v1/search?lookfor=", lookfor, 
                "&type=Author&field%5B%5D=measurements&field%5B%5D=formats&field%5B%5D=nonPresenterAuthors&field%5B%5D=year&field%5B%5D=title&field%5B%5D=id&filter%5B%5D=format%3A%220%2FWorkOfArt%2F%22&filter%5B%5D=format_ext_str_mv%3A%221%2FWorkOfArt%2FPainting%2F%22&sort=relevance%2Cid%20asc&page=",
                p, "&limit=90&prettyPrint=false&lng=fi")
  resp <- GET(url)
  cont <- content(resp, as = "parsed", type = "application/json")
  return(cont)
}

clean <- function(d) {
  
  cleaned <- d %>% 
    filter(nchar(size)>0) %>% 
    # Different ways to tell the size:
    #
    # teosmitat: 21,5 x 22,0 cm
    # korkeus: 60,0 cm leveys: 42,0 cm
    # kehyksen korkeus 43,5 cm ; kehyksen leveys 44 cm
    # koko: 82,0 x 64,0 cm
    # 138 x 89 cm
    # 12,7 x 17,0 cm valoaukko: 15,5 x 12,0 kehys: 32,0 x 27,5
    # korkeus\n\t\t\t\t\t\t\t29,7\n\t\t\t\t\t\tcm
    # kehyksineen korkeus (?) 102 cm   ; kehyksineen leveys (?) 157 cm
    # teosmitat, koko teos kehyksineen : 200,0 x 413,0 cm
    mutate(size = gsub("\n|\t|\\(\\?\\)", "", size),
           h = str_extract(size, "[\\d\\,]+"),
           # (?<=) is a positive lookbehind meaning before "[\\d\\,]+" is either "x ", "leveys" or "mitat",
           # the two last ones with optional ":" and space(s)
           w = str_extract(size, "(?<=x\\s|leveys\\:?\\s?\\s?|mitat\\:?)[\\d\\,]+"),
           h = gsub(",", ".", h),
           w = gsub(",", ".", w),
           h = as.numeric(h),
           w = as.numeric(w)) %>% 
    select(-size)

  return(cleaned)
}

#-----------------------------
# Vector of selected artists
#-----------------------------

who <- c("Pekka Halonen",
         "Helene Schjerfbeck",
         "Elin Danielson-Gambogi",
         "Albert Edelfelt",
         "Akseli Gallen-Kallela",
         "Eero Järnefelt",
         "Juho Rissanen",
         "Maria Wiik",
         "Fanny Churberg",
         "Hjalmar Munsterhjelm")

whourl <- map_chr(who, URLencode)
count <- map_int(whourl, get_resultcount)
pages <- map_int(count, function(x) ceiling(x/90))

# Fetch items by page
works_list <- map2(.x = whourl,
                   .y = pages,
                   .f = ~{
                     rep <- seq(1, .y, 1)
                     map2(.x, rep, get_paintings)
                   }, .progress = TRUE) 

# Pick items from this nested list to a list
l <- map_depth(.x = works_list,
               .depth = 2,
               .f = ~ {
                 .x$records %>% {
                   tibble(
                     id = map_chr(., "id"),
                     artist = map_chr(., ~ .x$nonPresenterAuthors[[1]]$name),
                     title = map_chr(., "title"),
                     year = map_chr(., "year", .default = NA_character_),
                     size = map_chr(., ~ paste(.x$measurements, collapse = " "), .default = NA_character_)
                    ) 
                  }
                })

# Bind rows of the list to get a data frame
df <- map_depth(.x = l, .depth = 0, .f = bind_rows)

dfcleaned <- clean(df) 
