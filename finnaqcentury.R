library(httr)
library(tidyverse)

# Finding out the dimensions of all paintings at finna.fi published between 1800 and 1900

get_resultcount_century <- function(lookfor = NULL) {
  url <- paste0("https://api.finna.fi/api/v1/search?lookfor=", lookfor, 
                "&field%5B%5D=measurements&field%5B%5D=formats&field%5B%5D=nonPresenterAuthors&field%5B%5D=year&field%5B%5D=title&field%5B%5D=id&filter%5B%5D=format%3A%220%2FWorkOfArt%2F%22&filter%5B%5D=format_ext_str_mv%3A%221%2FWorkOfArt%2FPainting%2F%22&filter%5B%5D=search_daterange_mv:%22[1800%20TO%201900]%22&sort=relevance%2Cid%20asc&page=1&limit=0&prettyPrint=false&lng=fi")
  resp <- GET(url)
  cont <- content(resp, as = "parsed", type = "application/json")
  rescount <- cont$resultCount
  return(rescount)
}

get_paintings_century <- function(p) {
  url <- paste0("https://api.finna.fi/api/v1/search?field%5B%5D=measurements&field%5B%5D=formats&field%5B%5D=nonPresenterAuthors&field%5B%5D=year&field%5B%5D=title&field%5B%5D=id&filter%5B%5D=format%3A%220%2FWorkOfArt%2F%22&filter%5B%5D=format_ext_str_mv%3A%221%2FWorkOfArt%2FPainting%2F%22&filter%5B%5D=search_daterange_mv:%22[1800%20TO%201900]%22&sort=relevance%2Cid%20asc&page=",
                p, "&limit=90&prettyPrint=false&lng=fi")
  resp <- GET(url)
  cont <- content(resp, as = "parsed", type = "application/json")
  return(cont)
}

clean <- function(d) {
  
  cleaned <- d %>% 
    filter(nchar(size)>0) %>% 
    mutate(size = gsub("\n|\t|\\(\\?\\)", "", size),
         h = str_extract(size, "[\\d\\,]+"),
         w = str_extract(size, "(?<=x\\s|leveys\\:?\\s?\\s?|mitat\\:?)[\\d\\,]+"),
         h = gsub(",", ".", h),
         w = gsub(",", ".", w),
         h = as.numeric(h),
         w = as.numeric(w)) %>% 
    select(-size)
  
  return(cleaned)
}

count <- get_resultcount_century()
pages <- seq(1, ceiling(count/90), 1)

works_list <- map(.x = pages, 
                  .f = get_paintings_century,
                  .progress = TRUE) 

l <- map_depth(.x = works_list,
               .depth = 1,
               .f = ~ {
                 .x$records %>% {
                   tibble(
                     id = map_chr(., "id", .default = NA_character_),
                     artist = map_chr(., ~pluck(.x, "nonPresenterAuthors", 1, "name", .default = NA_character_)),
                     title = map_chr(., "title", .default = NA_character_),
                     year = map_chr(., "year", .default = NA_character_),
                     size = map_chr(., ~ paste(.x$measurements, collapse = " "), .default = NA_character_)
                   ) 
                 }
               })

df <- map_depth(.x = l, .depth = 0, .f = bind_rows)

dfcleaned <- clean(df)

saveRDS(dfcleaned, "centuryworks.RDS")
