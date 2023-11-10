#---------------------------------#
# Michelin Restaurant Data
# Data Scraping Script
# Intention: Map of Data
#---------------------------------#

## Inspired by this podcast episode:
    # Economics of Everyday Things
    # https://freakonomics.com/podcast/michelin-stars/

# Set Up #
library(rvest)
library(stringr)
library(dplyr)
#library(ggmap)

## UPDATES NOV 2023 (first started in summer 2023) ##

setwd("/Volumes/AuraByte3/Data Projects/Michelin Restaurants")

# Data Scrape / Michelin Site:
# https://www.zenrows.com/blog/web-scraping-r#retrieve-html
# https://guide.michelin.com/us/en/restaurants/1-star-michelin/2-stars-michelin/3-stars-michelin

# Capture web page:
# STARTING WITH JUST 1-STAR MICHELIN
document <-  read_html("https://guide.michelin.com/us/en/restaurants/1-star-michelin")

## ---- Commented out inital trial & error ---- ##
#restaurant_card_html <- html_nodes(document, '.with-love')
#restaurant_card_html <- html_nodes(document, '.col-xl-3')

#css_selector <- "card__menu-content"

restaurant_card_html <- document %>% html_elements("div.card__menu-content")
title_element <- restaurant_card_html %>% 
  html_element("h3.card__menu-content--title")

title_card_html <- document %>% 
  html_elements("h3.card__menu-content--title") %>%
  html_elements("a")

## TEXT:
title <- document %>% 
  html_elements("h3.card__menu-content--title") %>%
  html_elements("a") %>% 
  html_text2()

location <- document %>% 
  html_elements("div.card__menu-footer--location") %>%
  html_text2()

price_cuisine <- document %>% 
  html_elements("div.card__menu-footer--price") %>% 
  html_text2()

## note on michelin star icon value below: because some have different lengths 
## of image, not same length (20) as prior extractions!
star_icon <- document %>% 
  html_elements("div.card__menu-content--rating") %>% 
  html_elements("span.distinction-icon") %>% 
  html_elements("img.michelin-award") %>% 
  html_attr("src")

# DATA FRAME:
michelin_1star_restaurants <- data.frame(
  title,
  location,
  price_cuisine
)

# parsing price rating and cuisine
parsed_price_cuisine <- strsplit(michelin_1star_restaurants$price_cuisine, split = ' · ')
parsed_price_cuisine_matrix <- matrix(unlist(parsed_price_cuisine),ncol=2,byrow=T)
michelin_1star_restaurants$price <- parsed_price_cuisine_matrix[,1]
michelin_1star_restaurants$cuisine <- parsed_price_cuisine_matrix[,2]

## Next step: Need to conduct this scrape for every page in results:
# E.g. PAGE 1: https://guide.michelin.com/us/en/restaurants/1-star-michelin/page/1
# through PAGE 141: https://guide.michelin.com/us/en/restaurants/1-star-michelin/page/141

## Start building function:

scrape_michelin_data <- function(how_many_stars, number_pages_in_search) {
  # Set up URL:
  url_part1 <- "https://guide.michelin.com/us/en/restaurants/"
  if (how_many_stars == 1) {
    url_part2 <- "-star-michelin/page/"
  } else {
    url_part2 <- "-stars-michelin/page/"
  }
  # Empty data frame:
  michelin_df <- data.frame(matrix(NA, ncol = 3, nrow = 0))
  colnames(michelin_df) <- c("title", "location", "price_cuisine")
  
  for (p in 1:number_pages_in_search) {
    
    # Search result URL:
    search_result_url <- paste0(url_part1, how_many_stars, url_part2, p, "?")
    # Read html:
    html_doc <- read_html(search_result_url)

    # Scraping the search results:
    title <- as.character(
      html_doc %>% 
      html_elements("h3.card__menu-content--title") %>%
      html_elements("a") %>% 
      html_text2()
    )
    
    location <- as.character(
      html_doc %>% 
      html_elements("div.card__menu-footer--location") %>%
      html_text2()
    )
    
    price_cuisine <- as.character(
      html_doc %>% 
      html_elements("div.card__menu-footer--price") %>% 
      html_text2()
    )
    
    # Data frame:
    michelin_df_append <- data.frame(
      title,
      location,
      price_cuisine
    )
    
    # Bind to cumulative data frame:
    michelin_df <- rbind(michelin_df, michelin_df_append)
    
  }
  
  # Clean up data frame:
  michelin_df$price <- str_split_i(michelin_df$price_cuisine, " . ", 1)
  michelin_df$cuisine <- str_split_i(michelin_df$price_cuisine, " . ", 2)
  michelin_df$michelin_stars <- how_many_stars
  # distinction var:
  michelin_df <- 
    michelin_df %>% 
    mutate(distinction = case_when(
      michelin_stars == 1 ~ "1 Star",
      michelin_stars == 2 ~ "2 Stars",
      michelin_stars == 3 ~ "3 Stars"))
  # rename for datawrapper:
  michelin_df <- rename4datawrapper(michelin_df, 'China Mainland', 'China')

  # Return data frame:
  return(michelin_df)
  
}

## MENTAL NOTE:
# I think the order changes because the michelin site is taking user location
# into account. So something about the request being made from R versus when I
# search on my browser shows different outcomes? But now at least distinct
# when adding the "?" at end of URL!

test <- scrape_michelin_data(3,7)
scrape_michelin_data(1,1)

michelin_1star <- scrape_michelin_data(1,142) # 142 pages in search result
michelin_2star <- scrape_michelin_data(2,25) # 25 pages in search result
michelin_3star <- scrape_michelin_data(3,7) # 7 pages in search result

# Check duplicates:
length(duplicated(michelin_1star)=="FALSE") # 2836
length(duplicated(michelin_2star)=="FALSE") # 487
length(duplicated(michelin_3star)=="FALSE") # 140

michelin_restaurants <- rbind(michelin_1star, michelin_2star, michelin_3star)

length(duplicated(michelin_restaurants)=="FALSE") # 3463

# Data wrapper location matching
rename4datawrapper <- function(data_set, old_name, new_name) {
  michelin_restaurants <- 
    data_set %>%
    mutate(across('location', str_replace, old_name, new_name))
  return(michelin_restaurants)
}

# only for michelin 2 star:
michelin_2star <- rename4datawrapper(michelin_2star, 'Leynavatn, Denmark', 'Leynavatn')

## Made into function (above)
# # Change China Mainland to China
    # michelin_restaurants <- 
    #   michelin_restaurants %>%
    #   mutate(across('location', str_replace, 'China Mainland', 'China'))
# # Change "Leynavatn, Denmark" to "Leynavatn"
    # michelin_restaurants <- 
    #   michelin_restaurants %>%
    #   mutate(across('location', str_replace, 'Leynavatn, Denmark', 'Leynavatn'))

## Add a couple more details:
  ## Added to function above:
    # michelin_restaurants <- 
    #   michelin_restaurants %>% 
    #   mutate(distinction = case_when(
    #     michelin_stars == 1 ~ "1 Star",
    #     michelin_stars == 2 ~ "2 Stars",
    #     michelin_stars == 3 ~ "3 Stars"))


write.csv(michelin_restaurants,"Data/michelin_all.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

write.csv(michelin_1star,"Data/michelin_1star.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

write.csv(michelin_2star,"Data/michelin_2stars.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

write.csv(michelin_3star,"Data/michelin_3stars.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")





## Natural Language Processing Data Srape
library("rvest")
# Load the page
main.page <- read_html("https://www.google.com/search?q=software%20programming")

#  <div class = "yuRUbf"> seems to contain the link of first result for multiple results
# within that, a href is the url... 




