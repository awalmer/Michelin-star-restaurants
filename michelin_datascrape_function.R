# -------------------------------------------------------------------------
# Michelin Star Data Scrape Function
# Anonymized R Script for GitHub
# April, 2024
# Aura (Auralee) Walmer | https://aurawalmer.com/
# -------------------------------------------------------------------------


# Set Up ------------------------------------------------------------------

library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(plyr)

# setwd()

# Functions ---------------------------------------------------------------

# Data wrapper location matching:
rename_location <- function(data_set, old_name, new_name) {
  michelin_restaurants <- 
    data_set %>%
    mutate(across('location', str_replace, old_name, new_name))
  return(michelin_restaurants)
}

# Scrape Michelin data:
scrape_michelin_data <- function(how_many_stars, number_pages_in_search) {
  
  # Build the URL:
  url_part1 <- "https://guide.michelin.com/us/en/restaurants/"
  if (how_many_stars == 1) {
    url_part2 <- "-star-michelin/page/" # singular
  } else {
    url_part2 <- "-stars-michelin/page/" # plural
  }
  
  # Initial empty data frame:
  #michelin_df <- data.frame("title"="","location"="","price_cuisine"="")
  michelin_df <- data.frame(matrix(NA, ncol = 3, nrow = 0))
  # colnames(michelin_df) <- c("title", "location", "price_cuisine")
  
  # For each result page, scrape restaurant data (loop):
  for (p in 1:number_pages_in_search) {
    
    # Search result URL:
    search_result_url <- paste0(url_part1, how_many_stars, url_part2, p, "?")
    # Note: Adding "?" at end of URL stabilizes URL location
    # Read html:
    html_doc <- read_html(search_result_url)
    
    # Capture restaurant titles:
    title <- as.character(
      html_doc %>% 
        html_elements("h3.card__menu-content--title") %>%
        html_elements("a") %>% 
        html_text2()
    )
    
    # Capture restaurant locations, price tier, and cuisine:
    # Note: as of 2024, HTML elements of Michelin website changed.
    lpc_vector <- as.vector(
      html_doc %>%
        html_elements("div.align-items-end") %>%
        html_text2())[2:21] # provides "\n" separated character block
    
    lpc_table <- tibble(lpc_vector) # convert to table for extraction
    lpc_table_separated <- lpc_table %>% 
      separate_wider_delim(lpc_vector, "\n", names = c("location", "price_cuisine"))
    
    # Append elements to Michelin data frame:
    michelin_df_append <- 
      data.frame("title"=title,
                 "location"=lpc_table_separated$location,
                 "price_cuisine"=lpc_table_separated$price_cuisine)
    
    michelin_df <- rbind(michelin_df, michelin_df_append)
    
  } # end loop
  
  # Clean / reformat data frame & add new variables:
  michelin_df$price <- str_split_i(michelin_df$price_cuisine, " . ", 1)
  michelin_df$cuisine <- str_split_i(michelin_df$price_cuisine, " . ", 2)
  michelin_df$price_cuisine <- NULL
  michelin_df$michelin_stars <- how_many_stars
  # Sometimes the location is city + country, or just country.
  # Create country variable from conditional comma split:
  michelin_df$country <- 
    ifelse(grepl(", ", michelin_df$location), # condition
           str_split_i(michelin_df$location, ", ", 2), # case if condition true
           michelin_df$location # case if condition false (no comma)
    )
  
  # Create distinction variable in character format for labeling:
  michelin_df <- 
    michelin_df %>% 
    mutate(distinction = case_when(
      michelin_stars == 1 ~ "1 Star",
      michelin_stars == 2 ~ "2 Stars",
      michelin_stars == 3 ~ "3 Stars"))
  
  # Rename certain countries:
  michelin_df <- rename_location(michelin_df, 'China Mainland', 'China')
  michelin_df <- rename_location(michelin_df, 'Leynavatn, Denmark', 'Leynavatn')
  
  # Return data frame:
  return(michelin_df)
  
}

# Test:
test <- scrape_michelin_data(1,1)


