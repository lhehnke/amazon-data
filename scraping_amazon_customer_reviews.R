##-----------------------------------------------------------------------------------##
##                       SCRAPING CUSTOMER REVIEWS FROM AMAZON                       ##
##-----------------------------------------------------------------------------------##


## R version 3.4.3 (2017-11-30)

## Author: Lisa Hehnke || lhehnke.github.io || @DataPlanes
## Thanks to Josef Holnburger (holnburger.com || @holnburger) for helping.


#-------#
# Setup #
#-------#

# Install and load packages using pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(rvest, tidyverse)


#-----------------------------------#
# Function to scrape Amazon reviews #
#-----------------------------------#

## Based on: https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R

amazon_scraper_mod <- function(node, delay = 0){
  
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(RCurl, XML, xml2, dplyr, stringr, rvest, audio)
  
  sec = 0
  if(delay < 0) warning("delay was less than 0: set to 0")
  if(delay > 0) sec = max(0, delay + runif(1, -1, 1))

  r.product_id <- html_nodes(node, ".a-link-normal") %>%
    html_attr("href") %>%
    gsub(".*ASIN=","", .)
  
  r.title <- html_nodes(node, ".a-color-base") %>%
    html_text() 
  
  r.author <- html_nodes(node, ".author") %>%
    html_text() 
  
  r.date <- html_nodes(node, ".review-date") %>%
    html_text() %>%
    gsub(".*on ", "", .)
  
  r.ver.purchase <- html_nodes(node, ".review-data.a-spacing-mini") %>% 
    html_text() %>% grepl("Verified Purchase", .) %>% 
    as.numeric()
  
  r.format <- html_nodes(node, ".review-data.a-spacing-mini") %>% 
    html_text() %>% 
    gsub("Color: |\\|.*|Verified.*", "", .)
  
  r.stars <- html_nodes(node, ".review-rating") %>% 
    html_text() %>% 
    str_extract("\\d") %>% 
    as.numeric() 
  
  r.comments <- html_nodes(node, ".review-text") %>% 
    html_text()
  
  r.helpful <- html_nodes(node, ".cr-vote-buttons .a-color-secondary") %>% 
    html_text() %>% str_extract("[:digit:]+|One") %>%
    gsub("One", "1", .) %>%
    as.numeric()
  
  r.rver_url <- html_nodes(node, ".author") %>%
    html_attr("href") %>%
    gsub("/ref=cm_cr_getr_d_pdp?ie=UTF8", "", .) %>%
    paste0("https://www.amazon.com", .) 
  
  r.rver_id <- r.rver_url %>%
    gsub("https://www.amazon.com/gp/profile/amzn1.account.", "", .)  %>%
    gsub("/ref=cm_cr_arp_d_pdp?ie=UTF8", "", fixed = TRUE, .) %>%
    gsub("https://www.amazon.com", NA, .) 
  
  df <- data.frame(
    product_id = ifelse(length(r.product_id) == 0, NA, r.product_id),
    title = ifelse(length(r.title) == 0, NA, r.title),
    author = ifelse(length(r.author) == 0, NA, r.author), 
    date = ifelse(length(r.date) == 0, NA, r.date), 
    ver.purchase = ifelse(length(r.ver.purchase) == 0, NA, r.ver.purchase),
    format = ifelse(length(r.format) == 0, NA, r.format),
    stars = ifelse(length(r.stars) == 0, NA, r.stars), 
    comments = ifelse(length(r.comments) == 0, NA, r.comments), 
    helpful = ifelse(length(r.helpful) == 0, NA, r.helpful), 
    rver_url = ifelse(length(r.rver_url) == 0, NA, r.rver_url), 
    rver_id = ifelse(length(r.rver_id) == 0, NA, r.rver_id), 
    stringsAsFactors = F)
  
  return(df)  
}


#-------------------------------#
# Scrape required data for URLs #
#-------------------------------#

# Create URLs from product IDs (ASIN)
## Example: https://www.amazon.com/Data-Science-Transform-Visualize-Model/dp/1491910399/ref=sr_1_3?ie=UTF8&qid=1518683139&sr=8-3&keywords=R+statistics
id <- c("1491910399")
urls_pages <- paste0("https://www.amazon.com/product-reviews/", id, "/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews") 

# Function to scrape page numbers of review pages
get_numbers <- function(url) {
  pages_raw <- url %>%
    html_session() %>%
    html_nodes("#cm_cr-pagination_bar") %>%
    html_text() 

  pages <- ifelse(is.null(pages_raw), 0, pages_raw)
  return(pages)
} 

# Extract max. page number and replace NA with 1
numbers_raw <- unlist(lapply(urls_pages, get_numbers))
pages <- sub(".*Previous(.*?)Next.*", "\\1", numbers_raw) 
pages <- ifelse(grepl(".*\\.\\.\\.", pages), gsub(".*\\.\\.\\.","", pages), gsub(".*(.)$", "\\1", pages))
pages[is.na(pages)] <- 1

# Create data.frame with product ID and max. page number
books <- data.frame(id, pages)

# Function to create full URLs with page numbers of reviews
get_urls <- function(x) {
  id <- x[1]
  pages <- x[2]
  num <- seq(1, pages, 1)
  urls <- paste0("https://www.amazon.com/product-reviews/", id, "/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews&pageNumber=", num, "&sortBy=recent") 
  
  return(urls)
}

# Get all URLs for scraping
urls <- unlist(apply(books, 1, get_urls))


#-------------------------#
# Scrape customer reviews #
#-------------------------#

# Function to scrape reviews
get_reviews <- function(url) {
  url %>%
    read_html() %>% 
    html_nodes("div[id*=customer_review]") %>% 
    lapply(., amazon_scraper_mod) %>% bind_rows()
} 

# Get reviews
reviews <- lapply(urls, get_reviews)

# Convert list to data.frame
reviews_df <- do.call(rbind, lapply(reviews, data.frame))