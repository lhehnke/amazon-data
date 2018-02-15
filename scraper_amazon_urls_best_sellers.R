##-----------------------------------------------------------------------------------##
##          FUNCTION FOR SCRAPING PRODUCT URLS FROM AMAZON BEST SELLERS LIST         ##
##-----------------------------------------------------------------------------------##


## R version 3.4.3 (2017-11-30)

## Author: Lisa Hehnke || lhehnke.github.io || @DataPlanes


#-------#
# Setup #
#-------#

# Install and load packages using pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(rvest, xml2)


#-----------------------#
# Scrape URLs from list #
#-----------------------#

# Create HTML of best sellers
## Example: https://www.amazon.com/gp/bestsellers/books/3377866011/ref=pd_zg_hrsr_books_1_2
list <- read_html("https://www.amazon.com/gp/bestsellers/books/3377866011/ref=pd_zg_hrsr_books_1_2")

# Get URLs for all pages of list
## Alternatively: add #2, #3, #4, and #5 to base URL
list_pages <- list %>%
  html_nodes("#zg_paginationWrapper") %>%
  html_nodes("a") %>% 
  html_attr("href") 


#---------------------------------#
# Function to scrape product URLs #
#---------------------------------#

# Function to scrape product URLs for all products on list
product_url_scraper <- function(doc){
  
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(rvest)

    product_urls <- doc %>%
    html_nodes("div.zg_itemImmersion") %>%
    html_nodes("a.a-link-normal") %>%
    html_attr("href") %>%
    grep("(.*)product-reviews(.*)", invert = TRUE, value = TRUE, .) %>%
    unique() 
  
  return(product_urls)
}


#---------------------#
# Scrape product URLs #
#---------------------#

# Function to scrape URLs
get_product_urls <- function(url) {
  url %>%
    read_html() %>%
    product_url_scraper() %>%
    paste0("https://www.amazon.com", .) 
} 

# Get all product URLs from best sellers list
product_urls <- unlist(lapply(list_pages, get_product_urls))