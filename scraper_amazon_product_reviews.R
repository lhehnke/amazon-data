##-----------------------------------------------------------------------------------##
##                   SCRAPING FUNCTION FOR AMAZON PRODUCT REVIEWS                    ##
##-----------------------------------------------------------------------------------##


## R version 3.4.3 (2017-11-30)

## Author: Lisa Hehnke || lhehnke.github.io || @DataPlanes


#----------#
# Function #
#----------#

## Based on: https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R

get_reviews <- function(node){
  
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(dplyr, rvest, xml2)
  
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


#----------------#
# Scrape reviews #
#----------------#

# Read HTML
## Example: https://www.amazon.com/Fire-Fury-Inside-Trump-White/product-reviews/1250158060/ref=cm_cr_arp_d_viewopt_srt?ie=UTF8&reviewerType=all_reviews&sortBy=recent&pageNumber=1
node <- read_html("https://www.amazon.com/Fire-Fury-Inside-Trump-White/product-reviews/1250158060/ref=cm_cr_arp_d_viewopt_srt?ie=UTF8&reviewerType=all_reviews&sortBy=recent&pageNumber=1") %>% html_nodes("div[id*=customer_review]")

# Get reviews
reviews <- lapply(node, get_reviews) %>% bind_rows()