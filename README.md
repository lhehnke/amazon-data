# amazon-data
Scraping functions for (1) Amazon customer reviews and (2) product information from best sellers list

## scraper_amazon_best_sellers.R

Function for scraping Amazon product URLs and IDs (ASIN) from best sellers list

## scraper_amazon_product_reviews.R

Function for scraping Amazon customer reviews from specified review page

## scraping_amazon_customer_reviews.R

Script for automated scraping of all customer reviews for one or more Amazon products (with product ID/ASIN as input); returns data frame with the following information:

   * product ID
   * title of review
   * author of review
   * date
   * verified purchase (yes/no)
   * format (paperback, kindle etc.)
   * stars
   * comments
   * helpful
   * reviewer URL
   * reviewer ID

*(Basic Amazon scraper adapted and extended from: https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R)*

