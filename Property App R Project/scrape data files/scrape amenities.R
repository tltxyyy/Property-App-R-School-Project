# Instructions:
# 1) call getEachPage() to scrape listings in multiple pages - put in link and # of pages to scrape
# 2) call getEachListing() to scrape all listings in a page -  need to put a vector of page w/o domain

library(rvest)
library(tidyverse) 
# functions to scrape SRX inside each page and each lisitng
domain <- "https://www.srx.com.sg"

getEachListing <- function(page.urllist) { #the listing links w/o domain
  
  joined <- data.frame(amenity=character())

  for (i in 1:length(page.urllist)) {
    
    Sys.sleep(sample(1:10,1,replace = T))
    
    listingurl <- paste0(domain, page.urllist[i])
    pg <- read_html(listingurl)
    
    # train
    latitude <- pg %>% html_nodes(".Trains .listing-amenity") %>% html_attr("amenity-lat")
    longitude <- pg %>% html_nodes(".Trains .listing-amenity") %>% html_attr("amenity-lng")
    amenity <- pg %>% html_nodes(".Trains .listing-amenity-name") %>% html_text()
    if (length(amenity) > 0 ) {
      type <- "Train"
    } else {
      type <- c()
    }
    compile <- data.frame(cbind(amenity,latitude,longitude, type))
    
    # bus stop
    latitude <- pg %>% html_nodes(".Bus-Stops .listing-amenity") %>% html_attr("amenity-lat")
    longitude <- pg %>% html_nodes(".Bus-Stops .listing-amenity") %>% html_attr("amenity-lng")
    amenity <- pg %>% html_nodes(".Bus-Stops .listing-amenity-name") %>% html_text()
    if (length(amenity) > 0 ) {
      type <- "Bus Stop"
    } else {
      type <- c()
    }
    compile <- rbind(compile, cbind(amenity,latitude,longitude, type))
    
    # sch
    latitude <- pg %>% html_nodes(".Schools .listing-amenity") %>% html_attr("amenity-lat")
    longitude <- pg %>% html_nodes(".Schools .listing-amenity") %>% html_attr("amenity-lng")
    amenity <- pg %>% html_nodes(".Schools .listing-amenity-name") %>% html_text()
    if (length(amenity) > 0 ) {
      type <- "School"
    } else {
      type <- c()
    }
    compile <- rbind(compile, cbind(amenity,latitude,longitude, type))
    
    # mkt
    latitude <- pg %>% html_nodes(".Markets .listing-amenity") %>% html_attr("amenity-lat")
    longitude <- pg %>% html_nodes(".Markets .listing-amenity") %>% html_attr("amenity-lng")
    amenity <- pg %>% html_nodes(".Markets .listing-amenity-name") %>% html_text()
    if (length(amenity) > 0 ) {
      type <- "Market"
    } else {
      type <- c()
    }
    compile <- rbind(compile, cbind(amenity,latitude,longitude, type))
    
    # mall
    latitude <- pg %>% html_nodes(".Shopping-Malls .listing-amenity") %>% html_attr("amenity-lat")
    longitude <- pg %>% html_nodes(".Shopping-Malls .listing-amenity") %>% html_attr("amenity-lng")
    amenity <- pg %>% html_nodes(".Shopping-Malls .listing-amenity-name") %>% html_text()
    if (length(amenity) > 0 ) {
      type <- "Shopping Mall"
    } else {
      type <- c()
    }
    compile <- rbind(compile, cbind(amenity,latitude,longitude, type))    
    
    # join everything
    joined <- unique(rbind(joined, compile))
    close(url(listingurl))
  }
  return(joined)
}


getEachPage <- function(home.link, last.page) {
  
  start.time <- Sys.time()
  
  srxMaster <- data.frame(amenity=character())
  pg <- read_html(home.link)
  close(url(home.link))
  for (i in 1:last.page) {
    Sys.sleep(sample(1:8,1,replace = T))
    if (i != 1) {
      nextUrl <- pg %>% html_nodes(".active+ li a") %>% html_attr("href")
      pgl <- paste0(domain, nextUrl)
      pg <- read_html(pgl)
      close(url(pgl))
    }
    page.urllist <- pg %>% html_nodes(".title-row .listingDetailTitle") %>% html_attr("href")
    onePgOfSRX <- getEachListing(page.urllist)
    srxMaster <- unique(rbind(srxMaster, onePgOfSRX))
    print(paste0("Page #", i))
  }
  final_data <<- srxMaster
}
