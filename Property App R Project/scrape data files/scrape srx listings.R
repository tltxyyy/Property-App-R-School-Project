library(tidyr)
library(rvest)
library(tidyverse) # gglplot2, dplyr, stringr, readr
library(curl)
library(XML)

#function for one listing
single.listing <- function(listinglink)
{
  listing.pg <- read_html(listinglink)
  close(url(listinglink))
  
  #reading about this property table
  all.details.key <-listing.pg %>% html_nodes(".listing-about-main-key") %>% html_text(., trim = T)
  all.details.value <- listing.pg %>% html_nodes(".listing-about-main-value") %>% html_text(., trim = T)
  
  #facilities
  Facilities <- listing.pg %>% html_nodes(".listing-about-facility-span") %>% html_text(., trim = T)
  Facilities <- paste(Facilities, collapse = ", ")
  
  #trains
  Trains <- listing.pg %>% html_nodes(".Trains .listing-amenity-name") %>% html_text(., trim = T)
  Trains <- paste(Trains, collapse = ", ")
  
  #bustops
  Bus.Stops <- listing.pg %>% html_nodes(".Bus-Stops .listing-amenity-name") %>% html_text(., trim = T)
  Bus.Stops <- paste(Bus.Stops, collapse = ", ")
  
  #primary schools
  Primary.Schools <- listing.pg %>% html_nodes(".Bus-Stops+ .Schools .listing-amenity") %>% html_text(., trim = T)
  Primary.Schools <- paste(Primary.Schools, collapse = ", ")
  
  #secondary schools
  Secondary.Schools <- listing.pg %>% html_nodes(".Schools:nth-child(4) .listing-amenity") %>% html_text(., trim = T)
  Secondary.Schools <- paste(Secondary.Schools, collapse = ", ")
  
  #integrated schools
  Integrated.Schools <- listing.pg %>% html_nodes(".Schools~ .Schools+ .Schools .listing-amenity") %>% html_text(., trim = T)
  Integrated.Schools <- paste(Integrated.Schools, collapse = ", ")
  
  #shopping malls
  Shopping.Malls <- listing.pg %>% html_nodes(".Shopping-Malls .listing-amenity") %>% html_text(., trim = T)
  Shopping.Malls <- paste(Shopping.Malls, collapse = ", ")
  
  #marts
  Marts <- listing.pg %>% html_nodes(".Markets .listing-amenity") %>% html_text(., trim = T)
  Marts <- paste(Marts, collapse = ",")
  
  #coordinates 
  latitude <- listing.pg %>% html_nodes("#listing-latitude") %>% html_attr("value")
  longitude <- listing.pg %>% html_nodes("#listing-longitude") %>% html_attr("value")

  all.details.df <- data.frame(all.details.key, all.details.value)
  
  all.details.df <- all.details.df %>% pivot_wider(names_from = all.details.key, values_from = all.details.value)
  
  temporary <- data.frame(Facilities, Trains, Bus.Stops, Primary.Schools, Secondary.Schools, Integrated.Schools, Shopping.Malls, Marts, latitude, longitude)
  all.details.df <- cbind(all.details.df, temporary)
  
  return(all.details.df)
}


#function for one page
one.page <- function(page.link)
{
  main_page <- read_html(page.link)
  sub.link.nodes <- main_page %>% html_nodes(".insideListingTitleRow .listingDetailTitle") %>% html_attr("href")
  data <- data.frame()
  for(i in 1:length(sub.link.nodes)){
    Sys.sleep(sample(1:10,1))
    try(listing <- paste0("https://www.srx.com.sg",sub.link.nodes[i]))
    listing.data <- single.listing(listing)
    data <- bind_rows(data,listing.data)
    #print(i)
  }
  close(url(page.link))
  return(data)
}

#final function for multiple pages
multiple.page.srx <- function(home_page, pages)
{
  data = data.frame()
  for(i in 1:pages){
    Sys.sleep(sample(1:10, 1))
    if(i!=1){
      next_url <- read_html(home_page) %>% html_nodes(".active + li a") %>% html_attr("href") 
      home_page <- paste0("https://www.srx.com.sg", next_url)
    }
    onepageofsrx <- one.page(home_page)
    data <- bind_rows(data, onepageofsrx)
    print(paste0("Page #", i))
  }
  final_data <<- data
}