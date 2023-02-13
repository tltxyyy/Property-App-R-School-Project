library(tidyr)
library(rgdal)
library(geojsonio)
library(leaflet)
library(jsonlite)
library(tidyverse)
library(mapview)
library(viridis)
library(openxlsx)
library(ggmap)
library(geosphere)
library(DT)
library(plotly)
library(ggplot2)
library(ggalt)
library(readxl)


# All the datasets: porperties, properties1, properties2, sgplanning1, sg_subzone_proj, housing_grants
# properties 
properties <- read.csv("./final data files/properties dataset.csv")
new_districts_tbl <- read.xlsx("./final data files/districts_final.xlsx")
options(scipen = 999)

properties$Asking <- parse_number(properties$Asking)
properties$Size.builtup <- parse_number(sub(".+?/", "", properties$Size))
properties$PSF.builtup <- parse_number(sub(".+?/", "", properties$PSF))
properties.function <- properties
properties$district <- parse_number(properties$District)
properties$district <- sprintf("%02d",properties$district)
properties <- properties %>% left_join(new_districts_tbl, by=("district"))
properties <- properties %>% separate_rows(location, sep= ", ") 
properties$location <- sapply(properties$location, str_trim)
location_asking <- properties %>% dplyr::group_by(location) %>% summarise(median_price = median(Asking, na.rm = TRUE))
colnames(location_asking)[1] <- "PLN_AREA_N" 

# properties 1
properties1 <- read.csv("./final data files/properties dataset.csv")

# all_amenities
all_amenities <- read.csv("./final data files/amen_joined_final.csv")

#bto_prices
bto_prices <- read.csv("./final data files/newbtoprices.csv")

# BTOproperties
BTOproperties <- properties1 %>% filter(property.class == "HDB-BTO")

# sg_planning ,sg_subzone
sg_planning <- readOGR("./final data files/MP14_PLNG_AREA_WEB_PL.shp")
sg_planning_proj <- spTransform(sg_planning, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sg_planning_proj <- merge(sg_planning_proj, location_asking, by = 'PLN_AREA_N', all.x=TRUE)
sg_subzone <- readOGR("./final data files/MP14_SUBZONE_WEB_PL.shp")
sg_subzone_proj <- spTransform(sg_subzone, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sg_subzone_proj <- merge(sg_subzone_proj, location_asking, by.x = 'SUBZONE_N', by.y = "PLN_AREA_N", all.x=TRUE)
pal <- colorNumeric("viridis", NULL, reverse=T)


# housing_grants
Housing_grants <- read_excel("./final data files/Housing_grants_final.xlsx")

properties2 <- properties1 %>% mutate(property.type.hdb = gsub("HDB ", "", .$Property.Type))
properties2$property.type.hdb <- gsub("HDB-BTO ", "", properties2$property.type.hdb)

Housing_grants[Housing_grants$HDB_Type == "Resale", ]$HDB_Type <- "HDB"
Housing_grants[Housing_grants$HDB_Type == "Resale / BTO", ]$HDB_Type <- "HDB-BTO"
Housing_grants[Housing_grants$Citizenship == "SC", ]$Citizenship <- "Singaporean"
Housing_grants[Housing_grants$Citizenship == "SC/SPR", ]$Citizenship <- "Singaporean/Singaporean PR"

# all_transactions
all_transactions <- read.csv("./final data files/all_transactions.csv")
all_transactions$district <- str_pad(all_transactions$district, 2, pad="0")
all_transactions$psf <- all_transactions$resale_price / all_transactions$area
new_districts_tbl <- read_xlsx("./final data files/districts_final.xlsx")
# new_districts_tbl$district <- str_pad(new_districts_tbl$district, 2, pad="0")
resale_flats <- read.csv("./final data files/resale_flats_w_lat_long_merged.csv")
resale_flats$district <- gsub("D", "", resale_flats$district)
resale_flats$district <- str_pad(resale_flats$district, 2, pad="0")



  # All functions

# fx 1: home page 1st chart
Property_Visualisation <- function(house.address=c(), property.type=c(), property.model=c(), property.bedroom=c(), property.bathrooms=c(), property.furnish=c(), property.floor=c(), property.tenure=c(), property.developer=c(), property.no.of.units=c(), property.asking=c(), property.size=c(), property.psf=c(), property.builtyear=c(), property.top=c(), property.newlaunch=c(), pricing.heatmap=c(TRUE)){
  
  filter.address <- if(is_empty(house.address) == TRUE){
    properties.function
  }else{
    properties.function %>% filter(grepl(house.address, Address, ignore.case = TRUE) | grepl(house.address, Property.Name, ignore.case = TRUE) | grepl(house.address, District, ignore.case = TRUE) | grepl(house.address, Property.Name, ignore.case = TRUE) | grepl(house.address, property.class, ignore.case = TRUE))
  }
  
  filter.property.type <- if(is_empty(property.type) == TRUE){
    filter.address
  }else{
    filter.address %>% filter(Property.Type %in% property.type)
  }
  
  
  filter.property.model <- if(is_empty(property.model) == TRUE){
    filter.property.type
  }else{
    filter.property.type %>% filter(Model %in% property.model)
  }
  
  
  filter.property.bedroom <- if(is_empty(property.bedroom) == TRUE){
    filter.property.model
  }else{
    filter.property.model %>% filter(Bedrooms %in% property.bedroom)
  }
  
  
  filter.property.bathroom <- if(is_empty(property.bathrooms) == TRUE){
    filter.property.bedroom
  }else{
    filter.property.bedroom %>% filter(Bathrooms %in% property.bathrooms)
  }
  
  
  filter.property.furnish <- if(is_empty(property.furnish) == TRUE){
    filter.property.bathroom
  }else{
    filter.property.bathroom %>% filter(Furnish %in% property.furnish)
  }
  
  
  filter.property.floor <- if(is_empty(property.floor) == TRUE){
    filter.property.furnish
  }else{
    filter.property.furnish %>% filter(Floor.Level %in% property.floor)
  }
  
  
  filter.property.tenure <- if(is_empty(property.tenure) == TRUE){
    filter.property.floor
  }else{
    filter.property.floor %>% filter(Tenure %in% property.tenure)
  }
  
  
  filter.property.developer <- if(is_empty(property.developer) == TRUE){
    filter.property.tenure
  }else{
    filter.property.tenure %>% filter(Developer %in% property.developer)
  }
  
  
  filter.no.of.units <- if(is_empty(property.no.of.units) == TRUE){
    filter.property.developer
  }else if (property.no.of.units[1] == 0){
    filter.property.developer %>% filter((No..of.Units >= property.no.of.units[1] & No..of.Units <= property.no.of.units[2]) %>% replace_na(TRUE))
  }else{
    filter.property.developer %>% filter((No..of.Units >= property.no.of.units[1] & No..of.Units <= property.no.of.units[2]))
  }
  
  
  filter.asking <- if(is_empty(property.asking) == TRUE){
    filter.no.of.units
  }else if (property.asking[1] == 0){
    filter.no.of.units %>% filter((Asking >= property.asking[1] & Asking <= property.asking[2]) %>% replace_na(TRUE))
  }else{
    filter.no.of.units %>% filter((Asking >= property.asking[1] & Asking <= property.asking[2]))
  }
  
  
  filter.size <- if(is_empty(property.size) == TRUE){
    filter.asking
  }else if(property.size[1] == 0){
    filter.asking %>% filter((Size.builtup >= property.size[1] & Size.builtup <= property.size[2]) %>% replace_na(TRUE))
  }else{
    filter.asking %>% filter((Size.builtup >= property.size[1] & Size.builtup <= property.size[2]))
  }
  
  
  filter.psf <- if(is_empty(property.psf) == TRUE){
    filter.size
  }else if (property.psf[1] == 0){
    filter.size %>% filter((PSF.builtup >= property.psf[1] & PSF.builtup <= property.psf[2]) %>% replace_na(TRUE))
  }else{
    filter.size %>% filter((PSF.builtup >= property.psf[1] & PSF.builtup <= property.psf[2]))
  }
  
  
  filter.builtyear <- if(is_empty(property.builtyear) == TRUE){
    filter.psf
  }else{
    filter.psf %>% filter(Built.Year %in% property.builtyear)
  }
  
  
  filter.top <- if(is_empty(property.top) == TRUE){
    filter.builtyear
  }else{
    filter.builtyear %>% filter(TOP %in% property.top)
  }
  
  
  filter.newlaunch <- if(property.newlaunch == "1"){
    filter.top
  }else if(property.newlaunch == "2"){
    filter.top %>% filter(new_launch == 1)
  }else{
    filter.top %>% filter(new_launch == 0)
  }
  
  filter.newlaunch$Asking <- format(filter.newlaunch$Asking, big.mark = ",", scientific = FALSE)
  
  
  if(dim(filter.newlaunch)[1]== 0){
    printMessage <- c("No houses available, please filter again.")
  } else {
    printMessage <- c("Search Successful!")
  }
  
  if(pricing.heatmap == "2"){
    asking_leaflet <- leaflet() %>% 
      addTiles() %>% 
      setView(lng=103.8198, lat=1.352083, zoom = 11) %>%
      addMapPane("Planning", zIndex = 420) %>%
      addMapPane("Subzone", zIndex = 410) %>%
      addPolygons(data=sg_planning_proj, weight = 2, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8, fillColor = ~pal(log10(median_price)), label= paste0("<strong>Name: </strong>", sg_planning_proj$PLN_AREA_N, "<br>", "<strong>Median Price: </strong>$", format(sg_planning_proj$median_price, big.mark = ",", scientific = FALSE), "<br>") %>% lapply(htmltools::HTML), group = "Planning", options = pathOptions(pane = "Planning")) %>%
      addLegend(pal = pal, title = "Median Transaction Price ($)", values = log10(sg_planning_proj$median_price), opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(10^x))) %>%
      addPolygons(data=sg_subzone_proj, weight = 2, fillColor = "yellow", label=paste0("<strong>Name: </strong>", sg_subzone_proj$SUBZONE_N, "<br>", "<strong>Median Price:</strong> $", sg_subzone_proj$median_price, "<br>") %>% lapply(htmltools::HTML), group="Subzone", options = pathOptions(pane = "Subzone")) %>%
      addAwesomeMarkers(data=filter.newlaunch, lng = ~longitude, lat = ~latitude, icon = awesomeIcons(icon = 'glyphicon-home', iconColor = "white", markerColor = "pink"), clusterOptions = markerClusterOptions(), popup = paste0("<strong>Project Name: </strong>", filter.newlaunch$Property.Name, "<br>", "<strong>Asking Price: $ </strong>", filter.newlaunch$Asking, "<br>", "<strong>No. of Bedrooms: </strong>", filter.newlaunch$Bedrooms, "<br>", "<strong>No. of Bathrooms: </strong>", filter.newlaunch$Bathrooms, "<br>", "<strong>PSF: </strong>", filter.newlaunch$PSF, "<br>", "<strong>District: </strong>", filter.newlaunch$District, "<br>", "<strong>Size: </strong>", filter.newlaunch$Size, "<br>", "<strong>Property Type: </strong>", filter.newlaunch$Property.Type, "<br>") %>% lapply(htmltools::HTML), group="Properties") %>%  addLayersControl(baseGroups = c("Planning"), overlayGroups = c("Subzone", "Properties"))
  }else{
    asking_leaflet <- leaflet() %>% addTiles() %>% setView(lng=103.8198, lat=1.352083, zoom = 11) %>%
      addAwesomeMarkers(data=filter.newlaunch, lng = ~longitude, lat = ~latitude, 
                        icon = awesomeIcons(icon = 'glyphicon-home', iconColor = "white", markerColor = "pink"),
                        clusterOptions = markerClusterOptions(), 
                        popup = paste0("<strong>Project Name: </strong>", filter.newlaunch$Property.Name, "<br>",
                                       "<strong>Asking Price: </strong>$", filter.newlaunch$Asking, "<br>", 
                                       "<strong>No. of Bedrooms: </strong>", filter.newlaunch$Bedrooms, "<br>",
                                       "<strong>No. of Bathrooms: </strong>", filter.newlaunch$Bathrooms, "<br>",
                                       "<strong>PSF: </strong>", filter.newlaunch$PSF, "<br>", 
                                       "<strong>District: </strong>", filter.newlaunch$District, "<br>", 
                                       "<strong>Size: </strong>", filter.newlaunch$Size, "<br>", 
                                       "<strong>Property Type: </strong>", filter.newlaunch$Property.Type))
    
    
  }
  
  
  newlist <- list(asking_leaflet, printMessage)
  
  return(newlist)
  
}

# fx 1b: search output
Property_Table <- function(house.address=c(), property.type=c(), property.model=c(), property.bedroom=c(), property.bathrooms=c(), property.furnish=c(), property.floor=c(), property.tenure=c(), property.developer=c(), property.no.of.units=c(), property.asking=c(), property.size=c(), property.psf=c(), property.builtyear=c(), property.top=c(), property.newlaunch=c(), pricing.heatmap=c(TRUE)){
  
  
  filter.address <- if(is_empty(house.address) == TRUE){
    properties.function
  }else{
    properties.function %>% filter(grepl(house.address, Address, ignore.case = TRUE) | grepl(house.address, Property.Name, ignore.case = TRUE) | grepl(house.address, District, ignore.case = TRUE) | grepl(house.address, Property.Name, ignore.case = TRUE) | grepl(house.address, property.class, ignore.case = TRUE))
  }
  
  filter.property.type <- if(is_empty(property.type) == TRUE){
    filter.address
  }else{
    filter.address %>% filter(Property.Type %in% property.type)
  }
  
  
  filter.property.model <- if(is_empty(property.model) == TRUE){
    filter.property.type
  }else{
    filter.property.type %>% filter(Model %in% property.model)
  }
  
  
  filter.property.bedroom <- if(is_empty(property.bedroom) == TRUE){
    filter.property.model
  }else{
    filter.property.model %>% filter(Bedrooms %in% property.bedroom)
  }
  
  
  filter.property.bathroom <- if(is_empty(property.bathrooms) == TRUE){
    filter.property.bedroom
  }else{
    filter.property.bedroom %>% filter(Bathrooms %in% property.bathrooms)
  }
  
  
  filter.property.furnish <- if(is_empty(property.furnish) == TRUE){
    filter.property.bathroom
  }else{
    filter.property.bathroom %>% filter(Furnish %in% property.furnish)
  }
  
  
  filter.property.floor <- if(is_empty(property.floor) == TRUE){
    filter.property.furnish
  }else{
    filter.property.furnish %>% filter(Floor.Level %in% property.floor)
  }
  
  
  filter.property.tenure <- if(is_empty(property.tenure) == TRUE){
    filter.property.floor
  }else{
    filter.property.floor %>% filter(Tenure %in% property.tenure)
  }
  
  
  filter.property.developer <- if(is_empty(property.developer) == TRUE){
    filter.property.tenure
  }else{
    filter.property.tenure %>% filter(Developer %in% property.developer)
  }
  
  
  filter.no.of.units <- if(is_empty(property.no.of.units) == TRUE){
    filter.property.developer
  }else if (property.no.of.units[1] == 0){
    filter.property.developer %>% filter((No..of.Units >= property.no.of.units[1] & No..of.Units <= property.no.of.units[2]) %>% replace_na(TRUE))
  }else{
    filter.property.developer %>% filter((No..of.Units >= property.no.of.units[1] & No..of.Units <= property.no.of.units[2]))
  }
  
  
  filter.asking <- if(is_empty(property.asking) == TRUE){
    filter.no.of.units
  }else if (property.asking[1] == 0){
    filter.no.of.units %>% filter((Asking >= property.asking[1] & Asking <= property.asking[2]) %>% replace_na(TRUE))
  }else{
    filter.no.of.units %>% filter((Asking >= property.asking[1] & Asking <= property.asking[2]))
  }
  
  
  filter.size <- if(is_empty(property.size) == TRUE){
    filter.asking
  }else if(property.size[1] == 0){
    filter.asking %>% filter((Size.builtup >= property.size[1] & Size.builtup <= property.size[2]) %>% replace_na(TRUE))
  }else{
    filter.asking %>% filter((Size.builtup >= property.size[1] & Size.builtup <= property.size[2]))
  }
  
  
  filter.psf <- if(is_empty(property.psf) == TRUE){
    filter.size
  }else if (property.psf[1] == 0){
    filter.size %>% filter((PSF.builtup >= property.psf[1] & PSF.builtup <= property.psf[2]) %>% replace_na(TRUE))
  }else{
    filter.size %>% filter((PSF.builtup >= property.psf[1] & PSF.builtup <= property.psf[2]))
  }
  
  
  filter.builtyear <- if(is_empty(property.builtyear) == TRUE){
    filter.psf
  }else{
    filter.psf %>% filter(Built.Year %in% property.builtyear)
  }
  
  
  filter.top <- if(is_empty(property.top) == TRUE){
    filter.builtyear
  }else{
    filter.builtyear %>% filter(TOP %in% property.top)
  }
  
  
  filter.newlaunch <- if(property.newlaunch == "1"){
    filter.top
  }else if(property.newlaunch == "2"){
    filter.top %>% filter(new_launch == 1)
  }else{
    filter.top %>% filter(new_launch == 0)
  }
  
  
  if(dim(filter.newlaunch)[1]== 0){
    
    print("No houses available")
  }
  
  #add format fx for ,
  filter.newlaunch$Asking <- trimws(format(filter.newlaunch$Asking, big.mark = ",", scientific = FALSE))
  filter.newlaunch$Asking <- paste0("$", filter.newlaunch$Asking)
  filter.newlaunch <- filter.newlaunch %>% mutate(Property.Desc = paste0(Address, " / ", Property.Name, " / ", Asking, " / ", index))
  
  final.table <- as.data.frame(t(filter.newlaunch))
  names(final.table) <- final.table["Property.Desc", ]
  final.table <- final.table[-1,]
  
  return(final.table)
  
}

# fx 2: home page 2nd chart & table
properties_searched <- properties1
properties_searched$Asking <- parse_number(properties_searched$Asking)
properties_searched$Asking <- trimws(format(properties_searched$Asking, big.mark = ",", scientific = FALSE))
properties_searched$Asking <- paste0("$", properties_searched$Asking)
properties_searched <- properties_searched %>% mutate(Property.Desc = paste0(Address, " / ", Property.Name, " / ", Asking, " / ", index))

showOnePropertyonMapnTable <- function(mutatedIdentidier) {
  theProperty <- properties_searched[properties_searched$Property.Desc == mutatedIdentidier, ] %>% dplyr::select(-c("index", "new_launch"))
  theProperty <- unique(theProperty)
  
  theMap <- leaflet() %>% addTiles() %>%
    addAwesomeMarkers(data=theProperty, lng = ~longitude, lat = ~latitude, icon = awesomeIcons(icon = 'glyphicon-home', iconColor = "white", markerColor = "pink"), popup = paste0("<strong>Address: </strong>", theProperty$Address, "<br/><strong>Property Name: </strong>", theProperty$Property.Name, "<br/><strong>Asking Price: </strong>",theProperty$Asking))
  
  theTable <- as.data.frame(t(theProperty))
  theTable$Details <- rownames(theTable)
  theTable <- theTable[,c(2,1)]
  names(theTable) <- theTable[1,]
  theTable <- theTable[-1,]
  
  showPropDetails <- list(theMap, theTable)
  return(showPropDetails)
}

# fx 3: fav page amenities
getAmenityRadiusMapnTable <- function(favourited, select_radius=400) { # favourited is a vector of property index
  saved_fav <- properties1[favourited, ]
  
  marker_options <- markerOptions(
    zIndexOffset = 100
  )
  
  a <- leaflet(saved_fav) %>% addTiles() %>%
    addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon = awesomeIcons(icon = 'glyphicon-home', iconColor = "white", markerColor = "gray") ,popup = paste0("<strong> Property: </strong>", saved_fav$Property.Name, "</br><strong> Address: </strong>", saved_fav$Address, "</br> <strong>Asking Price: </strong>", saved_fav$Asking), options = marker_options) %>%
    addCircles(data= saved_fav, lng = ~longitude, lat = ~latitude, radius = select_radius, color = "blue", opacity = 0.3, weight = 2)
  
  amenity.type <- unique(all_amenities$type)
  
  fav_amenities <- c()
  add_to_map <- data.frame()
  for (j in 1:length(favourited)) {
    for (i in 1:nrow(all_amenities)) {
      long <- all_amenities[i,][["longitude"]]
      lat <- all_amenities[i,][["latitude"]]
      dist_metres <- distm(unlist(saved_fav[j, c("longitude", "latitude")]), c(long, lat), fun = distHaversine)
      if (dist_metres <= select_radius) {
        property_index <- favourited[j]
        amen_in_radius <- data.frame(all_amenities$amenity[i], long, lat, all_amenities$type[i], dist_metres, property_index)
        add_to_map <- rbind(add_to_map, amen_in_radius)
      }
    }
  }
  
  colnames(add_to_map) <- c("amenity", "longitude", "latitude", "type", "distance", "property index")
  
  getColor <- function(all_amenities) {
    sapply(all_amenities$type, function(type) {
      if(type == "Primary School") {
        "lightred"
      } else if(type == "Secondary School") {
        "red"
      } else if(type == "Pre-School") {
        "beige"
      } else if(type == "Train") {
        "blue"
      } else if(type == "Bus Stop") {
        "lightblue"
      } else if(type == "Market") {
        "orange"
      } else if(type == "Shopping Mall") {
        "cadetblue"
      }  else if(type == "Hawker Centre") {
        "purple"
      }  else if(type == "Parks") {
        "green"
      } else {
        "black"
      }
    })
  }
  
  getIcon <- function(all_amenities) {
    sapply(all_amenities$type, function(type) {
      if(type == "Primary School") {
        "pencil"
      } else if(type == "Secondary School") {
        "graduation-cap"
      } else if(type == "Pre-School") {
        "child"
      } else if(type == "Train") {
        "subway"
      } else if(type == "Bus Stop") {
        "bus"
      } else if(type == "Market") {
        "shopping-basket"
      } else if(type == "Shopping Mall") {
        "shopping-cart"
      }  else if(type == "Hawker Centre") {
        "cutlery" 
      }  else if(type == "Parks") {
        "tree"
      }
    })
  }
  
  amen_icons <- awesomeIcons(
    icon = as.character(getIcon(add_to_map)),
    iconColor = '#fff',
    library = 'fa',
    markerColor = as.character(getColor(add_to_map))
  )
  

  html_legend <- "<div style='padding: 10px; padding-bottom: 10px;'>
    <h5 style='padding-top:0; padding-bottom:5px; margin: 0;'> Legend </h5>
    <h6 style='font-size: 10px'><i class='fa fa-child' aria-hidden='true'></i> Pre-Schools</h6>
    <h6 style='font-size: 10px'><i class='fa fa-pencil' aria-hidden='true'></i> Primary Schools</h6>
    <h6 style='font-size: 10px'><i class='fa fa-graduation-cap' aria-hidden='true'></i> Secondary Schools</h6>
    <h6 style='font-size: 10px'><i class='fa fa-train' aria-hidden='true'></i> MRT Station</h6>
    <h6 style='font-size: 10px'><i class='fa fa-bus' aria-hidden='true'></i> Bus Stops</h6>
    <h6 style='font-size: 10px'><i class='fa fa-shopping-basket' aria-hidden='true'></i> Markets</h6>
    <h6 style='font-size: 10px'><i class='fa fa-shopping-cart' aria-hidden='true'></i> Shopping Malls</h6>
    <h6 style='font-size: 10px'><i class='fa fa-cutlery' aria-hidden='true'></i> Hawker Centres</h6>
    <h6 style='font-size: 10px'><i class='fa fa-tree' aria-hidden='true'></i> Parks</h6>
  </div>"
  
  getPrettyLabels <- labelOptions(
    direction = "top",
    style = list(
      "color" = "black",
      "font-family" = "serif",
      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
      "font-size" = "12px",
      "border-color" = "rgba(0,0,0,0.5)",
      "background-color" = "beige"
    )
  )
  
  a <- addAwesomeMarkers(a, lng = add_to_map$long,lat = add_to_map$lat, icon = amen_icons, label = paste0(add_to_map$amenity),popup = paste0("<strong>", add_to_map$amenity, "</strong>", "</br> Distance from Property: ", round(add_to_map$distance), "m"), labelOptions = getPrettyLabels, group = add_to_map$type) %>%
    addLayersControl(overlayGroups = amenity.type) %>%
    addControl(html = html_legend, position = "bottomleft")
  
  # creating data table
  property_details_table <- as.data.frame(t(saved_fav)) %>% rownames_to_column("Details") #dplyr::
  
  amenities_table <- add_to_map %>% mutate(amen_dist = paste0(amenity, " (", round(distance), "m) ")) %>% 
    dplyr::group_by(`property index`, type) %>% mutate(amen_str = paste0(amen_dist, collapse = ", ")) %>% 
    dplyr::select(`property index`, type, amen_str)
  amenities_table <- unique(amenities_table) %>% tidyr::spread(`property index`, amen_str) %>% rename("Details" = "type")
  
  all_details <- rbind(property_details_table, amenities_table) %>% filter(Details != c("new_launch", "index"))
  
  names(all_details) <- all_details[1,]
  all_details <- all_details[-1,]
  
  # return two things
  mapNtable <- list(a, all_details)
  
  return(mapNtable)
}



# fx 4: fav page topN
getAmenityn_nearest <- function(location, n) {
  amenities <- c()
  for (i in 1:nrow(all_amenities)) {
    long <- all_amenities[i,][["longitude"]] 
    lat <- all_amenities[i,][["latitude"]]
    dist_metres <- distm(location, c(long, lat), fun = distHaversine)
    amenity1 <-data.frame(amenity = all_amenities$amenity[i], dist_metres = round(dist_metres), lat, long, type = all_amenities$type[i])
    amenities <- rbind(amenities, amenity1)
  }
  amenities <- amenities %>% arrange(type, dist_metres) %>% group_by(type) %>% slice_head(n=n)
  return(amenities)
}

vis_n_nearest <- function(favourited, n)
{
  property <- properties1[properties1$index %in% favourited,]
  
  #first creating a vector of four regions used in the data:
  amen.list <- unique(all_amenities$type)
  
  marker_options <- markerOptions(
    zIndexOffset = 100
  )
  
  m <-leaflet() %>%
    addTiles() %>%
    addAwesomeMarkers(data = property, lng = ~longitude, lat = ~latitude, icon = awesomeIcons(icon = 'glyphicon-home', iconColor = "white", markerColor = "gray"), popup = paste("Property:", property[,"Property.Name"], "<br>", "Asking:", property[,"Asking"], "<br>", "Size:", property[,"Size"] ), options = marker_options)
  
  
  all <- c()
  for (v in 1:nrow(property)) {
    a <- getAmenityn_nearest(c(property$longitude[v], property$latitude[v]), n)
    a$property.name <- property$Property.Name[v]
    a$index <- property$index[v]
    all <- bind_rows(all, a)
  }
  
  getColor <- function(amenities_df) {
    sapply(amenities_df$type, function(type) {
      if(type == "Primary School") {
        "lightred"
      } else if(type == "Secondary School") {
        "red"
      } else if(type == "Pre-School") {
        "beige"
      } else if(type == "Train") {
        "blue"
      } else if(type == "Bus Stop") {
        "lightblue"
      } else if(type == "Market") {
        "orange"
      } else if(type == "Shopping Mall") {
        "cadetblue"
      }  else if(type == "Hawker Centre") {
        "purple"
      }  else if(type == "Parks") {
        "green"
      } else {
        "black"
      }
    })
  }
  
  getIcon <- function(amenities_df) {
    sapply(amenities_df$type, function(type) {
      if(type == "Primary School") {
        "pencil"
      } else if(type == "Secondary School") {
        "graduation-cap"
      } else if(type == "Pre-School") {
        "child"
      } else if(type == "Train") {
        "subway"
      } else if(type == "Bus Stop") {
        "bus"
      } else if(type == "Market") {
        "shopping-basket"
      } else if(type == "Shopping Mall") {
        "shopping-cart"
      }  else if(type == "Hawker Centre") {
        "cutlery"
      }  else if(type == "Parks") {
        "tree"
      } 
    })
  }
  
  amen_icons <- awesomeIcons(
    icon = as.character(getIcon(all)),
    iconColor = '#fff',
    library = 'fa',
    markerColor = as.character(getColor(all))
  )
  
  html_legend <- "<div style='padding: 10px; padding-bottom: 10px;'>
    <h5 style='padding-top:0; padding-bottom:5px; margin: 0;'> Legend </h5>
    <h6 style='font-size: 10px'><i class='fa fa-child' aria-hidden='true'></i> Pre-Schools</h6>
    <h6 style='font-size: 10px'><i class='fa fa-pencil' aria-hidden='true'></i> Primary Schools</h6>
    <h6 style='font-size: 10px'><i class='fa fa-graduation-cap' aria-hidden='true'></i> Secondary Schools</h6>
    <h6 style='font-size: 10px'><i class='fa fa-train' aria-hidden='true'></i> MRT Station</h6>
    <h6 style='font-size: 10px'><i class='fa fa-bus' aria-hidden='true'></i> Bus Stops</h6>
    <h6 style='font-size: 10px'><i class='fa fa-shopping-basket' aria-hidden='true'></i> Markets</h6>
    <h6 style='font-size: 10px'><i class='fa fa-shopping-cart' aria-hidden='true'></i> Shopping Malls</h6>
    <h6 style='font-size: 10px'><i class='fa fa-cutlery' aria-hidden='true'></i> Hawker Centres</h6>
    <h6 style='font-size: 10px'><i class='fa fa-tree' aria-hidden='true'></i> Parks</h6>
  </div>"
  
  getPrettyLabels <- labelOptions(
    direction = "top",
    style = list(
      "color" = "black",
      "font-family" = "serif",
      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
      "font-size" = "12px",
      "border-color" = "rgba(0,0,0,0.5)",
      "background-color" = "beige"
    )
  )
  
  m <- m %>% addAwesomeMarkers(lng = all$long,lat = all$lat, icon = amen_icons, label = paste0(all$amenity),popup = paste0("<strong>", all$amenity, "</strong>", "</br> Distance from ", all$property.name, ": ", round(all$dist_metres), "m"), labelOptions = getPrettyLabels, group = all$type) %>% 
    addLayersControl(overlayGroups = amen.list) %>%
    addControl(html = html_legend, position="bottomleft")
  
  # creating data table
  property_details_table <- as.data.frame(t(property)) %>% rownames_to_column("Details")
  
  
  amenities_table <- all %>% mutate(amen_dist = paste0(amenity, " (", round(dist_metres), "m) ")) %>%
    dplyr::group_by(index, type) %>% mutate(amen_str = paste0(amen_dist, collapse = ", ")) %>% dplyr::select(index, type, amen_str)
  amenities_table <- unique(amenities_table) %>% spread(index, amen_str) %>% rename("Details" = "type")
  
  
  all_details <- rbind(property_details_table, amenities_table) %>% filter(Details != c("new_launch", "index"))
  
  names(all_details) <- all_details[1,]
  all_details <- all_details[-1,]
  
  mapNtable <- list(m, all_details)
  return(mapNtable)
}

# fx 5: fav page grants eligibility table
GrantsEligibilityTable <- function(favourited, salary, buyer.type, citizenship, Family.single) {
  saved_fav <- properties2[favourited, ]
  
  grant_df <- c()
  
  for (i in 1:nrow(saved_fav)){
    saved_fav_prop <- saved_fav[i,]
    
    if (saved_fav_prop$property.class != "HDB" & saved_fav_prop$property.class != "HDB-BTO"){
      
      filtered.housing.grant <- Housing_grants %>% filter(str_detect(HDB_Type, saved_fav_prop$property.class))
      filtered.housing.grant[1, "index"] <- saved_fav_prop$index
      filtered.housing.grant[1, "Desc"] <- NA
      
    } else {
      filtered.housing.grant <- Housing_grants %>% 
        filter(str_detect(HDB_Type, saved_fav_prop$property.class)) %>% 
        filter(str_detect(Property.Type, saved_fav_prop$property.type.hdb) | is.na(Property.Type)) %>%
        filter(salary >= AMHI_Floor & salary <= AMHI_Ceiling)  %>% 
        filter(Type_of_buyer == buyer.type) %>% 
        filter(Citizenship == citizenship) %>% 
        filter(Family_or_Single == Family.single)
      filtered.housing.grant$index <- saved_fav_prop$index
      filtered.housing.grant <- mutate(filtered.housing.grant, Desc = paste0(Grant_Name, " (", Grant_Amount,")"))
    }
    
    filtered.housing.grant$Address <- saved_fav_prop$Address
    filtered.housing.grant$Property.Name <- saved_fav_prop$Property.Name
    filtered.housing.grant$property.class <- saved_fav_prop$property.class
    grant_df <- rbind(grant_df, filtered.housing.grant)
  }
  
  compare_grants_table <- grant_df %>% 
    mutate(Property = paste0(Address, " / ", Property.Name, " / ", property.class, " / ", index)) %>% 
    group_by(index, Grant_Name) %>% mutate(desc_str = paste0(Desc, collapse = ", "), na.rm=T) %>% 
    ungroup() %>% dplyr::select(Grant_Name, desc_str, Remarks, Property) %>% 
    spread(Property, desc_str) %>% rename("Grant Name" = "Grant_Name")
  compare_grants_table <- compare_grants_table[!is.na(compare_grants_table$`Grant Name`), ]
  
  return(compare_grants_table)
}

# fx 6: grants 4km radius
FindPropertiesEligibleForProxGrant <- function(location) { # location is a string
  hdb_df <- properties2 %>% filter(property.class == "HDB") # only resale is qualified for this grant
  location_coord <- geocode(location)
  
  marker_options <- markerOptions(
    zIndexOffset = 100
  )
  
  m <- leaflet() %>% addTiles() %>% 
    setView(lng = location_coord[[1]], lat = location_coord[[2]], zoom = 13) %>% 
    addCircles(lng = location_coord[[1]], lat = location_coord[[2]], radius = 4000, color = "blue", opacity = 0.3, weight = 2) %>% 
    addAwesomeMarkers(lng = location_coord[[1]], lat = location_coord[[2]], icon=awesomeIcons(icon = "home", iconColor = '#fff', library = 'fa', markerColor = "gray"), options = marker_options, popup = paste0("<strong> Parent's Home </strong>"))
  
  # check whether coordinates available
  checkNa <- is.na(location_coord[[1]] | location_coord[[2]]) 
  if (checkNa == T) {
    errorMessage <- "Please try another address or postal code."
  } else {
    errorMessage <- "Search Successful!"
  }
  
  if (checkNa == F) {
    for (i in 1:nrow(hdb_df)) {
      long <- hdb_df[i,][["longitude"]]
      lat <- hdb_df[i,][["latitude"]]
      if (distm(location_coord, c(long, lat), fun = distHaversine) <= 4000) {
        m <- addAwesomeMarkers(m, lng = long, lat = lat, popup = paste0("<strong> Property Name: </strong>", hdb_df$Property.Name[i], "<br> <strong>Address: </strong>", hdb_df$Address[i], "<br> <strong>Property Type: </strong>", hdb_df$Property.Type[i], "<br> <strong> Asking Price: </strong>", hdb_df$Asking[i]), icon=awesomeIcons(icon = "home", iconColor = '#fff', library = 'fa', markerColor = "pink"))
      }
    }
  }
  
  
  
  grants_result <- list(m, errorMessage)
  
  return (grants_result)
}

# fx 7: fin overall_bto_prices
substr_year <- function(x){
  substr(x, nchar(x)-4+1, nchar(x))
}

bto_prices$year <- substr_year(bto_prices$Launch.Date)
bto_prices$avgprice <- 0.5*(bto_prices$Max.Price + bto_prices$Min.Price)
#getting the psf: ftÂ² = mÂ² * 10.764
bto_prices$avgpsf <- round(bto_prices$avgprice / (10.764 * bto_prices$Internal.Floor.Area.Min..sqm.), 2)

overall_bto_prices <- function(){
  bto_viz <- bto_prices %>% group_by(year) %>% summarise(avg_price = round(mean(avgprice),2)) %>% ggplot(aes(x=year, y=avg_price, group=1)) + geom_line() + geom_point() + ggtitle("Average Price of BTO Launches (2001 - 2021)") + ylab("Price of Launches ($)") + xlab("Year of Launch") + theme(axis.text.x = element_text(angle = 90))
  ggplotly(bto_viz) 
}


# fx 8: fin hdb_plotting
hdb_plotting <- function(Budget){
  resale_flats_median <- resale_flats %>% group_by(blockstreet) %>% slice_max(1) %>% group_by(blockstreet, latitude, longitude) %>% summarise(median_price = median(resale_price))
  
  hdb_location_transaction <- resale_flats %>% group_by(district) %>% summarise(median_price = median(resale_price)) %>% as.data.frame()
  new_districts_tbl$district <- str_pad(new_districts_tbl$district, 2, "left", pad = "0")
  
  hdb_location_transaction <- hdb_location_transaction %>% merge(new_districts_tbl, by.x="district", by.y="district", all.x=T) %>% dplyr::select(location, district, median_price)
  hdb_location_transaction <- hdb_location_transaction %>% separate_rows(location, sep= ", ")
  hdb_location_transaction$location <- sapply(hdb_location_transaction$location, str_trim)
  colnames(hdb_location_transaction)[1] <- "PLN_AREA_N"
  
  # sg_planning <- sg_planning1
  sg_planning_proj <- spTransform(sg_planning, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  sg_planning_proj <- sp::merge(sg_planning_proj, hdb_location_transaction, by = 'PLN_AREA_N', all.x=TRUE, duplicateGeoms =T)
  
  # sg_subzone <- sg_subzone1
  sg_subzone_proj <- spTransform(sg_subzone, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  sg_subzone_proj <- sp::merge(sg_subzone_proj, hdb_location_transaction, by.x = 'SUBZONE_N', by.y = "PLN_AREA_N", all.x=TRUE, duplicateGeoms =T)
  
  pal <- colorNumeric("viridis", NULL, reverse=T)
  
  historical_hdb_leaflet <- leaflet() %>% 
    addTiles() %>% setView(lat = 1.36754925, lng = 103.8367406,	
                           zoom = 11) %>%
    addMapPane("Planning", zIndex = 420) %>%
    addMapPane("Subzone", zIndex = 410) %>%
    addPolygons(data=sg_planning_proj, weight = 2, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8, fillColor = ~pal(log10(median_price)), label= paste0("<strong>Name: </strong>", sg_planning_proj$PLN_AREA_N, "<br>", "<strong>Median Price: </strong>", paste0('$', formatC(sg_planning_proj$median_price, format = 'd', big.mark = ",")), "<br>", "<strong>Likely to Afford: </strong>", sg_planning_proj$median_price <= Budget) %>% lapply(htmltools::HTML), group = "Planning", options = pathOptions(pane = "Planning")) %>%
    addLegend(pal = pal, title = "Median Transaction Price", values = log10(sg_planning_proj$median_price), opacity = 1.0,
              labFormat = labelFormat(transform = function(x) round(10^x))) %>%
    addPolygons(data=sg_subzone_proj, weight = 2, fillColor = "yellow", label=paste0("<strong>Name: </strong>", sg_subzone_proj$SUBZONE_N, "<br>", "<strong>Median Price: </strong>", paste0("$", sg_subzone_proj$median_price, format = 'd', big.mark = ","), "<br>", "<strong>Likely to Afford: </strong>", sg_subzone_proj$median_price <= Budget) %>% lapply(htmltools::HTML), group="Subzone", options = pathOptions(pane = "Subzone")) %>%  addLayersControl(baseGroups = c("Planning"), overlayGroups = "Subzone")
  hdb_property_type_location_transaction <- resale_flats %>% group_by(blockstreet, town, flat_type, latitude, longitude) %>% summarise(median_price = median(resale_price)) %>% as.data.frame()
  hdb_affordable_projects <- hdb_property_type_location_transaction[hdb_property_type_location_transaction$median_price <= Budget,]
  hdbpropertyType.list <- unique(hdb_affordable_projects$flat_type)
  
  for(i in 1:length(hdbpropertyType.list))
  {
    hdb_propertytype.type <- hdb_affordable_projects[hdb_affordable_projects$flat_type == hdbpropertyType.list[i],]
    
    historical_hdb_leaflet <- addMarkers(historical_hdb_leaflet,
                                         lng=hdb_propertytype.type$longitude,
                                         lat=hdb_propertytype.type$latitude, clusterOptions = markerClusterOptions(),
                                         popup=paste0("<strong>Project Name: </strong>", hdb_propertytype.type$blockstreet, "<br>", "<strong>Historical Median Price: </strong>", paste0('$', formatC(hdb_propertytype.type$median_price, format = 'd', big.mark = ",")), "<br>", "<strong>Likely to Afford: </strong>", hdb_propertytype.type$median_price <= Budget),
                                         group = hdbpropertyType.list[i]
    )
  }
  historical_hdb_leaflet <- addLayersControl(historical_hdb_leaflet, overlayGroups = c("Subzone", hdbpropertyType.list))
  return(historical_hdb_leaflet)
}

# hdb_plotting(1000000) #test

# fx 9: fin priv_plotting
priv_plotting <- function(Budget){
  projects_median <- all_transactions %>% group_by(project) %>% slice_max(1) %>% group_by(project, latitude, longitude) %>% summarise(median_price = median(resale_price))
  all_transactions <- all_transactions %>% left_join(new_districts_tbl, by="district")
  all_transactions <- all_transactions %>% separate_rows(location, sep= ", ") 
  all_transactions$location <- sapply(all_transactions$location, str_trim)
  location_transaction <- all_transactions %>% group_by(location) %>% summarise(median_price = median(resale_price)) %>% as.data.frame()
  colnames(location_transaction)[1] <- "PLN_AREA_N"
  
  # sg_planning <- sg_planning1
  sg_planning_proj <- spTransform(sg_planning, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  sg_planning_proj <- merge(sg_planning_proj, location_transaction, by = 'PLN_AREA_N', all.x=TRUE)
  
  # sg_subzone <- sg_subzone1
  sg_subzone_proj <- spTransform(sg_subzone, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  sg_subzone_proj <- merge(sg_subzone_proj, location_transaction, by.x = 'SUBZONE_N', by.y = "PLN_AREA_N", all.x=TRUE)
  pal <- colorNumeric("viridis", NULL, reverse=T)
  affordable_projects <- projects_median[projects_median$median_price <= Budget,]
  historical_priv_leaflet <- leaflet() %>% 
    addTiles() %>% setView(lat = 1.36754925, lng = 103.8367406, 	
                           zoom = 11) %>%
    addMapPane("Planning", zIndex = 420) %>%
    addMapPane("Subzone", zIndex = 410) %>%
    addPolygons(data=sg_planning_proj, weight = 2, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8, fillColor = ~pal(log10(median_price)), label= paste0("<strong>Name: </strong>", sg_planning_proj$PLN_AREA_N, "<br>", "<strong>Median Price: </strong>", paste0('$', formatC(sg_planning_proj$median_price, format = 'd', big.mark = ",")), "<br>", "<strong>Likely to Afford: </strong>", sg_planning_proj$median_price <= Budget) %>% lapply(htmltools::HTML), group = "Planning", options = pathOptions(pane = "Planning")) %>%
    addLegend(pal = pal, title = "Median Transaction Price", values = log10(sg_planning_proj$median_price), opacity = 1.0,
              labFormat = labelFormat(transform = function(x) round(10^x))) %>%
    addPolygons(data=sg_subzone_proj, weight = 2, fillColor = "yellow", label=paste0("<strong>Name: </strong>", sg_subzone_proj$SUBZONE_N, "<br>", "<strong>Median Price: </strong>", paste0('$', formatC(sg_subzone_proj$median_price, format = 'd', big.mark = ",")), "<br>", "<strong>Likely to Afford: </strong>", sg_subzone_proj$median_price <= Budget) %>% lapply(htmltools::HTML), group="Subzone", options = pathOptions(pane = "Subzone")) %>%  addLayersControl(baseGroups = c("Planning"), overlayGroups = "Subzone")
  property_type_location_transaction <- all_transactions %>% group_by(project, latitude, longitude, propertyType) %>% summarise(median_price = median(resale_price)) %>% as.data.frame()
  
  affordable_projects <- property_type_location_transaction[property_type_location_transaction$median_price <= Budget,]
  
  propertyType.list <- unique(affordable_projects$propertyType)
  #m <- leaflet() %>% addTiles()
  for(i in 1:length(propertyType.list)) 
  {
    propertytype.type <- affordable_projects[affordable_projects$propertyType == propertyType.list[i],]
    
    historical_priv_leaflet <- addMarkers(historical_priv_leaflet, 
                                          lng=propertytype.type$longitude,
                                          lat=propertytype.type$latitude, clusterOptions = markerClusterOptions(),
                                          popup=paste0("<strong>Project Name: </strong>", propertytype.type$project, "<br>", "<strong>Historical Median Price: </strong>", paste0('$', formatC(propertytype.type$median_price, format = 'd', big.mark = ",")), "<br>", "<strong>Likely to Afford: </strong>", propertytype.type$median_price <= Budget),
                                          group = propertyType.list[i]
    )
  }
  historical_priv_leaflet <- addLayersControl(historical_priv_leaflet, overlayGroups = propertyType.list)
  return(historical_priv_leaflet)
}

# priv_plotting(1000000)  #test

# fx 10: over_the_yrs
over_the_yrs <- function() {
  all_property <- all_transactions %>% dplyr::select(year, month, district, street, longitude, latitude, area, resale_price, propertyType)
  HDB_property <- resale_flats %>% dplyr::select(month, district, street_name, longitude, latitude, floor_area_sqm, resale_price) %>% separate(month, sep = "-", into=c("year", "month")) %>%  cbind("HDB")
  colnames(HDB_property) <- c("year", "month", "district", "street", "longitude", "latitude", "area", "resale_price", "propertyType")
  
  hdb_condo <- rbind(all_property, HDB_property)
  hdb_condo$month <- str_pad(hdb_condo$month, 2, side="left", pad="0")
  over_the_years <- hdb_condo %>% mutate(monthyear = paste(month, year, sep = "-")) 
  over_the_years$date <- as.Date(paste0("01- ", over_the_years$monthyear), format="%d-%m-%Y")
  return(over_the_years)
}

# fx 11: over_the_yrs and the rest
line_graph_district_property <- function(districts_list, property_list){
  over_the_years <- over_the_yrs()
  toviz <- over_the_years %>% base::subset(district %in% districts_list & propertyType %in% property_list)
  if (dim(toviz)[1] == 0) {
    txt <- ggplot() + theme_void() + ggtitle("Oops! We do not have data here. Please change your choice!")
    return(ggplotly(txt))
  } else {
    toviz <- toviz %>% mutate(district_prop = paste(propertyType, district, sep = " - ")) %>% group_by(date, propertyType, district_prop) %>% summarise(median_price = median(resale_price)) %>% ggplot(aes(x = date, y = median_price, color=district_prop)) + geom_line() + ylab("Median Price") + xlab("Year") + ggtitle("Overall Median Price over the Years") + guides(color=guide_legend(title="Property Type -\nDistrict No.")) + scale_y_continuous(breaks = c(500000, 1000000, 2000000, 3000000, 5000000, 10000000), labels=c("500K", "1M", "2M", "3M", "5M", "10M")) + geom_point()
    return(ggplotly(toviz))}
}

table_district_property <- function(districts_list, property_list){
  over_the_years <- over_the_yrs()
  fordt <- over_the_years %>% base::subset(district %in% districts_list & propertyType %in% property_list)
  if (dim(fordt)[1] == 0) {
    txt <- over_the_years[FALSE,]
    return(txt)
  } else {
    return(fordt)}
}

town_price <- function(Name){
  trending <- bto_prices %>% filter(Town.Name == Name) %>% group_by(year) %>% summarise(avg_price = round(mean(avgprice), 2)) %>% ggplot(aes(x=year, y=avg_price, group=1)) + geom_line() + geom_point() + ggtitle(paste("Average Price of BTO Launches in", Name, "(2001 - 2021)")) + ylab("Price of Launches ($)") + xlab("Year of Launch")
  ggplotly(trending)
}
town_price("Sembawang")

town_psf <- function(Name){
  trending <- bto_prices %>% filter(Town.Name == Name) %>% group_by(year) %>% summarise(avg_psf = round(mean(avgpsf),2)) %>% ggplot(aes(x=year, y=avg_psf, group=1)) + geom_line() + geom_point() + ggtitle(paste("Average PSF of BTO Launches in", Name, "(2001 - 2021)")) + ylab("Price of Launches ($)") + xlab("Year of Launch")
  print(trending)
  ggplotly(trending)
}

# town_psf("Sembawang")

launch_price_bto <- function(Name){
  housingplot <- bto_prices %>% filter(Town.Name == Name)%>% group_by(year, Flat.Type) %>% summarise(avg_price = mean(avgprice)) %>% mutate(type = substr(Flat.Type, 1L, 1L)) %>% filter(!is.na(type)) %>% group_by(year, type) %>% filter(type != "") %>% summarise(avg_price = round(mean(avg_price),2)) %>% ggplot(aes(x=year, y=avg_price, color=type, group=type)) + geom_point() + geom_line() + ggtitle(paste0("Trend of BTO Listing Prices by Property Type Over Time in: \n", Name)) + ylab("Average Price") + scale_y_continuous(breaks = c(100000, 200000, 300000, 400000, 500000, 1000000, 2000000, 3000000, 5000000, 10000000), labels=c("100K", "200K", "300K", "400K", "500K", "1M", "2M", "3M", "5M", "10M"))
  ggplotly(housingplot)
}

# launch_price_bto("Sembawang")


asking_leaflet <- function() {
  asking_leaflet <- leaflet() %>% addTiles() %>% 
    addAwesomeMarkers(data=BTOproperties, lng = ~longitude, lat = ~latitude,icon = awesomeIcons(icon = 'glyphicon-home', iconColor = "white", markerColor = "pink"), clusterOptions = markerClusterOptions(), popup = paste0("<strong>Property: </strong>", BTOproperties$Address, "<br>", "<strong> Property Type: </strong>", BTOproperties$Property.Type, "<br>", "<strong>Asking Price: </strong>", BTOproperties$Asking), )
  asking_leaflet
}

# asking_leaflet()

x_year_comp <- function(year1, year2){
  data_for_dumbbell_plot <- bto_prices %>% 
    filter(year == year1 | year == year2) %>% 
    group_by(Town.Name, BTO.Name, year) %>% separate_rows(BTO.Name, sep = ",")
  
  data_for_dumbbell_plot$BTO.Name <- lapply(data_for_dumbbell_plot$BTO.Name, str_trim)
  
  data_for_dumbbell_plot <- data_for_dumbbell_plot %>% group_by(Town.Name, BTO.Name, year) %>% summarise(count=n()) %>% group_by(Town.Name, year) %>% summarise(count=n()) %>% pivot_wider(names_from = year, values_from = count) 
  
  data_for_dumbbell_plot <- data_for_dumbbell_plot %>% rename(launches_in_x = year1, launches_in_y = year2)
  
  data_for_dumbbell_plot <- data_for_dumbbell_plot %>% mutate_at(vars(launches_in_x, launches_in_y), ~replace_na(., 0))
  
  bto_year_viz <- data_for_dumbbell_plot %>%  
    ggplot( aes(x=as.factor(launches_in_x), xend=as.factor(launches_in_y), y=Town.Name, group=Town.Name) ) +
    geom_dumbbell(colour="#FFA511", 
                  size=2, 
                  colour_xend="#003D7C") +
    labs(x="Number of Launches", 
         y="Town Names", 
         title="Number of Launches in Singapore",
         subtitle = paste0("Change: ", year1, " vs ", year2),
         caption="Source: Teoalida") #Expand/Buy resale!
  return(bto_year_viz)
}


x_year_comp("2015", "2021")

transactions <- over_the_yrs()

formatX <- function(x)
{
  format(x, big.mark=",",scientific = F) #turn off scientific, add comma every 3 zero
}

formatX1 <- function(x) {
  dplyr::case_when( #or use ifelse - if value of x is 100, based on condtions and change
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0(as.character(x/1e3), "K"),
    x < 1e9 ~ paste0(as.character(x/1e6), "M"), # e is how many 0
    x < 1e12 ~ paste0(as.character(x/1e9),"B"),
    x < 1e15 ~ paste0(as.character(x/1e12),"T"),
    TRUE ~ "To be implemented..."
  )
}
