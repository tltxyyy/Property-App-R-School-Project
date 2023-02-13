library(shiny)
library(tidyverse)
library(shinyWidgets)
library(leaflet)
library(shinycssloaders)
library(DT)
library(ggmap)
library(emoji)
library(shinyjs)
library(shinythemes)

source("master_source_codes_utf8.R")

myFavNames <- c()
myFavValues <- c()


# Define UI for app
ui <- fluidPage(
  
  setBackgroundColor("#FFF6F6"),
  
  tagList(
    tags$head(
      tags$style(type = 'text/css', 
                 "@import url('https://fonts.googleapis.com/css2?family=Playfair+Display&display=swap');",
                 "@import url('https://fonts.googleapis.com/css2?family=Comforter+Brush&display=swap');",
                 
                 'body {
                          font-family: "Playfair Display", serif;
                          
                          }',
                 
                 'a {color: #b37979;}',   # changing the bg-color of a will result in blocked colour
                 
                 'a:hover {color: #6e2222;}',
                 
                 '.navbar {
                           background-color: #d9b4b4;
                           font-family: "Playfair Display", serif;
                           font-size: 15px;}',
                 
                 '.dropdown-menu { background-color: #fff0f0;
                           font-size: 13px; }',
                 
                 '.navbar-default {
                 border-color: 
                 }',
                 
                 '.navbar-default .navbar-brand {
                             color: #FFFFFF;
                             font-size: 30px;
                             font-family: "Comforter Brush", serif;
                             padding-top: 20px;
                             padding-right: 30px;
                             padding-left: 25px;
                             }',
                  
                 '.navbar-default .navbar-nav {
                             color: #FFFFFF;}',
                 
                 # sidebarpanel
                 '.well {
                          background-color: #e0d3d3;
                            }',
                 
      ),
      
      tags$script(" $(document).ready(function () {
      $('#navigation a[data-toggle=\"tab\"]').on('click', function (e) {
       window.scrollTo(0, 0)
            });

            });"), # does not seem to work: to make page scroll to the top again when switch to guide
    ),
    navbarPage("hSh", id = "navigation",
               
               
               tabPanel(
                 "Home",
                 tags$div(
                   style="margin-bottom: 50px",
                   img(src="hshp.png", height = 600, width = 1200),
                 ),
                 sidebarLayout(
                   
                   sidebarPanel(
                     h3("Begin your Search", emoji("sparkles")),
                     tags$br(),
                     textInput("house.address", "Search", placeholder = "Let's start searching!"), #, btnReset = icon("times")
                     
                     radioButtons(inputId = "pricing.heatmap", label = "Select Type of Map",
                                  choices = list("Default" = 1, "Pricing Heat Map" = 2), 
                                  selected = 1
                     ),
                     
                     actionButton("adv_filter_button", "Show/Hide Advance Filters",
                                  style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                     
                     hidden(
                       tags$br(),
                       pickerInput(inputId = "property.type", 
                                   label = "Select Property Type", 
                                   choices = sort(unique(properties$Property.Type)),
                                   options = list(
                                     `actions-box` = TRUE,
                                     size = 10,
                                     `selected-text-format` = "count > 3"
                                   )
                                   , multiple = TRUE
                       ),
                       
                       pickerInput(
                         inputId = "property.model", 
                         label = "Select Property Model", 
                         choices = sort(unique(properties$Model)), 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE
                       ),
                       
                       pickerInput(
                         inputId = "property.bedroom", 
                         label = "Select Number of Bedrooms", 
                         choices = sort(unique(properties$Bedrooms)), 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE
                       ),
                       
                       pickerInput(
                         inputId = "property.bathrooms", 
                         label = "Select Number of Bathrooms", 
                         choices = sort(unique(properties$Bathrooms)), 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE
                       ),
                       
                       pickerInput(
                         inputId = "property.furnish", 
                         label = "Select State of Furnishing", 
                         choices = sort(unique(properties$Furnish)), 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE
                       ),
                       
                       pickerInput(
                         inputId = "property.floor", 
                         label = "Select Property Floor", 
                         choices = sort(unique(properties$Floor.Level)), 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE
                       ),
                       
                       pickerInput(
                         inputId = "property.tenure", 
                         label = "Select Property Tenure Type", 
                         choices = sort(unique(properties$Tenure)), 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE
                       ),      
                       
                       pickerInput(
                         inputId = "property.developer", 
                         label = "Select Property Developer", 
                         choices = sort(unique(properties$Developer)), 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE
                       ),
                       
                       setSliderColor(c('#CB9C9C', '#CB9C9C'), c(1, 2)),
                       sliderInput(inputId = "property.no.of.units",
                                   label = "Select Number of Units",
                                   min = 0, max = max(properties$No..of.Units, na.rm = TRUE),
                                   value = c(0,max(properties$No..of.Units, na.rm = TRUE))
                       ),
                       
                       numericRangeInput(
                         inputId = "property.asking", label = "Select Property Asking Price ($)",
                         value = c(0,max(properties$Asking, na.rm = TRUE))
                       ),
                       
                       numericRangeInput(
                         inputId = "property.size", label = "Select Property Size (Sqft)",
                         value = c(0,max(properties$Size.builtup, na.rm = TRUE))
                       ),
                       
                       numericRangeInput(
                         inputId = "property.psf", label = "Select Property PSF",
                         value = c(0,max(properties$PSF.builtup, na.rm = TRUE))
                       ),
                       
                       pickerInput(
                         inputId = "property.builtyear", 
                         label = "Select Property Built Year", 
                         choices = sort(unique(properties$Built.Year)), 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE
                       ),
                       
                       pickerInput(
                         inputId = "property.top", 
                         label = "Select Property TOP", 
                         choices = sort(unique(properties$TOP)), 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE
                       ),
                       
                       radioButtons(inputId = "property.newlaunch",label="New Launch",
                                    choices = list("All Properties" = 1, "New Launch" = 2, "Not New Launch" = 3), 
                                    selected = 1
                       )
                     ), 
                     
                     
                     
                     
                     actionButton("action_confirm_filter", label = "Search",
                                  style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                     tags$hr(),
                     helpText("Click below to learn more about how to use the page!"),
                     actionButton("helpHome", "Guide Me",
                                  style="color: #b37979; background-color: #FFF6F6, border-color: #b37979")
                     
                   ),
                   
                   #now define the output
                   mainPanel(h4("Filter Results"),
                             leafletOutput("plotMapFilterResults"),
                             verbatimTextOutput("leafletMessage"),
                             useShinyjs(),
                             actionButton("filterResultsToggle", "Hide/Show Map",
                                          style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                             tags$br(),
                             tags$br(),
                             pickerInput(inputId = "search_output", label = h4("Select Property to View Details"),
                                         choices = list("Please Filter First" = "")),
                             actionButton("action_confirm_search", label = "Confirm",
                                          style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                             tags$br(),
                             tags$br(),
                             h4("Search Results"),
                             actionButton("action_confirm_fav", label = "Add to Favourite",
                                          icon("heart"), 
                                          style="color: #fff ; background-color: #CB9C9C"),
                             actionButton("action_confirm_fav_remove", label = "Remove from Favourite",
                                          icon("trash"), 
                                          style="color: #fff ; background-color: #B2B1B1"),
                             tags$br(),
                             tags$br(),
                             withSpinner(leafletOutput("onePropMap"), color = "pink"),
                             tags$br(),
                             tags$br(),
                             withSpinner(DT::dataTableOutput("onePropTable"), color = "pink"),
                             h4("You have favourited: "),
                             verbatimTextOutput("favText2"),
                             
                   )
                   
                 )
               ),
               navbarMenu("Analyse Past Transactions",
                          tabPanel("View on the Map", fluid = TRUE, icon = icon("globe-americas"),
                                   titlePanel("Historical Property Prices"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons(inputId = "propertytype",label="Property Type",
                                                    choices = c("HDB","Private Property")),
                                       numericInput(inputId="budget", label = "Input Your Budget ($):", value = 800000), actionButton("action_confirm_pptytype", label = "Confirm",
                                                                                                                                          style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                       
                                       tags$hr(),
                                       helpText("Click below to learn more about how to use the page!"),
                                       actionButton("hideshowmsg", "Guide Me",
                                                    style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                       ),
                                     
                                     mainPanel(h3("Explore Median Prices Across Districts!"), 
                                               p(paste0("Click on the Panel on the Left to Start Your Exploration ", emoji('round_pushpin'))),
                                               withSpinner(leafletOutput("propertyplot"), color = "pink"),
                                               tags$hr(),
                                               helpText("Data from URA, HDB, data.gov.sg")
                                     )
                                   )),
                          tabPanel("Shortlist and Analyse Properties Sold within Your Budget", fluid = TRUE, icon = icon("store"), 
                                   
                                   titlePanel("Shortlist and Analyse the Properties Sold within Your Budget"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       numericInput(inputId= "budgetID", label = "Input Budget ($)", value = 1000000),
                                       
                                       pickerInput(inputId="districtID3", label = "Select District:", 
                                                   choices = str_pad(1:28, 2, side="left", "0"), 
                                                   options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), multiple=T,
                                                   selected = str_pad(1:28, 2, side="left", "0")), 
                                       checkboxGroupInput(inputId = "propertyTypeID2",
                                                          label = "Select Property Type:",
                                                          choices = c("Apartment","Condominium","Detached", "HDB", "Semi-detached","Terrace","Strata Terrace","Strata Detached","Strata Semi-detached","Executive Condominium"),
                                                          selected = c("Apartment","Condominium","Detached", "HDB", "Semi-detached","Terrace","Strata Terrace","Strata Detached","Strata Semi-detached","Executive Condominium")),
                                       checkboxGroupInput(inputId = "yearID2",
                                                          label = "Select Year(s):",
                                                          choices = c(2016:2021),
                                                          selected = c(2019:2021)),
                                       numericInput(inputId= "topxID", label = "Select no. of properties to show:", value = 25),
                                       actionButton("action_confirm_shortlistview", label = "Confirm",
                                                    style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                       tags$hr(),
                                       helpText("*Shortlist shows data for the highest prices that are within the budget"),
                                       helpText("Data from URA, HDB, data.gov.sg")),
                                     
                                     mainPanel( useShinyjs(),
                                                actionButton("shortlistprop", "Hide/show message",
                                                             style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                                tags$br(),
                                                htmlOutput("shortlistmsg"), 
                                                tags$br(),
                                                plotlyOutput("testplot3"),
                                                tags$br(),
                                                DT::dataTableOutput("ShortlistTable"), p())
                                   )), 
                          tabPanel("Compare Price Trends (by Property Type)", fluid = TRUE, icon = icon("search-dollar"),
                                   titlePanel("Price data by Property Type and Districts"),
                                   sidebarLayout(
                                     sidebarPanel(pickerInput(inputId="district", label = "Select Your Districts to View:", choices = c(str_pad(1:28, 2, side="left", "0")), options = list(
                                       `actions-box` = TRUE, 
                                       size = 10,
                                       `selected-text-format` = "count > 3"
                                     ), multiple=T), 
                                     checkboxGroupInput(inputId = "propertytype2",label="Property Type", choices = c("Apartment","Condominium","Detached", "HDB", "Semi-detached","Terrace","Strata Terrace","Strata Detached","Strata Semi-detached","Executive Condominium")
                                     ),
                                     actionButton("action_confirm_graphview", label = "Confirm",
                                                  style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                     tags$hr(),
                                     helpText("Data from URA, HDB, data.gov.sg")),
                                     
                                     mainPanel(h3("Analyse the past median price data over the years"), 
                                               # h5("Click on 'Confirm' to start!"),
                                               tabsetPanel( 
                                                 tabPanel("Districts", DT::dataTableOutput("DistrictsTable")), 
                                                 tabPanel("View Analysis", withSpinner(plotlyOutput("lgdistrictpropertyplot"), color = "pink"), DT::dataTableOutput("PropPricesTable"))
                                               )
                                     )
                                   )
                          ),
                          tabPanel("District, Price and Property Type Comparisons", fluid = TRUE,icon = icon("house-user"),
                                   
                                   titlePanel("District, Price and Property Type Comparisons"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       # Select which Property Type(s) to plot
                                       checkboxGroupInput(inputId = "propertyTypeID",
                                                          label = "Select Property Type:",
                                                          choices = c("Apartment","Condominium","Detached", "HDB", "Semi-detached","Terrace","Strata Terrace","Strata Detached","Strata Semi-detached","Executive Condominium"),
                                                          selected = c("Apartment","Condominium")),
                                       # Select which Year(s) to plot
                                       pickerInput(inputId="districtID1", label = "Select District:", 
                                                   choices = str_pad(1:28, 2, side="left", "0"), 
                                                   options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), multiple=T,
                                                   selected = c("01")
                                       ),
                                       pickerInput(inputId = "yearID",
                                                   label = "Select Year(s) (Choose Max 3):",
                                                   choices = c(2016:2021),
                                                   selected = c(2019,2020, 2021), multiple=TRUE, options= list("max-options" = 3)),
                                       
                                       tags$hr(),
                                       helpText("Data from URA, HDB, data.gov.sg"),
                                     ),
                                     
                                     mainPanel(tabsetPanel(
                                       tabPanel("Price Comparison (Year, District, Property Type)", plotOutput("testplot")),
                                       tabPanel("PSF Comparison (Year, District, Property Type)", plotOutput("testplot2")),
                                       
                                     ),
                                     tags$br(),
                                     useShinyjs(),
                                     actionButton("hideshowtbl", "Hide/show district table",
                                                  style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                     DT::dataTableOutput("HidingDistrictTable"), p()
                                     
                                     )
                                   ))),
               tabPanel("BTO Launches", fluid = TRUE, icon = icon("hotjar"),
                        titlePanel("BTO Launches"),
                        sidebarLayout(
                          sidebarPanel(
                            conditionalPanel(condition = 'input.bto_tabs === "BTO Launches this November 2021"',
                                             h5("These are all the BTO available currently!")
                            ),
                            conditionalPanel(condition = 'input.bto_tabs === "Price Trends"',
                                             pickerInput(inputId = "towntype1",label="Choose Your HDB Town",
                                                         choices = c(sort(unique(bto_prices$Town.Name))), selected= NULL,
                                                         options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), multiple=F),
                                             radioButtons(inputId="psfprice", label = "Choose Price or PSF", choices = c("Price", "PSF")), 
                                             actionButton("action_confirm_bto1", label = "Confirm",
                                                          style="color: #b37979; background-color: #FFF6F6, border-color: #b37979")
                            ),
                            conditionalPanel(condition = 'input.bto_tabs === "Prices and Launches Trend (Town)"',
                                             pickerInput(inputId = "towntype2",label="Choose Your HDB Town",
                                                         choices = c(sort(unique(bto_prices$Town.Name))), selected= NULL,
                                                         options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), multiple=F),
                                             actionButton("action_confirm_bto2", label = "Confirm",
                                                          style="color: #b37979; background-color: #FFF6F6, border-color: #b37979")
                            ),
                            conditionalPanel(condition = 'input.bto_tabs === "N Year Comparison"', # N year comparison
                                             
                                             sliderInput(inputId = "btoxyearslider", 
                                                         label = "Years to Compare Number of Launches (by BTO Name)",
                                                         min = 2016,max = 2021, value = c(2016, 2021), sep = ""),
                                             tags$hr(),
                                             helpText("Click below to learn more about how to use the page!"),
                                             actionButton("btoxyear", "Guide Me",
                                                          style="color: #b37979; background-color: #FFF6F6, border-color: #b37979")
                            ),
                            
                            tags$hr(),
                            helpText("Source: SRX, Teoalida")),
                          
                          mainPanel(
                            h3("Understand the BTO Scene in Singapore!"), 
                            tabsetPanel(id = "bto_tabs",
                                        tabPanel("BTO Launches this November 2021", withSpinner(leafletOutput("BTOleaflet"), color = "pink"), hr(), p("Interact with our popups for more information about the location, property type and asking price!")), 
                                        tabPanel("Price Trends", withSpinner(plotlyOutput("price_or_psf_plot"), color = "pink"), hr(), p("Choose your town and view the average price (or PSF) of BTO launches!")), 
                                        tabPanel("Prices and Launches Trend (Town)", withSpinner(plotlyOutput("price_launch_town"), color = "pink"), hr(), p(paste0("Choose your town and view the average launch price of BTOs by flat type. ", emoji('eyes'), " You may see 2-room ('2'), 3-room ('3'), 4-room ('4') and 5-room ('5') flats, as well as Studio Apartments ('S')!", " Psst... You might want to hurry..."))), 
                                        tabPanel("N Year Comparison",
                                                 withSpinner(plotOutput("price_launch_x"), color = "pink")
                                        )
                            )
                          )
                        )
               ),
               navbarMenu("Compare Favourites!", icon = icon("heart"),
                          tabPanel("Amenities Within Specified Radius of Property",
                                   tags$br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       h2("Favourites"),
                                       checkboxGroupInput("checkbox_fav1", label = h5("Find all your favourited properties here!", emoji("house_with_garden"), emoji("heart")), choiceNames = c(), choiceValues = c()),
                                       tags$br(),
                                       numericInput(inputId= "num_radius", label = "Find amenities within radius (metres): ", value = 400),
                                       actionButton("action_confirm_rad", label = "Confirm",
                                                    style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                       tags$hr(),
                                       helpText("Click below to learn more about how to use the page!"),
                                       actionButton("helpAmenRad", "Guide Me",
                                                    style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                       
                                     ),
                                     mainPanel(
                                       h3("Search for amenities within specified radius distance!", emoji("round_pushpin")),
                                       withSpinner(leafletOutput("plotMapRad"), color = "pink"),
                                       tags$br(),
                                       withSpinner(DT::dataTableOutput("PropDetailsTableRad"), color = "pink")
                                     )
                                   )
                                   
                          ),
                          tabPanel("Amenities Nearest to Property",
                                   tags$br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       h2("Favourites"),
                                       checkboxGroupInput("checkbox_fav2", label = h5("Find all your favourited properties here!", emoji("house_with_garden"), emoji("heart")), 
                                                          choiceNames = c(), choiceValues = c()),
                                       tags$br(),
                                       numericInput(inputId= "num_topn", label = "Select Closest Number of Amenities", value = 3),
                                       actionButton("action_confirm_topn", label = "Confirm",
                                                    style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                       tags$hr(),
                                       helpText("Click below to learn more about how to use the page!"),
                                       actionButton("helpAmenTopN", "Guide Me",
                                                    style="color: #b37979; background-color: #FFF6F6, border-color: #b37979")
                                     ),
                                     mainPanel(
                                       h3("Show specified number of amenities around each property!"),
                                       withSpinner(leafletOutput("plotMapTopN"), color = "pink"),
                                       tags$br(),
                                       withSpinner(DT::dataTableOutput("PropDetailsTableTopN"), color = "pink")
                                     )
                                   )
                          ),
                          tabPanel("Grants",
                                   tags$br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       conditionalPanel(
                                         condition = 'input.grant_tabs === "Grants Table"',
                                         h2("Favourites"),
                                         checkboxGroupInput("checkbox_fav_grant", label = h5("Find all your favourited properties here!", emoji("house_with_garden"), emoji("heart")), 
                                                            choiceNames = c(), choiceValues = c()),
                                         tags$br(),
                                         numericInput(inputId= "salary_grant", label = "Annual Monthly Household Income:", value = 0),
                                         pickerInput(inputId= "buyer_type_grant", label="Type of Applicant:", choices=unique(Housing_grants$Type_of_buyer)),
                                         pickerInput(inputId= "citizenship_grant", label="Citizenship:", choices=unique(Housing_grants$Citizenship)),
                                         pickerInput(inputId= "fam_sin_grant", label="Applying as a Family or Single:", choices=unique(Housing_grants$Family_or_Single)),
                                         actionButton("action_confirm_grant", label = "Confirm",
                                                      style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                         tags$hr(),
                                         helpText("Click below to learn more about how to use the page!"),
                                         actionButton("helpgrants1", "Guide Me",
                                                      style="color: #b37979; background-color: #FFF6F6, border-color: #b37979")
                                         
                                         
                                       ),
                                       conditionalPanel(
                                         condition = 'input.grant_tabs === "Properties within 4km radius eligible for Proximity Grant"',
                                         textInput(inputId = "proxrad", label = "Parent's house:", placeholder = "Address or Postal Code"), #test: "49 Telok Blangah Drive (100049)" 
                                         actionButton("action_confirm_prox", label = "Confirm",
                                                      style="color: #b37979; background-color: #FFF6F6, border-color: #b37979"),
                                         tags$hr(),
                                         helpText("Click below to learn more about how to use the page!"),
                                         actionButton("helpgrants2", "Guide Me",
                                                      style="color: #b37979; background-color: #FFF6F6, border-color: #b37979")
                                         
                                       )
                                     ),
                                     mainPanel(
                                       tabsetPanel(
                                         id = "grant_tabs",
                                         tabPanel("Grants Table",
                                                  h3("All eligible grants at one glance!"),
                                                  withSpinner(DT::dataTableOutput("plotGrantsTable"), color = "pink")
                                         ),
                                         tabPanel("Properties within 4km radius eligible for Proximity Grant",
                                                  h3("Find all properties that is eligible for the Proximity Grant!"),
                                                  withSpinner(leafletOutput("plotProxRad"), color = "pink"),
                                                  
                                                  
                                         )
                                       )
                                     )
                                   )
                          )
               ),
               tabPanel("Guide",
                        tags$style(HTML(
                          "h3 {text-align:center }",
                          "h1 {text-align:center }",
                          "p {text-align:center }",
                          "hr {border: 0.7px solid #e6e3e3}"
                        )),
                        fluidRow(
                          column(2),
                          column(8,
                                 tags$br(),
                                 h1("~User Guide~"),
                                 tags$br(),
                                 p(emoji('sparkles'), "Hello there", emoji('sparkles')),
                                 p("Welcome to Home Sweet Home (hSh)!"),
                                 p("Are you finding for a new home? Fret not! Let us guide you through all the interesting and practical tools we have on our website to help you in your search for your dream home."),
                                 p("Happy exploring! ", emoji('blush')),
                                 tags$br(),
                                 tags$hr(),   # horizontal line
                                 tags$br(),
                                 
                                 h3("Home"),
                                 tags$br(),
                                 p("Feeling property hunting is too tiring? Wanna chop chop and find your ideal property now and not take decades to find it? This app is for you if:"),
                                 tags$ul(
                                   tags$li("You are a young couple or newly married couple looking for your dream home"),
                                   tags$li("You want to find a property that fits your budget and has your preferred specifications"),
                                   tags$li("You want to make your life easier and save on your hard-earned money by finding grants that you are eligible for"),
                                   tags$li("And the list goes on...we don't wanna bore you.")
                                 ),
                                 p("Without further ado, let's get you started!"),
                                 p("Firstly, if you already have an ideal type of house you have in mind then filter right away! 
                               Click on the 'Show/Hide Advance Filters' button and specify all the nitty gritty details so that the app will only show you what you want to see!.
                               If you don't already know what you want then just click on 'Search' without any inputs to see all properties available now in Singapore!
                               Concerned about affordability? Select the 'Pricing Heat Map' view to look at the median prices of properties by the different subzones in Singapore!
                               Get an even clearer idea of your options at our financials page to see which property types and districts in Singapore are meant for your budget.
                               Your search results will appear in the map on the right and it is interactive! It the shows basic features for each property when you click on them too.", emoji('sparkles')),
                                 p("See a property you are interested in the map? Find the listing you're interested in the 'Select Property to View Details' input and press confirm to view its complete details!"),
                                 p("Like the property you see? Favourite it! "),
                                 p("To favourite another item, make sure to click on 'confirm' first before adding to favourite!"),
                                 p("Come back to view your favourited properties whenever you want and here's a little secret: 
                               you can even compare favorited properties!"),
                                 tags$br(),
                                 
                                 tags$hr(),   # horizontal line
                                 
                                 tags$br(),
                                 h3("Analyse Past Transactions"),
                                 tags$br(),
                                 
                                 p("Wondering about past transactions in Singapore over the past few years? Curious to know what properties you could have afforded with your budget?"),
                                 p(emoji('sparkles'), "We got ya!  ", emoji('sparkles')),
                                 p("Our map will show you the median transacted prices of properties sold across our little red dot. Get a sensing of how much the HDBs and private properties in your areas of interest by district!"),
                                 p("How to use this tool:"),
                                 tags$ol(
                                   tags$li("Select 'HDB' or 'Private Property' from the left panel."),
                                   tags$li("Input your budget level."),
                                   tags$li("Select 'Confirm'.", "Let the magic happen! ", emoji('heart_eyes')),
                                   tags$li("Hover over the map to view the region's median price, or click on the popups to view the median transacted prices!"),
                                   tags$li("If you would like to view certain types of properties in the map, hover over the map and deselect property types which you do not want to see.")
                                 ),
                                 p("Happy exploring! ", emoji('blush')),
                                 tags$br(),
                                 
                                 tags$hr(),
                                 
                                 tags$br(),
                                 h3("District, Price and Property Type Comparisons"),
                                 tags$br(),
                                 
                                 p("This graph may be new to you but it's all good! ", emoji('thumbsup')),
                                 p("For instance, in Figure X, Bedok had four project launches in 2016, but had none in 2021. 
                                   On the other hand, Tampines had three project launches in 2016 and another three in 2021. 
                                   By comparing the number of launches over the years, you may plan your next step. Just some guiding questions for your planning:"
                                   ),
                                 tags$ol(
                                   tags$li(
                                     "Do you wanna go to somewhere more 'hype' with more BTO Launches in the last 1-2 years? Maybe there's a higher chance of a BTO in that area with a higher chance for you to get a unit? You could even grow old with peers of your similar age? ", emoji('sunglasses')
                                   ),
                                 tags$li(
                                   "Or, do you want to stay near your parents and see if the area possibly could have more new BTO launches? ", emoji('heart_eyes')
                                   )
                                 ),
                                 tags$br(),
   
                                 
                                 tags$hr(),   # horizontal line
                                 
                                 tags$br(),
                                 h3("Amenities Within Specified Radius of Property"),
                                 tags$br(),
                                 
                                 p("Already saved some properties to your favourites? Woohoo! Let's get started on comparing which is better for you! Tick the properties you want to compare on the left and key in a radius around the properties you want to see.
                               Toggle on the filters in the map to hide or show the different types of amenities near the properties!"),
                                 p("If you don't see any properties for you to click on, head over to home page to favourite some before coming to this page!", emoji("eyes")),
                                 p("Loading may take awhile so do be patient with us", emoji("pray")),
                                 p("After that's done, take your time to see the amenities around these properties! If you're not able to find certain amenities, it's not within the radius you entered! You can re-enter another radius or head over to the next page to view the nearest number of amenities you want to see."),
                                 tags$br(),
                                 
                                 tags$hr(),
                                 
                                 tags$br(),
                                 h3("Amenities Nearest to Property"),
                                 tags$br(),
                                 
                                 p("This sub-page is similar to the first sub page, the only difference is that we show the closest number of amenities to the properties regardless of its radius distance! Let's say you would like to see all the closest 3 amenities to each property, just simply type '3' inside the field 'Select Closest Amenities' and search right away! ", emoji("smiley"), "Remember to tick the properties you want to compare too!"),
                                 p("Loading may take a while so do be patient with us", emoji("disappointed")),
                                 p("Hope you are able to find your property of choice!"),
                                 tags$br(),
                                 
                                 tags$hr(),
                                 
                                 tags$br(),
                                 h3("Eligible Grants"),
                                 tags$br(),
                                 
                                 p("Hello there! By using this page, you've just made our day", emoji("smile")),
                                 p("This is because you are one of us - you like to save money! Well, again, we gotcha..."),
                                 p("The process to find your eligible grant is made easy-peasy. Just enter your details in the table, select the properties from your favourites list for which you want to compare grants and click confirm. You're all ready to go!!"),
                                 p("A caution though! Unfortunately, these government grants are only available for HDB resale and BTO properties. So if the property you selected shows no grants, it can mean one of two things:"),
                                 tags$ul(
                                   tags$li("You are not eligible for any of the grants for that property (we feel for ya", emoji("cry"), ")"),
                                   tags$li("The property itself is not eligible since it is not a HDB resale or BTO property. In this case, you might wanna consider selecting HDB or BTO properties if you want to apply for these grants. Else, go ahead and explore the loan options here: https://www.moneysmart.sg/home-loan.")
                                 ),
                                 p("All the best and go grab that grant!", emoji("smile")),
                                 p("To find out more about each of these grants, head over to this link: https://www.hdb.gov.sg/residential/buying-a-flat/new/schemes-and-grants"),
                                 p("P.S. check out the proximity grant tab too if you want to see which resale properties would enable you to be eligible for the proximity grant."),
                                 tags$br(),
                                 
                                 tags$hr(),
                                 
                                 tags$br(),
                                 h3("Properties within 4km radius eligible for Proximity Grant"),
                                 tags$br(),
                                 
                                 p("Are you wondering what this tab is about?", emoji("confused")),
                                 p("Well, it's an easy way for you to find out which of the currently listed (available) properties make you eligible for the proximity grant. This is determined based on the location of your parent's house, where we search up properties within 4km of your parent's house and those properties are the ones we recommend you to consider if you want to apply for a proximity grant! 
                                 Using this feature takes just a single step. Enter the postal code of your parent's home and click confirm."),
                                 p("Now, watch the magic unfold", emoji("blush")),
                                 p("Note that the map will appear as blank if Address or Postal code entered cannot be found. Try another input!"),
                                 tags$br(),
                                 
                                 p("Happy exploring!", emoji("relaxed")),
                                 tags$br(),
                                 tags$br(),
                                 tags$br()
                                 
                                 # to fix: if i define style like that, all the h3 and h4 will be centered, i want different h3 and h4 to behave differently
                                 
                          ),
                          column(2)
                        )
                        
                        
               )
               
               
    )
    
  )
  
)

#Define server logic
server <- function(input,output, session) {

  # eventReactive(wait for first parameter to happen, then output second parameter)
  rad <- eventReactive(input$action_confirm_rad, {input$num_radius})
  fav_selected1 <- eventReactive(input$action_confirm_rad, {input$checkbox_fav1})
  fav_selected2 <- eventReactive(input$action_confirm_topn, {input$checkbox_fav2})
  n <- eventReactive(input$action_confirm_topn, {input$num_topn})


  output$plotMapRad <- renderLeaflet(
    {
      getAmenityRadiusMapnTable(fav_selected1(), rad())[[1]]
    })


  output$PropDetailsTableRad <- DT::renderDT(
    {
      plot_table_amen_table <- getAmenityRadiusMapnTable(fav_selected1(), rad())[[2]]

      # data table customisation 1
      my.options <- list(ordering = FALSE,
                         info = FALSE,
                         scrollX = TRUE)

      header.style <- "th { font-family: 'Arial'; font-weight: bold; color: white; background-color: #CB9C9C;}"

      header.names <- c(colnames(plot_table_amen_table))

      # data table customisation 2
      my.container <- withTags(
        table(
          style(type = "text/css", header.style),
          thead(
            tr(
              lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
            )
          )
        )
      )

      DT::datatable(plot_table_amen_table, options = my.options, container = my.container, rownames = F) %>% formatStyle(columns = c("Address"), fontWeight = "bold")
    }
  )

  output$plotMapTopN <- renderLeaflet(
    {
      vis_n_nearest(fav_selected2(), n())[[1]]
    })

  output$PropDetailsTableTopN <- DT::renderDT(
    {
      topNTable <- vis_n_nearest(fav_selected2(), n())[[2]]

      # data table customisation 1
      my.options <- list(ordering = FALSE,
                         info = FALSE,
                         scrollX = TRUE)

      header.style <- "th { font-family: 'Arial'; font-weight: bold; color: white; background-color: #CB9C9C;}"

      header.names <- c(colnames(topNTable))

      # data table customisation 2
      my.container <- withTags(
        table(
          style(type = "text/css", header.style),
          thead(
            tr(
              lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
            )
          )
        )
      )
      DT::datatable(topNTable, options = my.options, container = my.container, rownames = F) %>% formatStyle(columns = c("Address"), fontWeight = "bold")
    }
  )

  # EventReactives
  fav_selected_grant <- eventReactive(input$action_confirm_grant, {input$checkbox_fav_grant})
  salary_grant_c <- eventReactive(input$action_confirm_grant, {input$salary_grant})
  buyer_type_grant_c <- eventReactive(input$action_confirm_grant, {input$buyer_type_grant})
  citizenship_grant_c <- eventReactive(input$action_confirm_grant, {input$citizenship_grant})
  fam_sin_grant_c <- eventReactive(input$action_confirm_grant, {input$fam_sin_grant})

  output$plotGrantsTable <- DT::renderDT(
    {
      plot_table_grants <- GrantsEligibilityTable(fav_selected_grant(), salary_grant_c(), buyer_type_grant_c(), citizenship_grant_c(), fam_sin_grant_c())

      # data table customisation 1
      my.options <- list(ordering = FALSE,
                         info = FALSE,
                         scrollX = TRUE)

      header.style <- "th { font-family: 'Arial'; font-weight: bold; color: white; background-color: #CB9C9C;}"

      header.names <- c(colnames(plot_table_grants))

      # data table customisation 2
      my.container <- withTags(
        table(
          style(type = "text/css", header.style),
          thead(
            tr(
              lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
            )
          )
        )
      )
      DT::datatable(plot_table_grants, options = my.options, container = my.container, rownames = F) %>% formatStyle(columns = c("Grant Name"), fontWeight = "bold", width="200px")
    }
  )

  # EventReactives
  parentsHome <- eventReactive(input$action_confirm_prox, {input$proxrad})

  output$plotProxRad <- renderLeaflet(
    {
      FindPropertiesEligibleForProxGrant(parentsHome())[[1]]
    }
  )
#
#   output$checkNaGrant <- renderText(
#     {
#       naResult <- FindPropertiesEligibleForProxGrant(parentsHome())
#       naResult[[2]]
#     }
#   )

    # tim
  
  observeEvent(input$adv_filter_button, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("property.type")
    toggle("property.model")
    toggle("property.bedroom")
    toggle("property.bathrooms")
    toggle("property.furnish")
    toggle("property.floor")
    toggle("property.tenure")
    toggle("property.developer")
    toggle("property.no.of.units")
    toggle("property.asking")
    toggle("property.size")
    toggle("property.psf")
    toggle("property.builtyear")
    toggle("property.top")
    toggle("property.newlaunch")
  })
  
  # toggle for help
  observeEvent(input$helpHome, {
    # every time the button is pressed, alternate between hiding and showing the plot
    updateTabsetPanel(session, "navigation", selected = "Guide")
  })


  observeEvent(input$helpgrants1, {
    # every time the button is pressed, alternate between hiding and showing the plot
    updateTabsetPanel(session, "navigation", selected = "Guide")
  })

  observeEvent(input$helpgrants2, {
    # every time the button is pressed, alternate between hiding and showing the plot
    updateTabsetPanel(session, "navigation", selected = "Guide")
  })


  #helpAmen
  observeEvent(input$helpAmenRad, {
    # every time the button is pressed, alternate between hiding and showing the plot
    updateTabsetPanel(session, "navigation", selected = "Guide")
  })

  observeEvent(input$helpAmenTopN, {
    # every time the button is pressed, alternate between hiding and showing the plot
    updateTabsetPanel(session, "navigation", selected = "Guide")
  })


  output$helpAmenTopN_Output <- renderUI(
    {
      HTML(
        paste("Yo! Good to see you here!","<br/>", "This sub-page is similar to the first sub page, but no worries we will still guide you through heh", emoji("smiley"), "<br/>", "Tick the properties you want to compare on the left and enter in the number of amenities you want to see around the properties.","<br/>",
              "Loading may take a while so do be patient with us", emoji("disappointed"), "<br/>", "Take your time to see the amenities around these properties!","<br/>",
              "Hope you are able to find your property of choice!")
      )
    }
  )



  observeEvent(input$filterResultsToggle, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("plotMapFilterResults")
    toggle("leafletMessage")
  })

  # eventReactive for home page
  house.address <- eventReactive(input$action_confirm_filter, {input$house.address})
  property.type <- eventReactive(input$action_confirm_filter, {input$property.type})
  property.model <- eventReactive(input$action_confirm_filter, {input$property.model})
  property.bedroom <- eventReactive(input$action_confirm_filter, {input$property.bedroom})
  property.bathrooms <- eventReactive(input$action_confirm_filter, {input$property.bathrooms})
  property.furnish <- eventReactive(input$action_confirm_filter, {input$property.furnish})
  property.floor <- eventReactive(input$action_confirm_filter, {input$property.floor})
  property.tenure <- eventReactive(input$action_confirm_filter, {input$property.tenure})
  property.developer <- eventReactive(input$action_confirm_filter, {input$property.developer})
  property.no.of.units <- eventReactive(input$action_confirm_filter, {input$property.no.of.units})
  property.asking <- eventReactive(input$action_confirm_filter, {input$property.asking})
  property.size <- eventReactive(input$action_confirm_filter, {input$property.size})
  property.psf <- eventReactive(input$action_confirm_filter, {input$property.psf})
  property.builtyear <- eventReactive(input$action_confirm_filter, {input$property.builtyear})
  property.top <- eventReactive(input$action_confirm_filter, {input$property.top})
  property.newlaunch <- eventReactive(input$action_confirm_filter, {input$property.newlaunch})
  pricing.heatmap <- eventReactive(input$action_confirm_filter, {input$pricing.heatmap})

  output$plotMapFilterResults <- renderLeaflet(
    {
      result <- Property_Visualisation(house.address(), property.type(), property.model(), property.bedroom(), property.bathrooms(), property.furnish(), property.floor(), property.tenure(), property.developer(), property.no.of.units(), property.asking(), property.size(), property.psf(), property.builtyear(), property.top(), property.newlaunch(), pricing.heatmap()) # rmb to put () !!!
      result[[1]]

    })


  output$leafletMessage <- renderText(
    {
      result <- Property_Visualisation(house.address(), property.type(), property.model(), property.bedroom(), property.bathrooms(), property.furnish(), property.floor(), property.tenure(), property.developer(), property.no.of.units(), property.asking(), property.size(), property.psf(), property.builtyear(), property.top(), property.newlaunch(), pricing.heatmap()) # rmb to put () !!!
      result[[2]]
    }
  )

  # Filter on the filter
  # if i press filter confirm, then i update the choices in search_output
  observeEvent(input$action_confirm_filter, {
    updatePickerInput(session, "search_output", choices = names(Property_Table(input$house.address, input$property.type, input$property.model, input$property.bedroom, input$property.bathrooms, input$property.furnish, input$property.floor, input$property.tenure, input$property.developer, input$property.no.of.units, input$property.asking, input$property.size, input$property.psf, input$property.builtyear, input$property.top, input$property.newlaunch)))
  })

  # EventReactive
  property.to.display <-  eventReactive(input$action_confirm_search, {input$search_output})

  output$onePropMap <- renderLeaflet(
    {
      showOnePropertyonMapnTable(property.to.display())[[1]]
    }
  )

  output$onePropTable <- DT::renderDataTable(
    {
      displayTable <- showOnePropertyonMapnTable(property.to.display())[[2]]

      # data table customisation 1
      my.options <- list(ordering = FALSE,
                         info = FALSE,
                         scrollX = TRUE,
                         autoWidth = FALSE)

      header.style <- "th { font-family: 'Arial'; font-weight: bold; color: white; background-color: #CB9C9C;}"

      header.names <- c(colnames(displayTable))

      # data table customisation 2
      my.container <- withTags(
        table(
          style(type = "text/css", header.style),
          thead(
            tr(
              lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
            )
          )
        )
      )

      DT::datatable(displayTable, options = my.options, rownames = F, container = my.container)
    }
  )

    ## fav section

  # EventReactives
  property.to.display <-  eventReactive(input$action_confirm_search, {input$search_output}) # mutated name

  fav_clicked_index <- eventReactive(input$action_confirm_fav, {gsub(".*?\\/\\s", "", property.to.display())})
  fav_clicked_index_r <- eventReactive(input$action_confirm_fav_remove, {gsub(".*?\\/\\s", "", property.to.display())})

  ## initialize current states as reactive values
  currentStates <- reactiveValues(
    selected = NULL,
    myFavValues = myFavValues,
    myFavNames = myFavNames
  )


  observeEvent(input$action_confirm_fav, {
    ## add to favourites
    currentStates$myFavValues <- union(currentStates$myFavValues, fav_clicked_index())  # the updated options: index form
    currentStates$myFavNames <- union(currentStates$myFavNames, property.to.display()) # mutated name form

    updateCheckboxGroupInput(session, "checkbox_fav1", choiceNames = currentStates$myFavNames, choiceValues = currentStates$myFavValues)
    updateCheckboxGroupInput(session, "checkbox_fav2", choiceNames = currentStates$myFavNames, choiceValues = currentStates$myFavValues)
    updateCheckboxGroupInput(session, "checkbox_fav_grant", choiceNames = currentStates$myFavNames, choiceValues = currentStates$myFavValues)
  })

  observeEvent(input$action_confirm_fav_remove, {
  ## remove from favourites
    currentStates$myFavValues <- setdiff(currentStates$myFavValues, fav_clicked_index_r())
    currentStates$myFavNames <- setdiff(currentStates$myFavNames, property.to.display())
    updateCheckboxGroupInput(session, "checkbox_fav1", choiceNames = currentStates$myFavNames, choiceValues = currentStates$myFavValues)
    updateCheckboxGroupInput(session, "checkbox_fav2", choiceNames = currentStates$myFavNames, choiceValues = currentStates$myFavValues)
    updateCheckboxGroupInput(session, "checkbox_fav_grant", choiceNames = currentStates$myFavNames, choiceValues = currentStates$myFavValues)
  })

  output$favText2 <- renderText(
    {
      paste0(currentStates$myFavNames, ",")
    }
  )

  budgetamt <- eventReactive(input$action_confirm_pptytype, {input$budget})
  propertyview <- eventReactive(input$action_confirm_pptytype, {input$propertytype})
  propertytypelg <- eventReactive(input$action_confirm_graphview, {input$propertytype2})
  district <- eventReactive(input$action_confirm_graphview, {input$district})
  pricepsf <- eventReactive(input$action_confirm_bto1, {input$psfprice})
  townname1 <- eventReactive(input$action_confirm_bto1, {input$towntype1})
  townname2 <- eventReactive(input$action_confirm_bto2, {input$towntype2})
  budgetID <- eventReactive(input$action_confirm_shortlistview, {input$budgetID})
  districtID3 <- eventReactive(input$action_confirm_shortlistview, {input$districtID3})
  propertyTypeID2 <- eventReactive(input$action_confirm_shortlistview, {input$propertyTypeID2})
  yearID2 <- eventReactive(input$action_confirm_shortlistview, {input$yearID2})
  topxID <- eventReactive(input$action_confirm_shortlistview, {input$topxID})


  output$propertyplot <- renderLeaflet(
    {
      if(propertyview() == "HDB"){ hdb_plotting(budgetamt()) } else { priv_plotting(budgetamt())}})

  output$DistrictsTable <- DT::renderDataTable({ new_districts_tbl %>% select(district, postalsector, location) }, options = list(pageLength = 28), rownames = F)




  DT::renderDataTable({ new_districts_tbl %>% select(district, postalsector, location) }, options = list(pageLength = 28), rownames = F)

  output$lgdistrictpropertyplot <- renderPlotly({
    out <- tryCatch(
      {
        line_graph_district_property(district(), propertytypelg() )},
      error=function(cond){
        print("Oops! We do not have data here. Please change your choice!")
        return(NULL)},
      warning=function(cond){
        print("Oops! We do not have data here. Please change your choice!")
        return(NULL)})
    return(out)
  })


  output$PropPricesTable <- DT::renderDataTable({ table_district_property(district(), propertytypelg() ) }, rownames = F)

  output$BTOleaflet <- renderLeaflet({asking_leaflet()})

  output$price_or_psf_plot <- renderPlotly({
    if(pricepsf() == "PSF"){town_psf(townname1())} else {
      town_price(townname1())}
  })

  output$price_launch_town <- renderPlotly({
    out <- tryCatch(
      {
        launch_price_bto(townname2())
      }, error=function(cond){
        message("Oops! Please confirm your town!")
        return(NULL)})

    return(out)
  })

  output$price_launch_x <- renderPlot({x_year_comp(as.character(input$btoxyearslider[1]), as.character(input$btoxyearslider[2]))})

  output$testplot <- renderPlot(
    {
      transactions1<-transactions %>% base::subset(year %in% input$yearID & propertyType %in% input$propertyTypeID & district %in% input$districtID1)

      ggplot(transactions1,aes(x=as.factor(district) ,y=resale_price, size=area, colour=propertyType)) + geom_point(size=3) + scale_y_log10(labels=formatX) + scale_size_continuous(trans = 'log10') +
        scale_colour_manual(values =  c("blue","green","red","yellow","black","pink","lightblue","purple","grey")) + facet_wrap(~year) + theme(panel.background = element_rect(fill = "gray96")) +labs(title = "District, price and property type", x = "district", colour = "propertytype") + xlab("Price ($)") + ylab("Resale Price ($)")

    }
  )

  output$testplot2 <- renderPlot(
    {
      dfspace<-transactions %>% base::subset(year %in% input$yearID & propertyType %in% input$propertyTypeID & district %in% input$districtID1)
      dfspace<-dfspace %>% mutate(psf=resale_price/area)
      ggplot(dfspace,aes(x=as.factor(year),y=psf, size=area, color=area)) + geom_point() + scale_y_log10(labels=formatX) + labs(title = "Plot of prices over time in selected area",x="Year", y="psf ($)")+labs(size = "Area") +facet_wrap(~propertyType) + scale_fill_discrete(name = "Property Type")
    }
  )
  output$testplot3 <- renderPlotly(
    {
      pricefilter <- budgetID()
      mydata <- as.data.frame(transactions %>% filter(resale_price<pricefilter))

      mydata <- mydata %>% base::subset(year %in% yearID2() & propertyType %in% propertyTypeID2() & district %in% districtID3())
      
      # mydata <- mydata %>% group_by(propertyType,district,year) %>% summarise(Count=n()) %>% arrange(desc(Count))
      mydata <- mydata %>% group_by(propertyType,district,year) %>% count(propertyType,district,year) %>% arrange(desc(n))

      ggplot(mydata,aes(x=as.factor(year), y=n, fill=propertyType)) +geom_col(position='dodge') + labs(title = "Number of Property Type Sold within budget in District/Year", x="Year", y="Count")   #stacked
      
    }
  )

  output$ShortlistTable <- DT::renderDataTable(
    {
      pricefilter <- budgetID()
      topx<- topxID()
      mydata <- as.data.frame(transactions %>% filter(resale_price<pricefilter) %>% mutate(psf=resale_price/area))
      mydata <- mydata %>% base::subset(year %in% yearID2() & propertyType %in% propertyTypeID2() & district %in% districtID3()) %>% arrange(desc(resale_price)) %>% slice(1:topx)

      datatable(mydata)


    })

  output$HidingDistrictTable <- DT::renderDataTable({ new_districts_tbl %>% select(district, postalsector, location) })

  observeEvent(input$hideshowtbl, {
    toggle("HidingDistrictTable")
  })


  observeEvent(input$hideshowmsg, {
    # every time the button is pressed, alternate between hiding and showing the plot
    updateTabsetPanel(session, "navigation", selected = "Guide")
  })


  observeEvent(input$btoxyear, {
    # every time the button is pressed, alternate between hiding and showing the plot
    updateTabsetPanel(session, "navigation", selected = "Guide")
  })

  output$shortlistmsg <- renderUI({
    HTML(paste(paste0("Newly wed or gonna get married? Hear say housing is important for the future but you think you clowning around / clueless? ", emoji('clown_face'), " Knowing what your budget could most likely afford you is like trivia."), "Click around and see the types of properties transacted at or within your budget! ", emoji('moneybag')), "<br/>", sep = "<br/>")
  })

  observeEvent(input$shortlistprop, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("shortlistmsg")
  })

}

shinyApp(ui = ui, server = server)