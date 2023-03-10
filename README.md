# Property App with R Studio

## Summary
- The aim is to create a useful property application that could help newlyweds to find their ideal home.
- This was a team project which I mainly contributed to the scraping of data from SRX site, cleaning the data, building the main page and favourites page.

## Scraping Web Data
### Property listings data
R script file srx.crawl.R contains the codes used to crawl the properties data from srx.com.sg.
More specifically, each different property type was crawled separately, then combined in the data-combined.Rmd file. Due to constant changes and updates in the listings every moment, there are some duplicates to be found in the crawled data set which were removed.

### Amenities data
The amenities dataset was formed in two parts. Firstly, a sizable number of pages were scraped by selecting pages from different regions in equal proportion (regardless of property type), with the scraped information only being the amenities and their locations from the SRX website. R script file srx-amen_scraping.R contains the codes used to crawl the SRX-listed amenities with their locations. Next, since we discovered that the data scraped was insufficient for the amenities, we found additional/replaceable data sources for whichever amenities data columns possible. The sources and combining process is also found in data-combined.Rmd file. Amenities overall included those which newly-weds might require: trains, buses, shopping malls, marts, pre-schools, primary schools, secondary schools, hawker centre, parks etc.

### Financial Historical Transactions Data
To track trends and create visualizations based on past data, previous transactions were taken from the URA government website for the past years. The data, some of which were in JSON format was then converted to csv format and cleaned as well, as can be seen in the fin_transactions.rmd file.

HDB transactions data 2017 onwards: https://data.gov.sg/dataset/resale-flat-prices
Private properties transactions data 2016 onwards: https://www.ura.gov.sg (api was requested)
BTO transactions data 2001 onwards: https://www.teoalida.com/singapore/btolist/
Indicative Polygon of Planning area boundary (SHP) files: https://data.gov.sg/dataset/master-plan-2014-planning-area-boundary-no-sea

## HomeSweetHome - hSh App Description
Page 1: Home page - This page serves to be a starting point for our users to explore the different properties that are on sale in the property market. Users can also save listings which they are interested in to compare them later in page 3.

Page 2: Analyse Past Transactions - This page is for users to explore past financial transactions.

Page 3: Comparing Favorites - Users can compare their favourited lisitngs by amenities nearby and grants provided.

Page 4: Guide - This page helps guides users through the usage of each of the above pages and how to navigate through them.

## Limitations
- Data is static
- Amenities dataset is not comprehensive
- Cleanliness of dataset used from the SRX website is subject to human error.
