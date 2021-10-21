rm(list=ls())

getwd()
setwd('/Users/.../Buffalo Housing')

library(tidyverse)
library(rvest)
library(xml2)
library(tibble)
library(stringi)
library(lubridate)
library(scorecard)
library(data.table)
library(finalfit)
library(stargazer)



###create urls for all webpages to be used
webpage <- stri_paste("https://www.zillow.com/buffalo-ny/", 1:18, '_p/')


##empty df to store data from pages
data <- data.frame()

##loop through known pages to pull data on all sale listings and store into df
for (i in webpage) {
  Red <- read_html(i)
  houses <- Red %>% html_elements('article')
  info <- houses %>% html_element('div')
  address <- info %>% html_element('a') %>% html_text()
  price <- info %>% html_element('div .list-card-price') %>% html_text() 
  details <- info %>% html_element('.list-card-details') %>% html_text()
  bedrooms <- info %>% html_element('.list-card-details li:nth-child(1)')  %>% html_text() %>% str_remove('bds')
  sqft <- info %>% html_element('.list-card-details li:nth-child(3)')  %>% html_text() %>% str_remove('sqft')
  bath <- info %>% html_element('.list-card-details li:nth-child(2)')  %>% html_text() %>% str_remove('ba') 
  records <- tibble(address = address, price = price, details = details, bedrooms = bedrooms, bath = bath, sqft = sqft)
  data <- rbind(data, records)
  
}


##drop excess data, keep house listings saved to csv (update month/year)
rm(list = setdiff(ls(), 'data'))
unique(data)
write.table(Zestimate_scrape, file = 'Zillow_listingsAug21.txt', sep = '\t', row.names = FALSE)


#####grab zestimte for live pricing for units in buffalo, NY####
#create URLs for iteration on webscrape loop
Buff_address <- read.csv('2020-2021_Assessment_Roll.csv')

Buff_address$HOUSE <- paste(Buff_address$HOUSE.NUMBER, Buff_address$STREET, sep = '%20')
Buff_address$ID <-  paste(Buff_address$HOUSE, Buff_address$CITY, Buff_address$STATE, Buff_address$ZIP.CODE..5.DIGIT., sep = '-')

##narrow down search to just family homes 
House_address <- Buff_address %>% filter(PROPERTY.CLASS %in% c(210, 220, 230))

###datafrome to temporarily store scrape data
data1 <- data.frame()

##iterate through city lot addresses and scrape zestimate
##test with only 50 rows from list
Test <- House_address[10001:65824, ]



##spawn urls 
for (i in Test$ID) {
  addresses <- paste0('https://www.zillow.com/homes/', i, '_rb/')
  records <- tibble(addresses = addresses)
  data1 = rbind(data1, records)
}
##need to break into smaller parts to split job up over time
ratio <- rep(0.025, times = 40)
df = list()

for (i in 1:40) {
  name <- stri_paste('df', i)
  df <- rbind(df, name)
}

LIST <- split_df(data1, ratios = ratio, name_dfs = df)
list2env(LIST, envir = .GlobalEnv)

rm(i)
rm(records)

data2 <- data.frame()

##webscrape, tryCatch to skip URLs that are not active on Zillow 
for (i in df40$addresses){
  tryCatch(
    error = function(e) NA,{
    Url <- read_html(i)
    det <- Url %>% html_elements('div .ds-home-details-chip')
    Addr <- det %>% html_element('h1') %>% html_text()
    det1 <- det %>% html_element('.ds-bed-bath-living-area-container') %>% html_text()
    bd <- det %>% html_element('.ds-bed-bath-living-area-container span:nth-child(1)') %>%    html_text()
    Zest_det <- det %>% html_element('p')
    Market_class <- Zest_det %>% html_element('span:nth-child(1)') %>% html_text()
    Zestimate <- Zest_det %>% html_element('span:nth-child(2)') %>%html_text() %>%     str_remove('Zestimate®: $')
    Rent_Zestimate <- Zest_det %>% html_element('span:nth-child(3)') %>% html_text() %>% str_remove('Rent Zestimate®: $')
    records <- tibble(det = det1, bd = bd, Addr = Addr, Market_class = Market_class, Zestimate = Zestimate, Rent_Zestimate = Rent_Zestimate)
    End_time <- Sys.time()
    data2 <- rbind(data2, records)
    }
  )

}

data2 <- data2 %>% separate(Addr, c('Street', 'City', 'State'), ',', remove = FALSE)
off_market <- filter(data2, Market_class == 'Off market')
off_market$Zestimate <- substring(off_market$Zestimate, 14, nchar(off_market$Zestimate))
as.numeric(off_market$Zestimate)
off_market$Rent_Zestimate <- substring(off_market$Rent_Zestimate, 19, nchar(off_market$Rent_Zestimate))
off_market$Rent_Zestimate <- off_market %>% str_remove(Rent_Zestimate, '/mo')
as.numeric(off_market$Rent_Zestimate)

off_market$Access_date <- today()

##drop excess data from loop & save to csv
rm(setdiff(ls(), c('data1', 'data2', 'off_market', 'Buff_address', 'House_address')))

###only used for first file of the month, else append
write.csv(data2, 'Zestimate_scrapeJuly21.csv')

##run for subsequent scrapes
Zestimate_scrape <- read_csv('Zestimate_scrapeJuly21.csv')

Zestimate_scrape <- subset(Zestimate_scrape, select = -X1)
Zestimate_scrape <- rbind(Zestimate_scrape, off_market) 
write.csv('Zestimate_scrapeJuly213.csv')

##write text file for PC 
write.table(Zestimate_scrape, file = 'Zestimate_scrape2.txt', sep = '\t', row.names = FALSE)

read.delim('Zestimate_scrape.txt', )

###scrape recently sold properties to compare actual closing price w/ zestimate & appraisal data


##confirm the pagination
page <- stri_paste('https://www.zillow.com/buffalo-ny/sold/', 0:20, '_p/')
Sold_scrape <- data.frame()

for (i in page) {
 Url <- read_html(i)
 House <- Url %>% html_elements('article')
 Agent_det <- House %>% html_element('div .list-card-footer') %>% html_element('p') %>% html_text()
 Top <- House %>% html_element('div .list-card-top') 
 Sale_date <- Top %>% html_element('div .list-card-variable-test.list-card-img-overlay') %>% html_text()
 Sale_price <- House %>% html_element('div .list-card-price') %>% html_text()
 Sold_address <- House  %>% html_element('a') %>% html_text()
 records <- tibble(Agent_det = Agent_det, Sale_date = Sale_date, Sale_price = Sale_price, Sold_address = Sold_address)
 Sold_scrape <- rbind(Sold_scrape, records)
}

##clean variables for merge of datasets
Sold_scrape <- Sold_scrape %>% separate(Sold_address, c('Street', 'City', 'State'), ',', remove = FALSE)
Sold_scrape <- Sold_scrape

##cleanup workspace
rm(setdiff(ls(), c('data1', 'data2', 'off_market', 'Sold_scrape', 'Buff_address', 'House_address')))

###analyze housing market and zillow data acurracy using scraped data w/ assessment data
##make unique ID from street and zip 
House_address$UNIQUE <- (House_address$STREET %+% House_address$ZIP.CODE..5.DIGIT.)
Sold_scrape$UNIQUE <- (Sold_scrape$Street %+% Sold_scrape$ZIP)

Sold_scrape <- Sold_scrape %>% separate(Sold_address, c('Street', 'City', 'State'), ',', remove = FALSE)
Sold_scrape2 <- Sold_scrape
Sold_scrape2$Street <- word(Sold_scrape2$Street, 1, 2)
Sold_scrape2$Address <- paste(Sold_scrape2$Street, Sold_scrape2$City, sep = ',')
Sold_scrape2$UNIQUE <- paste(Sold_scrape2$Address, Sold_scrape2$State, sep = ',')


Sold_scrape2$UNIQUE <- toupper(Sold_scrape2$UNIQUE)

Sold <- left_join(Sold_scrape2, House_address, by = 'UNIQUE')
write.csv(Sold, 'Sold_Oct.csv')

#------------------------------------------------------------------------------------------------------#
House_address <- left_join(House_address, Sold_scrape, by = 'UNIQUE')

###visualization of assesesed and sold prices 
#first clean date format
House_address$Month_Yr <- format(as.Date(House_address$DEED.DATE), '%Y-%M')
plot(House_address$Month_Yr, House_address$TOTAL.VALUE, type = 'l', col = 'red')
lines(House_address$Sale_Date, House_address$Sale_Price, type = 'l', col = 'green')

##combine date and price variables to forcast and determine accuracy of Zestimate 
House_address$Comb_Date <- House_address %>% mutate(mycol = coalesce(DEED.DATE, Sale_data))  
House_addressComb_Price <- House_address %>% mutate(mycol = coalesce(TOTAL.VALUE,Sale_Price)) 

Price_model <- House_address %>% lm(Comb_Price~Comb_Date)

####model to incorporate resident demographics 
ACS_PBuff <- read.csv('ACS_PBuff')
ACS_HBuff <- read.csv('HBuff')

##compare average housing 
Zest_scrape2 <- read.delim('Zestimate_scrape_merge', header = TRUE, sep = '\t')
Zestimate_scrape3 <- rbind(Zest_scrape2, Zestimate_scrape)
Zest_unique <- unique(Zestimate_scrape3, by = Zestimate_scrape3$Street)

write.table(Zest_unique, file = 'Zest_2', sep = '\t', row.names = TRUE, dec = '.')

##load data for ArcGIS cleaning and analysis
Housing_Character <- read.delim('export.txt', header = TRUE, sep = ',')

##trim GEOID to merge with shapefiles in GIS
Housing_Character$GEOID <- str_sub(Housing_Character$GEOID, start = 8, end = 20)
write.csv(Housing_Character, 'Housing_character.csv')

##select only housing value characteristics
Housing_Character2 <- select(Housing_Character,c(GEOID, B25075e1:B25093e29,B25003e3, B25003m3, B25008e1, B25008m1 ))

Zest_unique <- read.csv('Zest_unique.csv')
write_csv(Housing_Character2, 'Housing_Character2.csv')

Housing_Character <- read.csv('Housing_Character.csv')
Buff_tracts <- read.csv('Tracts_data.csv')
Buff_blocks <- read.csv('Block_Groups_data.csv')
##clear workspace except for Zestimate scrapte, listings, and assessed values
rm(list= ls()[!(ls() %in% c('Zest_unique','Buff_address', 'House_address', 'Housing_Character', 'Housing_Character2', 'Buff_tracts', 'Buff_blocks'))])

write.csv(Zest_unique, 'Zest_unique.csv')

###bring in ACS DATA and separate by race, income, and housing characteristics


####graphs#####
Zest_unique$Zestimate <- gsub(',', '', Zest_unique$Zestimate)
Zest_unique$Zestimate <- as.numeric(Zest_unique$Zestimate)

Zestimate <- Zest_unique
Zestimate <- Zestimate[!is.na(Zestimate$Zestimate), ]

density_object <- density(Zestimate$Zestimate)
plot(density_object)
density.default(x = Zestimate$Zestimate)


###combine tax assessed data with zestimate scrapes
##create unique ID based on address
House_address$UNIQUE <- paste(House_address$ADDRESS, House_address$CITY, sep = ', ')
House_address$STATE_ZIP <- paste(House_address$STATE, House_address$ZIP.CODE..5.DIGIT., sep = ' ')

House_address$UNIQUE <- paste(House_address$UNIQUE, House_address$STATE_ZIP, sep = ', ')

Zest_unique$Clean_street <- word(Zest_unique$Street, 1, 2)
Zest_unique$City_clean <- str_sub(Zest_unique$City, start = 2, end = 9)
Zest_unique$Address <- paste(Zest_unique$Clean_street, Zest_unique$City_clean, sep = ', ')
Zest_unique$UNIQUE <- paste(Zest_unique$Address, Zest_unique$State, sep = ',')
Zest_unique$UNIQUE <- toupper(Zest_unique$UNIQUE)

##combine Zestimate $tax assessment by UNIQUE
Combined <- left_join(House_address, Zest_unique, by = 'UNIQUE')

##drop rows that did not successfully merge
Full_df <- Combined[!is.na(Combined$Zestimate),]
Full_df <- Full_df[!duplicated(Full_df[ , 'UNIQUE']),]

Full_df$Price_Difference <- (Full_df$Zestimate - Full_df$TOTAL.VALUE)
Full_df$ln_diff <- ((Full_df$Price_Difference/Full_df$Zestimate))

write.csv(Full_df, 'Full_Housing.csv')

Full_df <- read.csv('Full_Housing.csv')
##graphing analysis
Price_Diff <- density(Full_df$ln_diff)
plot(Price_Diff) 
density.default(x = Full_df$ln_diff)

##check equilization rate of 85%
Check <- (sum(Full_df$ln_diff)/287)

##aggregate housing prices for census blocks
House_Avg <- setDT(Full_df)[, .(mean_price = mean(Price_Difference)), by = GEOID]
House_Avg2 <- setDT(Full_df)[, .(Mean_Zest = mean(Zestimate)), by = GEOID]

##load demographic details from ACS 5-yr 
Housing_Character <- read.delim('export.txt', header = TRUE, sep = ',')

##trim GEOID to merge with shapefiles in GIS
Housing_Character$GEOID <- str_sub(Housing_Character$GEOID, start = 8, end = 20)

##limit selection to Buffalo, NY
Buff_tracts <- read.csv('Tracts_data.csv')

Census_blocks <- as.list(Buff_tracts$GEOID10)
Block_groups <- read.csv('Block_Groups_data.csv')

Housing_Character$GEOID <- as.character(Housing_Character$GEOID)
Buff_tracts$GEOID10 <- as.character(Buff_tracts$GEOID10)


##select only housing value characteristics
Housing_Character$GEOID <- str_sub(Housing_Character$GEOID, start = 8, end = 20)
Block_groups$GEOID10 <- as.character(Block_groups$GEOID10)
Housing_Character2 <- filter(Housing_Character, Housing_Character$GEOID %in% Block_groups$GEOID10)
Housing_Character2 <- select(Housing_Character2,c(GEOID, B25075e1:B25093e29))

##we are only concerned with owner occupied in occupied units not total housing becasue vacants would skew results 
Housing_Character2$Ownership <- (Housing_Character2$B25003e2/Housing_Character2$B25003e1)

#make inference based on renter concentration
Housing_Character2$Renter <- (1-Housing_Character2$Ownership)

Housing_Character <- Housing_Character2

write.csv(Housing_Character, 'Housing_Character2.csv')

##select income information
Income_Character <- read.delim('Income.txt', header = TRUE, sep = ',')

Income_Character$GEOID <- str_sub(Income_Character$GEOID, start = 8, end = 20)
Block_groups$GEOID10 <- as.character(Block_groups$GEOID10)
Income_Character2 <- filter(Income_Character, Income_Character$GEOID %in% Block_groups$GEOID10)
Income_Character <- select(Income_Character2, c(GEOID, B19001e1:B19025m1))

#create another df for welfare analysis
Welfare_Character2 <- select(Income_Character2, c(GEOID, B19001e1:B19025Im1))

Welfare_Character$GEOID <- str_sub(Welfare_Character$GEOID, start = 8, end = 20)
Block_groups$GEOID10 <- as.character(Block_groups$GEOID10)
Welfare_Character2 <- filter(Welfare_Character, Welfare_Character$GEOID %in% Block_groups$GEOID10)

Income_Character <- Income_Character2

write.csv(Income_Character, 'Income_Character2.csv')
##select race characteristics
Race_Character <- read.delim('Export_Output2.txt', header = TRUE, sep = ',')

#limit to only Buffalo census blocks
Race_Character$GEOID <- str_sub(Race_Character$GEOID, start = 8, end = 20)
Block_groups$GEOID10 <- as.character(Block_groups$GEOID10)
Race_Character2 <- filter(Race_Character, Race_Character$GEOID %in% Block_groups$GEOID10)

Race_Character <- Race_Character2

Race_Character2 <- read.delim('Race_Character2.csv', header = TRUE, sep = ',')

#clean racial characteristics for analysis, make minority proportion variable 
#subtract Asian, Latino, black from total census block population ***need to clarify what is white vs. Latino
Race_Character2$Minority <- (1 -( Race_Character2$B02001e2/Race_Character2$B02001e1))

hist(Race_Character2$Minority, breaks = 50)
##Select migration characteristics
Migration_Character <- read.delim('Export_Output.txt', header = TRUE, sep = ',')

#limit to only Buffalo census blocks
Migration_Character$GEOID <- str_sub(Migration_Character$GEOID, start = 8, end = 20)
Block_groups$GEOID10 <- as.character(Block_groups$GEOID10)
Migration_Character2 <- filter(Migration_Character, Migration_Character$GEOID %in% Block_groups$GEOID10)


##model for race as a predictor or price assessment 
Full_df$CENSUS.TRACT <- as.character(Full_df$CENSUS.TRACT)

##fill leading zeros for census tract ID in order to merge with census data
Full_df$CENSUS.TRACT <- str_pad(Full_df$CENSUS.TRACT, 6, pad = '0')
Full_df$GEOID <- paste('360290', Full_df$CENSUS.TRACT, sep = '')
Full_df$GEOID <- as.character(Full_df$GEOID)
Full_df2 <- merge(Full_df, Race_Character[, c('GEOID', 'Minority')], by = 'GEOID', all.x = TRUE)

Race_Character2 <- left_join(Race_Character2, House_Avg, by = 'GEOID')

##Separate median income for block groups and merge with anaylsis df
Income_Character$Median_Inc <- Income_Character$B19013e1
Combined_df <- Race_Character2
Combined_df <- left_join(Combined_df, Income_Character, by = 'GEOID')

Analysis_DF <- left_join(Race_Character2, Housing_Character2, by = 'GEOID')
Analysis_DF$GEOID <- as.character(Analysis_DF$GEOID)
#Zestimate vs tax assessed
#create variable for yard size 
Analysis_DF2 <- left_join(Full_df, Analysis_DF, by = 'GEOID')

#Rent Estimate vs. median income
#median household income
rename(Income_Character, Median_Inc = B19013e1)

Analysis_DF2 <- left_join(Analysis_DF2, Income_Character, by = 'GEOID')
Analysis_DF2$Rent_Zestimate <- str_remove(Analysis_DF2$Rent_Zestimate, ',')
Analysis_DF2$Rent_Zestimate <- substr(Analysis_DF2$Rent_Zestimate, 1, nchar(Analysis_DF2$Rent_Zestimate)-3)

Analysis_DF2$Monthly_Income <- (Analysis_DF2$B19013e1/12)

#remove obs that don't report income
Analysis_DF2$Monthly_Income <- as.character(Analysis_DF2$Monthly_Income)
Analysis_DF2$ZIP.CODE..4.DIGIT. <- NULL
Analysis_DF3 <- na.omit(Analysis_DF2)

Analysis_DF3$Perc_Diff <- ((Analysis_DF3$Zestimate-Analysis_DF3$TOTAL.VALUE)/Analysis_DF3$TOTAL.VALUE)
Analysis_DF3$YARD.SIZE <- ((Analysis_DF3$FRONT * Analysis_DF3$DEPTH)-Analysis_DF3$FIRST.STORY.AREA)
Analysis_DF3$Monthly_Income <- as.numeric(Analysis_DF3$Monthly_Income)
Analysis_DF3$Rent_Zestimate <- as.numeric(Analysis_DF3$Rent_Zestimate)
Analysis_DF3$Rent_Affordabilty <- (Analysis_DF3$Rent_Zestimate/ Analysis_DF3$Monthly_Income)
Analysis_DF3$Affordable <- ifelse(Analysis_DF3$Rent_Affordabilty >= 0.3, 1, 0)

Affordability_ratio <- (sum(Analysis_DF3$Affordable)/28129)

#Zestimate vs. median income

Analysis_DF3 %>% rename(Median_Inc = B19013e1)

##export Analysis_DF3 for maps in arcGIS
write.csv(Analysis_DF3, 'Analysis_DF.csv')

#----------------------------------------------------------------------------------

##model 
Simple_model <- lm(Perc_Diff ~ Minority, data = Analysis_DF3)
Assessed_model <- lm(Perc_Diff ~ Minority + B19013e1 + OVERALL.CONDITION + TOTAL.LIVING.AREA + YEAR.BUILT + YARD.SIZE + X..OF.BEDS + X..OF.BATHS, data = Analysis_DF3)

summary(Assessed_model)
ggplot(Assessed_model)

stargazer(Simple_model, Assessed_model, type = 'text', title = 'OLS Results', align = TRUE, dep.var.labels = 'Value Gap', covariate.labels = c('Percent Minority', 'House Condition', 'Median Income', 'Square Feet', 'Yard Size', 'Beds', 'Baths'), p.auto = TRUE, digits = 3, out = 'Models.txt')

model <- lm(mean_price ~ Minority + Median_Inc, data = Combined_df)
summary(model)

ggplot(Combined_df, aes(x = Minority, y = mean_price)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

plot(Race_Character2$Minority, Race_Character2$mean_price)
summary(Race_Character2$Minority)
summary(Race_Character2$mean_price)

#Zestimate vs ownership
Ownership_model <- lm(mean_price ~ Ownership + Minority, data = Analysis_DF)
summary(Ownership_model)

#-------------------------------------------------------------------------------

###export df with variables for arcGIS

House_Avg2 <- setDT(Analysis_DF3)[, .(mean_price = mean(Perc_Diff)), by = GEOID]

Arc_Map1 <- data.frame(Race_Character2$Minority, Race_Character2$GEOID)
Arc_Map2 <- data.frame(Housing_Character2$Ownership, Housing_Character2$GEOID)
Arc_Map3 <- data.frame(Income_Character$B19013e1, Income_Character$GEOID)
Arc_Map4 <- data.frame(House_Avg2$GEOID, House_Avg2$mean_price)

Arc_Map1$GEOID <- Arc_Map1$Race_Character2.GEOID
Arc_Map2$GEOID <- Arc_Map2$Housing_Character2.GEOID
Arc_Map3$GEOID <- Arc_Map3$Income_Character.GEOID
Arc_Map4$GEOID <- Arc_Map4$House_Avg2.GEOID

Arc_Map1 <- left_join(Arc_Map1, Arc_Map2, by = 'GEOID')
Arc_Map1 <- left_join(Arc_Map1, Arc_Map3, by = 'GEOID')
Arc_Map1 <- left_join(Arc_Map1, Arc_Map4, by = 'GEOID')

write.csv(Arc_Map1, 'Arc_Map.csv')

##sold model to show distribution of sold properties and compare accuracy with Zestimate
Sold_scrape <- Sold_scrape %>% separate(Sold_address, c('Street', 'City', 'State'), ',', remove = FALSE)
Sold_scrape2 <- Sold_scrape
Sold_scrape2$Address <- paste(Sold_scrape2$Street, Sold_scrape2$City, sep = ',')
Sold_scrape2$UNIQUE <- paste(Sold_scrape2$Address, Sold_scrape2$State, sep = ',')
Sold_scrape2$Street <- word(Sold_scrape2$Street, 1, 2)

Sold_scrape2$UNIQUE <- toupper(Sold_scrape2$UNIQUE)

Sold <- left_join(Sold_scrape2, House_address, by = 'UNIQUE')
