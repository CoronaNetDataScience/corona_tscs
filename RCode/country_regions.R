# This code merges the country and region data from the geonames website together 
# It then subsets the data to just the COW countries from the countrycode R package and adds Hong Kong and Macau 

if(Sys.info()['user'] == 'cindycheng'){
	pathData = "/Users/cindycheng/Dropbox/corona_tscs/data"}

 
# -----------------------------
# Load packages
# -----------------------------

library(tidyr)
library(stringr)
library(magrittr)
library(countrycode)
 

# -----------------------------
# Clean region data
# -----------------------------
# read in region data
regions = read.delim(paste0(pathData, "/regions/admin1CodesASCII.txt"), fill = TRUE, header = FALSE, stringsAsFactors = FALSE, na.strings = '')
names(regions) = c('region_id', 'region_name_raw', 'region_name_clean', 'region_code')
regions$ISO2 = as.factor(substr(regions$region_id, 1, 2))
regions = regions[order(regions$ISO2, regions$region_name_clean),]
 
# create variable which keeps track of how many regions a country has
regions$region_number  = unlist(lapply(split(regions$region_name_clean, regions$ISO2), function(x){ 1:length(x)}))

# check the number of regions per country
# table(regions$region_number)
# checked on wiki:   Turkey does have 81 provinces; 
					# Russia has 83
					# Thailand has 77
					# Vietnam has 63

# Slovenia, Latvia, North Macedonia, Malta seem to have an ungodly number of regions
# turns out that they don't actually have provinces, the entries here are for 
# municipalities as that is the next smallest administrative unit after the national level for these countries
# As such, remove these countries from the regions dataset
# Note that since these countries are in the 'country' dataset, their data is not lost, they just won't have 'regions' associated with them
regions = regions[-which(regions$ISO2 %in% c('SI', 'LV', 'MK', 'MT')),] 
 
# remove puerto rico as an independent region and add Puerto Rico to the united states
regions = regions[-which(regions$ISO2 == 'PR'),]

us_data = data.frame(region_id = NA,
					  region_name_raw = "Puerto Rico",
					  region_name_clean = "Puerto Rico",
					  region_code = NA,
					  ISO2 = 'US',
					  region_number = 52)

regions = rbind(regions, 
				us_data )
regions = regions[order(regions$ISO2, regions$region_name_clean),]
 
# -----------------------------
# Merge region and country data
# -----------------------------
# read in country data
country = read.delim(paste0(pathData, "/regions/countryInfo.txt"), fill = TRUE, header = TRUE, skip = 50, na.strings = '')
names(country)[1] = 'ISO2'

# merge region and country data
country_regions = merge(country, regions, by = 'ISO2', all = TRUE) 
country_regions = country_regions[order(country_regions$Country, country_regions$region_name_clean),]


# reshape 
country_regions = country_regions[,c('Country', 'region_number', 'region_name_clean', 'ISO2')] %>% spread(region_number, region_name_clean)

# -----------------------------
# Subset to COW countries, Hong Kong and Macau
# -----------------------------

# note that qualtrics country list has duplicates for south korea and north korea 
# don't really trust it, select countries from countrycode instead
# country_qualtrics = read.csv(paste0(pathData, '/regions/countries_qualtrics.csv'), stringsAsFactors = FALSE)

# select countries
countries = codelist
countries = countries[which(!is.na(countries$cowc) ), c('iso.name.en', 'iso2c')]
countries = countries[which(!is.na(countries$iso2c) ), ]

 
country_regions_clean = merge(country_regions, countries, by.x = 'ISO2', by.y = 'iso2c', all.y = TRUE) 
country_regions_clean = country_regions_clean[, -which(names(country_regions_clean) %in% c("<NA>","iso.name.en"))]

add_countries = matrix(c(rep(NA, 2),
							c('Hong Kong', 'Macau'), 
							rep(rep(NA, 83), 2)), nrow = 2)

add_countries = data.frame(add_countries)
names(add_countries) = names(country_regions_clean)
country_regions_clean = rbind(country_regions_clean, add_countries)
country_regions_clean$Country = as.character(country_regions_clean$Country)
country_regions_clean = country_regions_clean[order(country_regions_clean$Country),]
 

## add Palestine
country_regions_clean = country_regions_clean %>% add_row(Country = "Palestine")

# per RA Cheng-Hao, change entries for Philippines to provinces instead
# of autonomous regions as currently given by geonames
philippines = read.csv(paste0(pathData, '/regions/List of Provinces of the Philippines.csv'), stringsAsFactors = FALSE, header = FALSE)
country_regions_clean[which(country_regions_clean$Country == 'Philippines'), -c(1, 2)] = c(philippines$V1, NA, NA)


# reorder 

country_regions_clean = country_regions_clean[order(country_regions_clean$Country),]

write.csv(country_regions_clean , file = paste0(pathData, '/regions/country_region_clean.csv'), row.names = FALSE, na= "") 


 

## from the code below looks like issue with Slovenia is common to these datasets
# continue to use geoname file because it has more data (e.g. info on land size) on countries
# library(jsonlite)

# # read in raw regions file
# json_file = paste0(pathData, '/regions/Archive/data.json')
# regions_raw <- jsonlite:::fromJSON(txt=json_file, flatten = TRUE)
# head(regions_raw)

# # flatten the data
# regions = do.call(rbind, lapply(1:dim(regions_raw)[1], function(x){

# 	country = regions_raw$countryName[x]
# 	regions = regions_raw$regions[x]

# 	if(length(regions)==2){
# 		rdata = data.frame(regions[1], regions[2])
# 	} else{
# 		rdata = data.frame(regions[1])
# 	}
	
# 	data = cbind(country, rdata)
# 	if(dim(data)[2] == 2){
# 		data = data.frame(data, shortCode = NA)
# 	}
# 	return(data)
# }))

# regions$region_number = unlist(lapply(split(regions$name, regions$country), function(x){ 1:length(x)}))


