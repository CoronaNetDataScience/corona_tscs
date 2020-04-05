

if(Sys.info()['user'] == 'cindycheng'){
	pathData = "/Users/cindycheng/Dropbox/corona-govt-response/Data"}

library(stringr)
library(dplyr)

# get eu regions
eu = read.csv(paste0(pathData, '/regions/eu_countries.csv'), stringsAsFactors = FALSE)
eu = eu[-which(eu$English == ""),]
eu$English = str_trim(eu$English)

# get regions
country_regions = read.csv(paste0(pathData, '/regions/country_region_clean.csv', stringsAsFactors = FALSE)
country_regions$eu = ifelse(country_regions$Country %in% c(eu$English), 1, 0)
country_regions = country_regions %>% add_row(Country = c('Palestine'))
 
 
# load data
data = read.csv(paste0(pathData, '/CoronaNet/preData/corona_events_for_validation_cc.csv'), stringsAsFactors = FALSE)
data$target_country_regions = str_trim(data$target_country_regions )

# expand dataset tho account for 'exceptions'
data[which(data$init_country == 'Myanmar'
			& data$target_country_regions == 'South Korea' 
			& data$target_province == 'EXCEPT: Daegu and Gyeongbuk Provinces'), 'target_province'] = paste(setdiff(c(country_regions[which(country_regions$Country == 'South Korea'),c(3:19)]), c('Daegu', 'Gyeongsangbuk-do')), collapse = ',')


data[which(data$init_country == 'Netherlands'
			& data$target_country_regions == 'All Countries' 
			& data$target_country_exception == 'EXCEPT: European Union and United Kingdom'), 'target_country_regions'] # = paste(country_regions$Country[which(country_regions$eu == 0 &country_regions$Country != 'United Kingdom')], collapse = ',')

 
 data[which(data$init_country == 'Norway'
			& data$target_country_regions == 'All Countries' 
			& data$target_country_exception == 'EXCEPT Nordic countries'), 'target_country_regions'] # = paste(setdiff(country_regions$Country, nordic), collapse = ',')


 data[which(data$init_country == 'Switzerland'
			& data$target_country_regions == 'All Countries' 
			& data$target_country_exception == 'AND EU countries except Italy, Germany, Austria, France'), 'target_country_regions'] # = paste(c(country_regions$Country[which(country_regions$eu ==0)],  country_regions$Country[country_regions$Country %in% c('Italy', 'Germany', 'Austria', 'France')]), collapse = ',')
 
data[which(data$init_country == 'European Union'
			& data$target_country_regions == 'All Countries' 
			& data$target_country_exception == 'EXCEPT Ireland'), 'target_country_regions']  = paste(country_regions$Country[which(country_regions$eu == 0 &country_regions$Country != 'Ireland')], collapse = ',')

data[which(data$init_country == 'Australia'
			& data$target_country_regions == 'All Countries' 
			& data$target_country_exception == 'EXCEPT: New Zealand'), 'target_country_regions'] = paste0(setdiff(country_regions$Country, 'New Zealand'), collapse = ',')


data[which(data$init_country == 'New Zealand'
			& data$target_country_regions == 'All Countries' 
			& data$target_country_exception == 'EXCEPT: Australia'), 'target_country_regions'] = paste0(setdiff(country_regions$Country, 'Australia'), collapse = ',')


data$record_id = factor(paste0(data$event_description, data$type, data$target_who_what))
levels(data$record_id) = 1:length(levels(data$record_id))
 
# expand rows
data = data %>% separate_rows(target_country_regions, sep = ',')
data = data %>% separate_rows(target_province, sep = ',')
data = data %>% select(-target_country_exception)

data = data[, c(27, 1:26)]

write.csv(data, file = paste0(pathData,'/corona_events_for_validation_clean_cc.csv'), row.names = FALSE) 