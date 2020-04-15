# this code makes biofabric plots for CoronaNet dyadic data
# Cindy Cheng, April 15

# ---------------------------
# load packages and functions
# ---------------------------
library(devtools)
library(igraph)
library(remotes)
#remotes::install_github("wjrl/RBioFabric")
library(RBioFabric)
 
# user function for plotting biofabric plots by region, policy type, date, travel mechanism and travel direction
'%!in%' <- function(x,y)!('%in%'(x,y))

plotBioGraph = function(regionName, 
                        policyType, 
                        dateStart, 
                        allDum  = FALSE, 
                        travelMech = c('Flights' ,'All kinds of transport (all below except visa restrictions)'),
                        travelDir = c("Inbound", "Inbound/Outbound")){
  
  # subset data
  sub_data = clean_data %>% filter(date_start<= dateStart) 
  sub_data =  sub_data %>% filter(type == policyType ) 
  sub_data = sub_data %>% separate_rows(travel_mechanism, sep = ',')
  sub_data = sub_data %>% filter(travel_mechanism %in% travelMech)  
  sub_data = sub_data %>% filter(target_direction %in% travelDir )  
  
  # mandatory compliance
  sub_data = sub_data %>% separate_rows(compliance, sep = ',' )  
  sub_data = sub_data %>% filter(compliance %!in% "Voluntary/Recommended but No Penalties")  
  
  sub_data = sub_data %>% filter(target_country!="Other (please specify below)")

  
  # conditional logic for only selecting inbound flight bans external border restrctions
  if(policyType == 'External Border Restrictions'){
    sub_data = sub_data %>% filter(type_sub_cat %!in% c("Health Screenings (e.g. temperature checks)", "Health Certificates", "Travel History Form (e.g. documents where traveler has recently been)") )
    sub_data = sub_data %>% filter(target_who_what %in% c("All (Travelers + Residents)", "All Travelers (Citizen Travelers + Foreign Travelers )") )
    }
  
  ## Note, the following entries are not 'wrong', but are not sweeping travel bans
  # travel restrictions by Azerbaijan for bsuiness trips/public employees
  sub_data = sub_data %>% filter(record_id %!in% c(6286807))
  

  # Bahrain Crown Prince issues directives to halt government official visits and training abroad - March 9 
  sub_data = sub_data %>% filter(record_id %!in% c(3869709 ))
  
  # Iran bans government travel
  sub_data = sub_data %>% filter(record_id %!in% c( 1693792 ))
  
  # Uzbekistan bans student travel
  sub_data = sub_data %>% filter(record_id %!in% c( 7862418 ))
  
  # Macau, not travel ban but health declaration doesn't fit into existing categories
  sub_data = sub_data %>% filter(record_id %!in% c(4638284 ))
  
  # Nepal, travel_mechanism is about allowing flights
  sub_data = sub_data %>% filter(record_id %!in% c(2225508 ))
  ## FIX THE BELOW LATER IN QUALTRICS
  
  # this is not a travel ban for people entering Italy, but for leaving Italy
  sub_data = sub_data %>% filter(record_id %!in% c(3071523 ))
  
  # Micronesia, not a ban on all countries, but unsepcificed countries that have coronacases
  sub_data = sub_data %>% filter(record_id %!in% c(8819062,3625348, 4465391 ))
  
  # Colombia: this is quarantine upon arrival, not external border restriction
  sub_data = sub_data %>% filter(record_id %!in% c(1293754 ))
  
  # Bahrain: these are recommendations, not mandatory bans
  sub_data = sub_data %>% filter(record_id %!in% c(5709516, 9327917 ))
  
  # South Sudan, not a ban on all countries, but on unsepcified countries affected by covid
  sub_data = sub_data %>% filter(record_id %!in% c(5302981))
  
  # Initiating country is Slovenia, not Iran
  sub_data = sub_data %>% filter(record_id %!in% c(3031369))
  
  # Germany: target country is not Germany but 'All countries'
  sub_data = sub_data %>% filter(record_id %!in% c(1592443,3788036, 5258786 ,1592443,3788036,5258786 ))
  
  #Azerbaijan target who what should be 'other' for bsuiness trips/public employees
  sub_data = sub_data %>% filter(record_id %!in% c( 2632978))
  
  #Tunisia target who what should be 'other' for students
  sub_data = sub_data %>% filter(record_id %!in% c( 7066331))
  
  # expand dataset to include disaggregated country data when applicable
  sub_data[which(sub_data$target_country == 'All countries'), 'target_country'] = paste(cid$country_name, collapse = ',')
  sub_data = sub_data %>% separate_rows(target_country, sep = ',')
  
  # remove duplicates
  sub_data = sub_data %>%   
    distinct(init_country, target_country, .keep_all = TRUE)  
  
  # subset to a region, if applicable
  if(allDum == FALSE){
    sub_data = sub_data %>% filter(region %in% regionName)}
  
  # extract edges and nodes for network
  nodes = data.frame(unique(c(sub_data$init_country,sub_data$target_country)))
  edges = data.frame(from = sub_data$init_country,to = sub_data$target_country)

  # create network from subdata
  sub_data_net = graph_from_data_frame(edges, nodes, directed = TRUE)

  #format .pdf size and save pdf of bifabric
  height <- vcount(sub_data_net)
  width <- ecount(sub_data_net)
  aspect <- height / width;
  plotWidth <- 100.0
  plotHeight <- plotWidth * (aspect * 1.2)

  # make biofabric plot
 pdf(paste0('~/Downloads/',paste(c("bioFabric", regionName, policyType, dateStart), collapse = '_'), '.pdf'), width=plotWidth, height=plotHeight)
  bioFabric(passthroughNodeOrder(sub_data_net) )
  dev.off()
}
# ---------------------------
# load data
# ---------------------------
path= '/cloud/project'

clean_data = readRDS(paste0(path, '/data/CoronaNet/coranaNetData_clean.rds')) %>% 
  mutate(date_announced=mdy(date_announced)) %>% 
  filter(date_announced<(today()-days(5)),!is.na(init_country),is.na(init_other),is.na(target_other) | target_other=="")
countries = read.csv(paste0(path, '/data/regions/all_countries.csv'), stringsAsFactors = FALSE)
# ---------------------------
# clean data
# --------------------------- 

# add region variable
clean_data$region = regions_df$regions[match(clean_data$init_country, regions_df$country)] 

# clean date_start
clean_data$date_start= as.Date(clean_data$date_start, '%m-%d-%y')

# create numeric ids per country; needed to create igraph 
country_id = factor(unique(countries$Country))
levels(country_id ) = 1:length(country_id )
country_id = as.numeric(as.character(country_id))
cid = data.frame(country_name = countries$Country, country_id)
clean_data$country_id = cid$country_id[match(clean_data$init_country, cid$country_name)]
clean_data$target_country_id = cid$country_id[match(clean_data$target_country, cid$country_name)]

# clean_data %>% 
#   filter(init_country == 'Portugal' & type == 'External Border Restrictions') %>%
#   filter(date_start<='2020-03-15') %>%
#   filter(travel_mechanism %in%   c('Flights' ,'All kinds of transport (all below except visa restrictions)')) %>% 
#   filter(target_direction %in% c("Inbound", "Inbound/Outbound") ) %>%
#   filter(type_sub_cat %!in% c("Health Screenings (e.g. temperature checks)", "Health Certificates", "Travel History Form (e.g. documents where traveler has recently been)")) %>%
#  # select(event_description, target_country, type, type_sub_cat, record_id, target_direction, travel_mechanism, target_who_what) %>%
# #  select(target_country) 
#   data.frame()
 
# ---------------------------
# make biofabric plots
# ---------------------------
plotBioGraph('all', "External Border Restrictions", "2020-03-15", allDum = TRUE)
plotBioGraph("Europe", "External Border Restrictions", "2020-03-15")
plotBioGraph("Africa",  "External Border Restrictions", "2020-03-15")
plotBioGraph("Asia",  "External Border Restrictions", "2020-03-15")
plotBioGraph("North America",  "External Border Restrictions", "2020-03-15")
plotBioGraph("Middle East",  "External Border Restrictions", "2020-03-15")
plotBioGraph("Oceania",  "External Border Restrictions", "2020-03-15")
plotBioGraph("Latin America",  "External Border Restrictions", "2020-03-15")
plotBioGraph("Central America",  "External Border Restrictions", "2020-03-15")
