# ---------------------
# Data Harmonization
# By Cindy Cheng
# Edited by Marco Waldbauer, Isaac Bravo and Rohan Bhavikatti
# ---------------------

# This code does the following:

# A) combines data from the following external datasets which have already been mapped into the CoronaNet taxonomy 
# in Step 1 of the data harmonization process into one dataset in the object ext_data:

# ACAPS
# CIHI
# COVIDAMP
# John Hopkins/JHU HIT Covid
# OxCGRT
# WHO PHSMs


# B) Step 2: 

# i) cleans the country and province fields in the combined ext_data object

# ii) subsets the ext_data object of data that is irrelevant to the main CoronaNet data into the object ext_data_sub, specifically with respect to
# data on economic policies
# data on countries CoronaNet does not collect data for 

# C) Step 3

# Deduplicates the ext_data_sub object 
# a) for each individual external dataset relative to itself: ext_data_sub_within
# b) for all the external datasets relative to each other : ext_data_sub_across
# c) for the external dataset relative to the coronaNet data: ext_data_sub_coronanet

# The code further

# D) provides an assessment of the data quality of the external dataset
# e.g. how rich are the descriptions
# how many policies have missing links


# ---------------------
# 0) load packages and functions
# ---------------------

library(dplyr)
library(magrittr)
`%!in%` <- Negate(`%in%`)
library(ggplot2)
library(readxl)
library(stringr)
library(stringdist)
library(tidyr)
library(readr)
library(here)
library(lubridate)
library(googlesheets4)


capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


# Greedy assignment heuristic (Your favorite heuristic here)
greedyAssign <- function(a,b,d){
  x <- numeric(length(a)) # assgn variable: 0 for unassigned but assignable,
  # 1 for already assigned, -1 for unassigned and unassignable
  while(any(x==0)){
    min_d <- min(d[x==0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
    a_sel <- a[d==min_d & x==0][1]
    b_sel <- b[d==min_d & a == a_sel & x==0][1]
    x[a==a_sel & b == b_sel] <- 1
    x[x==0 & (a==a_sel|b==b_sel)] <- -1
  }
  cbind(a=a[x==1],b=b[x==1],d=d[x==1])
}

 

library(data.table)
# ----------------------------------
# 0) load data data 
# ----------------------------------
# load coronanet data;  this version of the coronanet data has been formatted to for data harmonization
qualtrics_2b <- read_csv("data/collaboration/qualtrics_2b_reformatted_for_harmonization.csv.gz")

# load mapped data
# read_csv produces parsing failures, use read.csv
who_coronanet_map<- read.csv( file ="./data/collaboration/who/who_coronanet_map_2b.csv" ) 
hit_coronanet_map<- read.csv( file ="./data/collaboration/jhu/hit_coronanet_map_2b.csv" )  
cihi_coronanet_map<- read.csv( file ="./data/collaboration/cihi/cihi_coronanet_map.csv" )     
amp_coronanet_map<- read.csv( file ="./data/collaboration/covidamp/amp_coronanet_map.csv" )   
acaps_coronanet_map<- read.csv( file ="./data/collaboration/acaps/acaps_coronanet_map.csv" )   
ox_coronanet_map<- read.csv( file ="./data/collaboration/oxcgrt/ox_coronanet_map.csv.gz" , na.strings = '') 
 
# load data on country and province names
country_list <- read.csv('./data/collaboration/countryList.csv') %>%
  mutate(Country = ifelse(Country == 'United States', "United States of America", Country))

country_regions <- read_csv(file = 'data/collaboration/country_region_clean.csv')

country_names <-
  read_excel("./data/collaboration/ISO WORLD COUNTRIES.xlsx", sheet = "ISO-names") %>%
  mutate(
    ADMIN =
      recode(
        ADMIN,
        
        `East Timor` = "Timor Leste",
        `Macao S.A.R` = "Macau",
        `The Bahamas` = "Bahamas",
        `Republic of Congo` = "Republic of the Congo",
        `Czech Republic` = "Czechia",
        `Federated States of Micronesia` = "Micronesia",
        `Guinea Bissau` = "Guinea-Bissau",
        `Hong Kong S.A.R.` = "Hong Kong",
        `Macao S.A.R.` = "Macao",
        `Macedonia` = "North Macedonia",
        `Republic of Serbia` = "Serbia",
        `Swaziland` = "Eswatini",
        `United Republic of Tanzania` = "Tanzania"
      )
  )
country_names  = country_names %>% 
  mutate(ISO_A3 =case_when(
    ADMIN == 'Kosovo' ~ 'XKX',
    TRUE ~ ISO_A3),
    ISO_A2 =case_when(
      ADMIN == 'Kosovo' ~ 'XK',
      TRUE ~ ISO_A2
    )
  )

# ---------------------------
# A) create external dataset
# ---------------------------
ext_data = plyr:::rbind.fill(
  amp_coronanet_map %>% mutate(dataset = 'COVIDAMP', unique_id = paste0('COVIDAMP_', unique_id), ox_id = NA) %>% 
    select(unique_id, description,ISO_A3, ISO_A3, init_country_level, country, province, type, type_sub_cat, type_alt, type_alt_2, travel_mechanism, institution_cat, compliance, target_geog_level,target_country, target_province, target_city, target_other,  target_who_what, target_who_gen, date_announced, date_start, date_end, dataset, link, pdf_link),
  who_coronanet_map %>% 
    filter(!grepl('ACAPS|JH_HIT|OXCGRT|OxCGRT', unique_id)) , 
  ox_coronanet_map %>% 
    
    mutate(description_orig = description,
           description = description_nourl)%>%
    select(-description_orig) %>%
    filter(country !='Canada') %>% # oxford uses cihi data
    mutate(dataset = 'OXCGRT', unique_id = paste0('OXCGRT_', unique_id)),
  hit_coronanet_map %>%
    filter(country %!in% c("Bulgaria", "Luxembourg", "Poland")) %>% # Kyle Oliver already integrated all HIT-COVID data for these countries
    mutate(dataset = 'JHU', unique_id = paste0('JHU_', unique_id)) ,
  cihi_coronanet_map %>% mutate(ISO_A3 = "CAN", dataset = 'CIHI', unique_id = paste0('CIHI_', unique_id)) ,
  acaps_coronanet_map %>% mutate(dataset = 'ACAPS', unique_id = paste0('ACAPS_', unique_id)) ) %>%
  select(-count_raw)

# ---------------------------
# B) i) clean  external dataset
# ---------------------------

# standardize country names and type
ext_data = ext_data %>%
  select(-country)%>%
  left_join(country_names %>% rename(country = ADMIN) %>% select(country, ISO_A3), by = 'ISO_A3')%>%
  arrange(country, type, date_start)%>%
  mutate(type= str_trim(type))


## standardize province names

# read in data on standard province names used in CoronaNet and reformat them for data harmonization
country_regions_long <- country_regions %>%
  pivot_longer(
    cols = -c(Country, ISO2),
    names_to = "p_num",
    values_to = "init_2prov"
  ) %>% # Create country regions df with ids
  mutate(p_num = as.numeric(p_num) + 1) %>%
  mutate(index_prov = paste0(Country, p_num)) %>%
  mutate(index_target_prov = paste0(Country, p_num)) %>%
  filter(!is.na(init_2prov)) %>%
  select(-p_num)

country_regions_long = country_regions_long %>%
  mutate(Country = recode(Country,
                          `Timor-Leste` = "Timor Leste",
                          `Antigua & Barbuda` = "Antigua and Barbuda",
                          `United States` = "United States of America",
                          `São Tomé & Príncipe` = "Sao Tome and Principe",
                          `Côte d’Ivoire` = "Ivory Coast",
                          `Vatican City` = "Vatican",
                          `Cabo Verde` = "Cape Verde"),
         init_2prov = recode(init_2prov,
                             `St.-Petersburg` = "St. Petersburg"))

# select provincial data in external dataset and clean the strings for
# countries that we are collecting subnational data for
# commonly occurring provinces in the external data
prov = ext_data %>%
  filter(!is.na(province)) %>%
  separate_rows(province, sep = ',')%>%
  separate_rows(province, sep = ';')%>%
  mutate(
    province = str_trim(province ),
    province = gsub("^and", '', province),
    province  = str_trim(province),
    province  = case_when(
      province == 'Île-de-France' ~ 'Ile-de-France',
      province  == 'Ha N?i' ~ 'Hanoi',
      province  == 'H? Chi Minh' ~ 'Ho Chi Minh',
      province  == 'Guizhou.' ~ 'Guizhou',
      province %in% c("District Of Columbia", "D.C.", 'District of Columbia', "District of Columbia", "Washington DC") ~ "Washington, D.C.",
      province %in%c('FBiH') ~ 'Federation of B&H',
      province %in%c('São Paulo') ~ 'Sao Paulo',
      province %in% c('Lombardia') ~ 'Lombardy',
      province == 'Xinjiang Uygur' ~ 'Xinjiang',
      province == 'Xizang' ~ 'Tibet',
      province == 'Basel-Stadt' ~ "Basel-City",
      province %in% c( 'Wales,wales', 'wales') ~ "Wales",
      province %in% c( 'Scotland,scotland', 'scotland') ~ "Scotland",
      province  %in% c('England,england', "england") ~ "England",
      province == 'Corse' ~ "Corsica",
      province == 'Bretagne' ~ "Brittany",
      province == 'Rhineland-Palatinate' ~ "Rheinland-Pfalz",
      province == 'Sachsen' ~ "Saxony",
      province == "Baleric Islands"~ "Balearic Islands",
      province == "Pais Vasco"~ "Basque Country",
      province == "Lazio"~ "Latium",
      province == "Sicilia"~ "Sicily",
      province == "WA" & country == "Australia" ~ "Western Australia",
      province %in% c( "Vic", "regional Vic") & country == "Australia" ~ "Victoria",
      province == "Tas" & country == "Australia" ~ "Tasmania",
      province == "SA" & country == "Australia" ~ "South Australia",
      province == "Qld" & country == "Australia" ~ "Queensland",
      province == "Sï¿½o Paulo" ~ "Sao Paolo",
      province == "Parï¿½" ~ "Parana",
      province == "NT" & country == "Australia" ~ "Northern Territory",
      province == "NSW" & country == "Australia" ~ "New South Wales",
      province == "ACT" & country == "Australia" ~ "Australian Capital Territory",
      
      province  %in% c("Melbourne metro", 
                       "metro Melbourne",
                       "metropolitan Melbourne and Mitchell Shire"
      ) & country == "Australia" ~ "Melbourne",
      
      province == "Amur"~ "Amur Oblast",
      province == "Buryatia"~ "Buryatiya Republic",
      province == 'Comunidad de Madrid' ~ "Madrid",
      province %in% c("jammu and kashmir", "Jammu and kashmir") ~ "Jammu and Kashmir",
      province %in% c("karnataka") ~ "Karnataka",
      province %in% c("Basel Land") ~ "Basel-Landschaft",
      province %in% c("Geneve") ~ "Geneva",
      province %in% c("St. Gallen") ~ "Saint Gallen",
      province %in% c("Nei Mongol") ~ "Inner Mongolia",
      province %in% c("moscow") ~ "Moscow",
      province %in% c("Yucatï¿½n") ~ "Yucatan",
      province %in% c("Nuevo Leï¿½n") ~ "Nuevo Leon",
      province %in% c("Jalisco state") ~ "Jalisco",
      province %in% c("Ciudad de Mexico") ~ "Mexico City",
      province %in% c("Mecklenburg-Western Pomerania") ~ "Mecklenburg-Vorpommern",
      province %in% c("North Rhine Westphalia") ~ "North Rhine-Westphalia",
      province %in% c("Abuja.") ~ "Abuja",
      province %in% c("Bauchi State") ~ "Bauchi",
      province %in% c("Federal Capital Territory") ~ "FCT",
      province %in% c("KRG", "Kurdish") ~ "Kurdistan",
      province %in% c("Buenos Aires Metropolitan Area", "Buenos Aires (CABA)", 'Buenos Aires, CABA', 'CABA') ~ "Buenos Aires F.D.",
      province %in% c("National Capital Territory of Delhi (union territory)") ~ "Delhi",
      province %in% c("Santiago" )& country == 'Chile' ~ "Santiago Metropolitan",
      province %in% c("Santiago") & country == 'Cape Verde' ~ "Ribeira Grande de Santiago",
      province %in% c("Lombardi") ~ "Lombardy",
      province %in% c("Zhejiang Province") ~ "Zhejiang",
      province %in% c("Sichuan province") ~ "Sichuan",
      province %in% c("Hubei province") ~ "Hubei",      
      province %in% c("Chongquing") ~ "Chongqing",      
      grepl("Ningxia", province) ~ 'Ningxia Hui Autonomous Region',
      grepl("Yangon", province) ~ 'Rangoon',
      grepl("Tashkent", province) ~ 'Toshkent',
      grepl("Piemonte", province) ~ 'Piedmont',
      grepl("Toscana", province) ~ 'Tuscany',
      grepl("Seoul", province) ~ 'Seoul',
      grepl("Ha Noi", province) ~ 'Hanoi',
      grepl("Baden-Wurttemberg", province) ~ 'Baden-Wuerttemberg',
      grepl('Basilicata', province) ~ 'Basilicate',
      grepl('Moscow City', province) ~ 'Moscow',
      grepl("Bruxelles-Capitale", province) ~ "Brussels Capital",
      grepl("National Capital District", province) ~ "National Capital",
      grepl("Vï¿½", province) ~ "Vo",
      province == 'Distrito Federal' ~ 'Federal District',
      province == 'Sao Paolo'~ 'Sao Paulo',
      grepl("Montrï¿½al", province) ~ "Montreal",
      province == 'Ontario and Quebec' ~ 'Ontario,Quebec',
      province == 'multiple (Beijing,Chengdu,Zhejiang Ninghai)'~ 'Beijing,Chengdu,Zhejiang Ninghai',
      province == 'Tianjin and Hebei province' ~ 'Tianjin, Hebei',
      province == 'Florida and Texas' ~ 'Florida, Texas',
      
      province ==  'Lombardia and Veneto' ~ 'Lombardy,Veneto',
      province == 'Venice,Verona' ~ 'Veneto, Verona',
      province =='Andalusia,Balearic and Canary Islands' ~ 'Andalusia,Balearic Islands,Canary Islands',
      province == 'Region de Murcia' ~ 'Murcia',
      province == ''~ as.character(NA),
      TRUE ~ province
    )
  )


# match as much of the cleaned external province data to the standardized coronanet provincial data as possible
prov_match = inner_join(prov %>% 
                          select(unique_id, dataset, country, province),
                        country_regions_long %>% select(country = Country, province = init_2prov)
)


# a number of provinces in the external dataset are still unmatched;
# for these provinces, clean the strings more generically  to try to find a match 
prov_typo = prov %>% filter(province %!in% prov_match$province )%>%
  mutate(
    province = gsub("province|region|Province|Region", '', province),
    province = capwords(province),
    province  = str_trim(province)
  )

prov_typo_match = inner_join(prov_typo %>% 
                               select(unique_id, dataset, country, province),
                             country_regions_long %>% select(country = Country, province = init_2prov)
)



## We tried fuzzy matching of remaining unmatched province data
# Because it did not work very well, we did not end up implementing the following code

# prov_fuzzy = prov_typo %>% filter(province %!in% prov_typo_match$province ) %>%
#   filter(
#     !grepl("Multiple Areas|Green Zone|Low-risk|\\d|Internationally Recognized Government|Travellers", province)
#   ) 

# prov_fuzzy %>% select(province) %>% table %>% sort %>% tail(100)

# d <- expand.grid(prov_fuzzy$province %>% unique,country_regions_long$init_2prov) # Distance matrix in long form
# names(d) <- c("a_name","b_name")
# d$dist <- stringdist(d$a_name,d$b_name, method="jw") # String edit distance (use your favorite function here)
# 
# fuzzy_match = data.frame(greedyAssign(as.character(d$a_name),as.character(d$b_name),d$dist))
# 
# fuzzy_match %>% select(1:2) %>%  slice(80:90)

## create a data frame of the external data cleaned of as many errors in the province strings as possible
prov_clean = rbind(
  prov_match %>% select(unique_id, country, province),
  prov_typo_match %>% select(unique_id, country, province),
  prov %>% filter(province %!in% c( prov_typo_match$province) )%>%select(unique_id, country, province)
) %>%
  group_by(
    unique_id
  ) %>%
  mutate(province = paste(unique(province), collapse = ',')) %>%
  ungroup() %>%
  distinct


# add the cleaned strings to the main ext_data dataset
ext_data_prov = rows_update(ext_data %>% filter(!is.na(province)), prov_clean, by = 'unique_id')
ext_data = rbind(ext_data_prov, ext_data %>% filter(is.na(province)))



# make NAs consistent for type_sub_cat, links and descriptions
ext_data  =
  ext_data %>%
  mutate(
    type_sub_cat = case_when(
      
      type_sub_cat == 'NA' ~ as.character(NA),
      type_sub_cat == '' ~ as.character(NA),
      TRUE ~ type_sub_cat
    ),
    description = 
      case_when(
        description == '' ~ as.character(NA),
        TRUE ~ description
      ),
    link = 
      case_when(
        description == '' ~ as.character(NA),
        TRUE ~ link
      ),
    pdf_link = 
      case_when(
        description == '' ~ as.character(NA),
        TRUE ~ pdf_link
      )
    
  )
 
saveRDS(ext_data, file = here("data", "collaboration", "ext_data_raw.rds"))
fwrite(ext_data, file = here("data", "collaboration", "ext_data_raw.csv.gz"))
 
# ------------------------------
# B) ii) subset data of irrelevant info
#---------------------------------

# first subset : subset out tribal and county level data fro the COVIDAMP data   
# in doing this subset, we assume that
# i) the countries are matched well
# ii) the init_country_level is accurate for COVIDAMP but not necessarily for other countries
ext_data_sub = ext_data %>%
  
  # select all national level policies
  filter(
    init_country_level %in% c("National")|
      
      # select all policies that don't have an init_country_level mapped (very very policies)
      is.na(init_country_level)|
      
      # select provincial level policies for all countries
      # note that while we could subset to only countries we're interested in getting provincial level data for, this would assume that this data is accurately coded as coming from the provincial level which doesn't always seem to be the case
      (init_country_level %in% c("Provincial", "Municipal"))|
      
      # select all policies that originate from 'other' regions if the dataset is not from covidamp
      # note while we could select out these policies for all data, this would assume these policies are accurately coded as other which is perhaps not always the case
      # COVIDAMP's data quality is higher and its failry likely that they got this right
      ( init_country_level =="Other (e.g., county)" & dataset !='COVIDAMP'))

 
# filter out all econ other policies
# not we're assuming that other datasets have accurately coded policies as econ policies.
# it is possible that econ poliices in other datasets are miscoded but given that econ policies are pretty different from other types of policies, it is probably okay
ext_data_sub = ext_data_sub %>% filter(type != 'Other Policy Not Listed Above (econ)')

 
# remove data for countries that we are not collecting data for
ext_data_sub  = ext_data_sub %>% filter(ISO_A3 %in% qualtrics_2b$ISO_A3) 

ext_data_sub = ext_data_sub %>%
  filter(province %!in% c('Greenland', "United States Virgin Islands", "Guam"))
 
 
 # filter out all policies that do not have a description or link of any kind (around 150)
ext_data_sub_1 = ext_data_sub %>%  filter(!(is.na(description) & is.na(link) & is.na(pdf_link)))  

# note that the csv version of the files catches an extra observation that should be removed because
# it doesn't have a description or link of any kind that the .rds file does not
# for the sake of transparency/replicablity, keep the extra observation here 
ext_data_sub_2 = ext_data_sub %>%   filter(unique_id == 'JHU_586_nursery_school,586_primary_school,586_sec_school,586_post_school')  

ext_data_sub = rbind(ext_data_sub_1, ext_data_sub_2)%>% distinct
 
saveRDS(ext_data_sub, file = here("data", "collaboration", "ext_data_sub.rds"))
fwrite(ext_data_sub, file = here("data", "collaboration", "ext_data_sub.csv.gz"))
 

# ------------------------------
# B) iii) deduplication
#---------------------------------

### Defining objects

# ext_data_dedupe_within : object that records information as to duplicates within each individual dataset
# ext_data_dedupe_across: object that records information as to duplicates within across the external daasets
# ext_data_dedupe_coronanet: object that records information as to duplicates with respect to the external dataset and coronent variable

### Defining relevant ariables

## dedupe_id

# where are they: dedupe_id_1 (defined in ext_data_dedupe_within);  dedupe_id_2 (defined in ext_data_dedupe_across);  dedupe_id_3 (defined in ext_data_dedupe_coronanet)
# what are they: these are ids for duplicated ids and can be used to check whether a set of observations that are identified as duplicates in the code are truly duplicates when digging into the data


## keep:
# where are they: keep_1 (defined in ext_data_dedupe_within); keep_2 (defined in ext_data_dedupe_across) and keep_3 (defined in ext_data_dedupe_coronanet)
# what are they:  they are all dummy variables where 1 means that the data should be kept and 0 means the data is a duplicate and should be removed


## count_raw
# where are they: count_raw_1   (defined in ext_data_dedupe_within), count_raw _2(defined in ext_data_dedupe_across), count_raw_3 (defined in ext_data_dedupe_coronanet)
#what are they:  they are the total number of duplicates identified per grouping; a count_raw of 1 means there is no duplicate; count_raw>1 means there is a duplicate 


## count
# where are they: count_1   (defined in ext_data_dedupe_within), count_2 (defined in ext_data_dedupe_across), count_2 (defined in ext_data_dedupe_coronanet)
# what are they:  they are the sequential counts of the number of duplicates identified per grouping; a count of 1 means there is no duplicate; count>1 means there is a duplicate 




## Instructions/What to do

# 1. tweak the code/algorithm which creates the ext_data_dedupe_within; ext_data_dedupe_across and  ext_data_dedupe_coronanet objects respectively to try to maximize their ability to correctly identify duplicates.
# note if you're more comfortable using another programming language/another method to identify these duplicates that is perfectly fine. But nevertheless you should still do steps 2 and 3. below:

# 2. Evaluate how good your methodology for deduplication is by
# i) investigating any weird outliers (e.g. the code suggests that there are 55 duplicates of the same policy; its probably worth checking to see if those 55 duplicates are true duplicates or not)
# ii) sampling 100 groups of policies that are identified as being duplicates and check manually to see whether they really do seem like true duplicaets

# 3.  keep a record of the duplicates that you identify/evaulations that you make in step 2
# That is, please make sure to record a dummy variable for whether the duplicates are correctly identified or not along with the relevant unique_ids/duplicate groupings somewhere (e.g. a csv file or an excel file) ;
# and enter the information into this sheet: https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=1025573976

# the reason why we should do this is:
# it may be necessary to reiterate this process/tweak the methodology code a couple of times to deduplicate the data properly.
# If it is necessary to do this a couple of times/or even if we can't find an algorithm that does a good job, at the very least, we can remove at least some duplicates from these previous tries/iterative samplings.
# obviously if it becoes clear when you're sampling 100 policies that the algorithm you just tried doesn't work, you don't have to continue to look through every single though, but again, for the policies that you did evaulate
# to be duplicates, please try to keep a record
 

# only do the deuplication exercise for policies that have, at the very least, a description and/or a link
ext_data_sub_2 =  ext_data_sub %>%  filter(!(is.na(description)|(is.na(link) & is.na(pdf_link))))  

# note that the csv version of the files catches an extra observation that should be removed because
# it doesn't have a description or link of any kind that the .rds file does not
# for the sake of transparency/replicablity, keep the extra observation here 
ext_data_sub_2_extra <- ext_data_sub %>% filter(unique_id %in% 
c("JHU_355_screening_air", "JHU_363_screening_air,363_screening_land", 
  "JHU_564_screening_air,564_screening_land,564_screening_sea", 
  "JHU_556_screening_land,556_screening_sea", "JHU_4608_nursery_school,4608_primary_school,4608_sec_school,4608_post_school", 
  "JHU_757_screening_air", "JHU_1424_screening_air,1424_screening_land", 
  "JHU_1427_screening_land", "JHU_1428_screening_land,1428_screening_sea", 
  "JHU_1431_screening_air,1431_screening_land,1431_screening_sea", 
  "JHU_1449_screening_land", "JHU_1446_screening_air,1446_screening_land,1446_screening_sea", 
  "JHU_1448_screening_air", "JHU_819_screening_air,819_screening_land,819_screening_sea", 
  "JHU_988_border_in_air,988_border_in_land", "JHU_2102_screening_air,2102_screening_sea", 
  "JHU_5941_screening_air", "JHU_323_screening_air", "JHU_5809_screening_air", 
  "JHU_297_screening_air,297_screening_land", "JHU_742_testing_symp"
))
ext_data_sub_2 = rbind(ext_data_sub_2 , ext_data_sub_2_extra) %>% distinct

 
# not a policy
ext_data_sub_2  = ext_data_sub_2  %>%
  filter(!(ox_id %in% c('116409_tracing', '116409_testing')))
 
# ----------------
# a)  deduplicate data relative to each dataset
# note: in order to do this you *must* at a minimum group_by the `dataset` variable
# ----------------

# dedupes of covidamp are wrt to target_cities --- adjusted the code so that we keep this information even though we dedupe
# dedupes of oxcgrt are often wrt to having same description for lots of different policies; since the type already incroporates this fact (e.g. it will say type 1 OR type 2 OR type 3) these are really dupes and should be removed
# covidamp/euro: a fair number of policies that would be disaggregated into smaller subtypes; e.g. restaurants/businesses but we don't map that precisely for the type_sub_cat/its fine to keep only the aggr
# oxcgrt a fair number of policies that just have a link in the description
# oxcgrt a fair number of no change policies 

ext_data_dedupe_within = ext_data_sub_2 %>%
  filter(
    !grepl("extension|extend|Extend|Extension", description))    %>%
  mutate(description_dedupe = str_replace_all(description, "[[:punct:]]", ""),
         description_dedupe = tolower(description_dedupe),
         description_dedupe = gsub("  |   ", " ", description_dedupe),
         description_dedupe = str_trim(description_dedupe))%>%
  
  group_by(description_dedupe, 
           country, 
           province, 
           # init_other,
           # type, 
           # type_sub_cat,
           date_start,
           # date_announced,
           # date_end, 
           # init_country_level,
           # target_geog_level,
           # target_who_what,
           # target_country, 
           # target_province,
           # target_who_gen,
           # target_direction,
           # travel_mechanism, 
           # type_mass_gathering,
           # institution_status,
           # institution_cat,
           # compliance,
           # enforcer,
           link, 
           # pdf_link, 
           dataset)%>%
  mutate(
    dupe_id_1= cur_group_id(),
    count_raw_1 = n(),
    count_1 = 1:n(),
    type = paste(unique(na.omit(type)), collapse = ' or '), # allows us to keep the mapping of type for the observation that is being removed/deduplicated
    link = paste(unique(na.omit(link)), collapse = ' or '), # allows us to keep link for the observation that is being removed/deduplicated
    pdf_link = paste(unique(na.omit(pdf_link)), collapse = ' or '), # allows us to keep pdf_link for the observation that is being removed/deduplicated,
    target_other = paste(unique(na.omit(target_other)), collapse = ','),
    target_city = paste(unique(na.omit(target_city)), collapse = ','),
    travel_mechanism = paste(na.omit(travel_mechanism), collapse = ','),
    target_who_what = paste(na.omit( target_who_what), collapse = ','),
    target_who_gen = paste(na.omit( target_who_gen), collapse = ','),
    keep_1 = case_when(
      count_1 == 1 ~ 1,
      count_1 > 1 ~ 0,
      TRUE ~ 0
    )
  ) %>%
  ungroup()


ext_data_dedupe_within_full = 
  
  rbind(
    ext_data_sub_2 %>%
      
      filter(
        grepl("extension|extend|Extend|Extension", description)) %>%
      mutate(keep_1 = 1,
             count_1 = 1,
             count_raw_1 = 1,
             dupe_id_1 = paste0('d', 1:n())
      ) ,
    ext_data_dedupe_within %>% select(-description_dedupe)) 


## eda


ext_data_dedupe_within %>% select(keep_1, dataset) %>% table
ext_data_dedupe_within %>% filter(keep_1 == 0) %>% select(count_raw_1, dataset) %>% table
ext_data_dedupe_within %>% filter(keep_1 == 1) %>% select(count_1, dataset) %>% table
table(ext_data_dedupe_within$count_raw_1, ext_data_dedupe_within$dataset)

## the following is some suggested code for sampling /checking whether the duplicates are true duplicates or not;
# feel free to come up with your own method/adjust the code
# remember to record the information as to whether group of observations  are true duplicates (i.e. record the unique_ids for that grouping) here: https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=551452519
# so that at the very least, we can remove those individual policies later even if we can't find an algorithm that works more generally  

# sample observations/check whether they are true duplicates
set.seed(2)
check_ids_1 = ext_data_dedupe_within %>% 
  
  # subset to duplicated data
  filter(count_raw_1>1) %>%
  
  # sample 100 dupe ids
  select(dupe_id_1) %>%
  pull %>% 
  sample(.,100, replace = FALSE )



# check_1%>%
#   arrange(dupe_id_1) %>%
#   mutate(
#     algorithm = paste0("Group by description, country, province, date_start,link, dataset"),
#     true_dupe_dum =NA
#   )%>%
#   select(unique_id, dupe_id_1, keep =  keep_1, algorithm, true_dupe_dum) %>%
#   sheet_write(
#     ss = gs4_get(
#       "https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=0"
#     ),
#     sheet = "ext_data_dedupe_within_v2"
#   )
# 



# ----------------
# b)  deduplicate across external data
# ----------------

# the following code tries  deduplicating without grouping by the link variable (might want to try and see what happens when you do include/uncomment out the link variable)
ext_data_dedupe_across=
  ext_data_dedupe_within_full %>% 
  # ext_data_sub_2 %>%
  group_by(
    #description, 
    country, 
    province, 
    # city,
    #  init_other, # only the who has this data
    type, 
    type_sub_cat,
    date_start,
    # date_announced, only covidamp and cihi have this info
    # date_end, 
    #init_country_level,
    # target_geog_level,# only covidamp and cihi has this info
    target_who_what,
    # target_country, # only covidamp has this info
    #  target_province, only covidamp has this info
    # target_who_gen, # mostly acaps has this info but is probably overdetermined
    #  target_direction, # only jhu and who have this info and not much of it
    # travel_mechanism, # only jhu and acaps have this info; could probably extract it from the other datasets but likely too much work
    #  type_mass_gathering, # only who has this info
    #  institution_status,# only jhu has this info
    # institution_cat, # some data on this from covidamp, who and jhu
    # compliance# covidamp doesn't have this info; some data for this for acaps, jhu, ox and who
    # enforcer, # only cihi has this info
    # link
  ) %>%
  mutate(
    dupe_id_2= cur_group_id(),
    count_raw_2 = n(),
    count_2 = ifelse(length(unique(dataset))==1, 1,length(unique(dataset)) ),
    dupes = ifelse(count == 1, 0, 1),
    keep_2 = case_when(
      
      count_2 == 1 ~ 1,
      count_2 > 1 & dataset == 'CIHI' ~ 1,
      count_2 > 1 & dataset == 'COVIDAMP' & all(dataset %!in%                                     c('CIHI')) ~ 1,
      count_2 > 1 & dataset %in%c('EURO', 'CDC_ITF') & all(dataset %!in%                  c( 'COVIDAMP', 'CIHI')) ~ 1,
      count_2 > 1 & dataset == 'OXCGRT' & all(dataset %!in%             c('EURO','CDC_ITF',  'COVIDAMP', 'CIHI')) ~ 1,
      count_2 > 1 & dataset == 'JHU' & all(dataset %!in%        c('OXCGRT', 'EURO','CDC_ITF','COVIDAMP', 'CIHI')) ~ 1,
      count_2 > 1 &  dataset == 'ACAPS' & !any(dataset %in% c('JHU','OXCGRT','EURO','CDC_ITF','COVIDAMP', 'CIHI')) ~ 1,
      
      TRUE ~ 0
    )
  ) %>%
  ungroup()


dupe_order = ext_data_dedupe_across$dupe_id_2

# when there are only 2 entries to be deduped, pick the entry with the most characters
ext_data_dedupe_across_sub_1 = ext_data_dedupe_across %>%
  group_by(
    dupe_id_2
  ) %>%
  mutate(
    desc_length = nchar(description)
  )%>%
  filter(  count_2>1 & max(count_raw_2) == 2 & min(desc_length)!=max(desc_length) &any(keep_2==0) ) %>%
  mutate(
    keep_2= case_when(
      desc_length == max(desc_length)  ~1,
      desc_length == min(desc_length)  ~ 0,
      TRUE ~ keep_2
    )
  ) %>%
  ungroup

ext_data_dedupe_across_sub_2 = ext_data_dedupe_across %>%
  group_by(
    dupe_id_2
  ) %>%
  mutate(
    desc_length = nchar(description)
  )%>%
  filter( !(count_2>1 & max(count_raw_2) == 2 & min(desc_length)!=max(desc_length) &any(keep_2==0) )) %>%
  ungroup


ext_data_dedupe_across_full = rbind(ext_data_dedupe_across_sub_1, 
                                    ext_data_dedupe_across_sub_2)  %>%
  arrange(match(dupe_id_2, c(dupe_order))) 


# eda 
table(ext_data_dedupe_across$keep_2)
table(ext_data_dedupe_across$keep_2) 
table(ext_data_dedupe_across$keep_2, ext_data_dedupe_across$count_2)
ext_data_dedupe_across %>% select(keep_2, dataset) %>% table
ext_data_dedupe_across%>% filter(keep_2 == 0) %>% select(count_2, dataset) %>% table
ext_data_dedupe_across%>% select(count_2, dataset) %>% table
ext_data_dedupe_across%>% filter(count_2 ==1) %>% select(count_raw_2, dataset) %>% table
ext_data_dedupe_across%>% filter(count_2 !=1) %>% select(count_raw_2, dataset, keep_2) %>% table
ext_data_dedupe_across%>% select(count_2, count_raw_2, dataset) %>% table
ext_data_dedupe_across %>% filter(keep_2 == 1) %>% select(count_2, dataset) %>% table
table(ext_data_dedupe_across$count_raw_2)




## the following is some suggested code for sampling /checking whether the duplicates are true duplicates or not;
# feel free to come up with your own method/adjust the code
# remember to record the information as to whether group of observations  are true duplicates (i.e. record the unique_ids for that grouping) here: https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=551452519
# so that at the very least, we can remove those individual policies later even if we can't find an algorithm that works more generally  

# sample observations/check whether they are true duplicates
set.seed(2)
check_ids_2 = ext_data_dedupe_across%>% 
  group_by(dupe_id_2)%>%
  # subset to duplicated data
  filter(count_2>1 & 
           any(keep_2 == 0)
  ) %>%
  ungroup%>%
  
  # sample 100 dupe ids
  select(dupe_id_2) %>%
  pull %>% 
  sample(.,100, replace = FALSE )


check_2 = ext_data_dedupe_across%>% 
  filter(dupe_id_2 %in% check_ids_2) %>%
  arrange(dupe_id_2) %>% 
  select(dupe_id_2, unique_id, count_2, keep_1, description, dataset, type)%>%
  arrange(dupe_id_2) %>%
  left_join(ext_data_dedupe_across_full %>% select(keep_2, dupe_id_2, unique_id))


check_2_list = lapply(1:length(unique(check_2$dupe_id_2)), function(x){
  slice = check_2 %>% filter(dupe_id_2 == unique(check_2$dupe_id_2)[x] ) %>% select(unique_id, keep_1, keep_2, description) %>% data.frame 
}) 
names(check_2_list) = unique(check_2$dupe_id_2)


# check_2%>%
#   arrange(dupe_id_2) %>%
#   mutate(
#     algorithm = paste0("Group by country, province, type, date_start, type_sub_cat, target_who_what"),
#     true_dupe_dum =NA
#   )%>%
#   select(unique_id, dupe_id_2, keep =  keep_2, algorithm, true_dupe_dum) %>%
#   sheet_write(
#     ss = gs4_get(
#       "https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=0"
#     ),
#     sheet = "ext_data_dedupe_across_3"
#   )
 
# ----------------
# c)  deduplicate external data relative to coronanet
# ----------------
ext_data_dedupe_coronanet =left_join(
  ext_data_dedupe_across,
  qualtrics_2b %>%
    mutate(
      keep_3 = 0,
      description_coronanet = description
      
    ) %>%
    select(# description, 
      country, 
      province, 
      #   init_other,
      #  type, 
      # type_sub_cat,
      date_start,
      # date_announced,
      #  date_end, 
      init_country_level,
      # target_geog_level,
      # target_who_what,
      #  target_country, 
      #  target_province,
      # "target_who_gen" = type_who_gen,
      # target_direction,
      #  travel_mechanism, 
      #type_mass_gathering,
      #  compliance,
      # enforcer,
      # institution_cat,
      # institution_status,
      link, 
      #pdf_link, 
      # dataset,
      
      # don't remove keep_3, record_id or description_coronanet
      keep_3,
      record_id,
      description_coronanet
    ) )



ext_data_dedupe_coronanet= ext_data_dedupe_coronanet %>%
  mutate(keep_3 = ifelse(is.na(keep_3), 1, keep_3))


ext_data_dedupe_coronanet = ext_data_dedupe_coronanet %>% 
  group_by(record_id) %>%
  mutate(
    count_raw_3 = ifelse(is.na(record_id), 1, n()) ,
    count_3 =ifelse(is.na(record_id), 0, 1:n()) +1,
    dupe_id_3 = cur_group_id()
  ) %>% 
  ungroup()



# eda 

ext_data_dedupe_coronanet %>% select(keep_3, dataset) %>% table
ext_data_dedupe_coronanet%>% filter(keep_3 == 0) %>% select(count_3, dataset) %>% table
ext_data_dedupe_coronanet %>% filter(keep_3 == 1) %>% select(count_3, dataset) %>% table
table(ext_data_dedupe_coronanet$count_raw_3)



## the following is some suggested code for sampling /checking whether the duplicates are true duplicates or not;
# feel free to come up with your own method/adjust the code
# remember to record the information as to whether group of observations  are true duplicates (i.e. record the unique_ids for that grouping) here: https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=551452519
# so that at the very least, we can remove those individual policies later even if we can't find an algorithm that works more generally  

# sample observations/check whether they are true duplicates
set.seed(2)
check_ids_3 = ext_data_dedupe_coronanet%>% 
  
  # subset to duplicated data
  filter(count_raw_3>1) %>%
  
  # sample 100 dupe ids
  select(dupe_id_3) %>%
  pull %>% 
  sample(.,100, replace = FALSE )


check_3 = ext_data_dedupe_coronanet%>% 
  filter(dupe_id_3 %in% check_ids_3) %>%
  arrange(dupe_id_3) %>% 
  select(dupe_id_3, count_raw_3, description, description_coronanet, dataset, type)



# ----------------------------------
# sample accuracy when deduping within and across
# ---------------------------------

set.seed(2)
check_ids_4_sub_1 = ext_data_dedupe_across_full%>% 
  
  # subset to duplicated data
  filter(
    keep_1 == 0 |keep_2 == 0
  ) %>%
  
  # sample 100 dupe ids
  select(dupe_id_1) %>%
  pull %>% 
  sample(.,50, replace = FALSE )

set.seed(2)
check_ids_4_sub_2 = ext_data_dedupe_across_full%>% 
  
  # subset to duplicated data
  filter(
    keep_2 == 0
  ) %>%
  # sample 100 dupe ids
  select(dupe_id_2) %>%
  pull %>% 
  sample(.,50, replace = FALSE )


check_4 = ext_data_dedupe_across_full%>% 
  filter(dupe_id_1 %in% check_ids_4_sub_1|
           dupe_id_2 %in% check_ids_4_sub_2) 



check_4_dupe_1 = which(table(check_4$dupe_id_1)>1) %>% names
check_4_dupe_2 =  
  check_4 %>%
  group_by(dupe_id_2)%>%
  # subset to duplicated data
  filter(count_2>1 & 
           any(keep_2 == 0)
  ) %>%
  ungroup()%>%
  select(dupe_id_2) %>% distinct %>% pull  
#  
# check_4%>%
#   arrange(dupe_id_1, dupe_id_2) %>%
#   filter(dupe_id_1 %in% check_4_dupe_1 ) %>%
#   mutate(
#     algorithm = paste0("dedupe within: Group by description, country, province, date_start,link, dataset; dedupe across: Group by country, province, type, date_start, type_sub_cat, target_who_what"),
#     true_dupe_dum =NA
#   )%>%
#   select(unique_id, dupe_id_1,  dupe_id_2, keep_1, keep_2, algorithm, true_dupe_dum) %>%
#   sheet_write(
#     ss = gs4_get(
#       "https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=1518601057"
#     ),
#     sheet = "ext_data_dedupe_within_and_across_sub_1"
#   )
# 
# check_4%>%
#   arrange(dupe_id_2, dupe_id_1) %>%
#   filter(dupe_id_2 %in% check_4_dupe_2 ) %>%
#   
#   mutate(
#     algorithm = paste0("dedupe within: Group by description, country, province, date_start,link, dataset; dedupe across: Group by country, province, type, date_start, type_sub_cat, target_who_what"),
#     true_dupe_dum =NA
#   )%>%
#   select(unique_id, dupe_id_1,  dupe_id_2, keep_1, keep_2, algorithm, true_dupe_dum) %>%
#   sheet_write(
#     ss = gs4_get(
#       "https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=1518601057"
#     ),
#     sheet = "ext_data_dedupe_within_and_across_sub_2"
#   )


# --------------------------
# update data based on manual checks done on sampled policies
# -------------------------

assess_within_1 = read_sheet("https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=0",
                             sheet = "ext_data_dedupe_within")

assess_within_2 = read_sheet("https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=0",
                             sheet = "ext_data_dedupe_within_v2")

assess_across_2 = read_sheet("https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=1518601057",
                             sheet = "ext_data_dedupe_across_v2")

assess_across_3 = read_sheet("https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=0",
                             sheet = "ext_data_dedupe_across_3")


assess_4_1 = read_sheet("https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=0",
                        sheet = "ext_data_dedupe_within_and_across_sub_1")

assess_4_2 = read_sheet("https://docs.google.com/spreadsheets/d/1lwBoBR9Gb5LLlX2ZqDhBpbxHbJb8_h7p4Sr8Bumny5o/edit#gid=0",
                        sheet = "ext_data_dedupe_within_and_across_sub_2")

assess_within_1 = assess_within_1 %>%
  mutate(keep_1 = ifelse(true_dupe_dum == 0, 1, keep)) 


assess_within_2 = assess_within_2 %>%
  mutate(keep_1 = ifelse(true_dupe_dum == 0, 1, keep)) %>%
  mutate(unique_id =  gsub("NA$", '', unique_id)) 


assess_across_2 = assess_across_2 %>% 
  mutate(keep_2 = keep_by_hand)


assess_across_3 = assess_across_3 %>% 
  mutate(keep_2 = keep_manual)

assess_4_1 = assess_4_1 %>%
  mutate(keep_1 = ifelse(true_dupe_dum == 0, 1, keep_1))

assess_4_2 = assess_4_2 %>%
  mutate(keep_1 = ifelse(true_dupe_dum == 0, 1, keep_2)) %>%
  mutate(unique_id =  gsub("NA$", '', unique_id))

# extra within dedupe
ext_data_dedupe  = rows_update(ext_data_dedupe_across_full , assess_within_1 %>%
                                 select(keep_1, unique_id) %>%
                                 filter(unique_id %in%   intersect(assess_within_1$unique_id , ext_data_dedupe_across_full$unique_id)),
                               by = c('unique_id'))
# main within dedupe
ext_data_dedupe = rows_update(ext_data_dedupe ,
                              assess_within_2 %>% select(keep_1, unique_id) %>%
                                filter(unique_id %in%   intersect(assess_within_2$unique_id , ext_data_dedupe_across_full$unique_id))
                              
                              , by = c('unique_id'))


# extra across dedupe
ext_data_dedupe = rows_update(ext_data_dedupe ,
                              assess_across_2 %>% 
                                filter(unique_id %in%   intersect(assess_across_2$unique_id , ext_data_dedupe_across_full$unique_id)) %>% 
                                select(keep_2, unique_id), by = c('unique_id'))
# main across dedupe
ext_data_dedupe = rows_update(ext_data_dedupe ,
                              assess_across_3 %>% 
                                select(keep_2, unique_id), by = c('unique_id'))
# main within/across dedupe sub 1
ext_data_dedupe = rows_update(ext_data_dedupe ,
                              assess_4_1%>% 
                                select(keep_1, unique_id), by = c('unique_id'))
# main within/across dedupe sub 1
ext_data_dedupe = rows_update(ext_data_dedupe ,
                              assess_4_2%>% 
                                select(keep_2, unique_id), by = c('unique_id'))



ext_data_sub_missing = ext_data_sub %>%  filter((is.na(description)|(is.na(link) & is.na(pdf_link))))  %>%
  mutate(
    dupe_id_1 = NA,
    dupe_id_2 = NA,
    count_1 = NA,
    count_2= NA,
    
    count_raw_1= NA,
    count_raw_2= NA,
    
    keep_1= NA,
    keep_2= NA
    
  )

# only need to do this if loading .csvs of the taxonomy maps; for whatever reason the rds version catches these policies but the .csv version doesn't
ext_data_sub_missing  = ext_data_sub_missing %>%
  filter(unique_id %!in% c("JHU_355_screening_air", "JHU_363_screening_air,363_screening_land", 
                           "JHU_564_screening_air,564_screening_land,564_screening_sea", 
                           "JHU_556_screening_land,556_screening_sea", "JHU_4608_nursery_school,4608_primary_school,4608_sec_school,4608_post_school", 
                           "JHU_757_screening_air", "JHU_1424_screening_air,1424_screening_land", 
                           "JHU_1427_screening_land", "JHU_1428_screening_land,1428_screening_sea", 
                           "JHU_1431_screening_air,1431_screening_land,1431_screening_sea", 
                           "JHU_1449_screening_land", "JHU_1446_screening_air,1446_screening_land,1446_screening_sea", 
                           "JHU_1448_screening_air", "JHU_819_screening_air,819_screening_land,819_screening_sea", 
                           "JHU_988_border_in_air,988_border_in_land", "JHU_2102_screening_air,2102_screening_sea", 
                           "JHU_5941_screening_air", "JHU_323_screening_air", "JHU_5809_screening_air", 
                           "JHU_297_screening_air,297_screening_land", "JHU_742_testing_symp"
  )) %>% distinct
 
ext_data_dedupe = rbind(ext_data_dedupe %>% 
                          filter(keep_1 == 1 &  keep_2 == 1|
                                   unique_id %in% 
                                   c("OXCGRT_Lebanon_20200818_intmvt", 
                                     "OXCGRT_Lebanon_20200818_stay", "OXCGRT_Lebanon_20200818_extmvt", 
                                     "OXCGRT_Lebanon_20200818_workplace", "OXCGRT_Lebanon_20200818_gatherings", 
                                     "OXCGRT_Lebanon_20200818_events", "OXCGRT_Lebanon_20200818_transport", 
                                     "OXCGRT_Luxembourg_20200420_elderly")
                                 
                          ) %>%
                          select(-dupes, -desc_length),
                        
                        ext_data_sub_missing
)


 
# remove senegal from who PHSM dataset as these are already harmonised
ext_data_dedupe = ext_data_dedupe %>%
  filter(
    !(country == 'Senegal' &
        dataset %in% c("CDC_ITF", 'EURO') &
        recorded_date < as.Date("2021-04-29", "%Y-%m-%d")))




# non policieis

# these polices were identfied from sampling within; there are 10 dupe_ids in total here (out of 100 sampled), suggesting that 10% of what has been removed are observations that are not policies

non_policies = c(  c("OXCGRT_Burkina Faso_20210808_stay", 
                     "OXCGRT_Burkina Faso_20210808_workplace", "OXCGRT_Burkina Faso_20210808_gatherings", 
                     "OXCGRT_Burkina Faso_20210808_events", "OXCGRT_Burkina Faso_20210808_transport", 
                     "OXCGRT_Gambia_20210622_intmvt", "OXCGRT_Gambia_20210622_stay", 
                     "OXCGRT_Gambia_20210622_transport", "OXCGRT_Iraq_20210608_tracing", 
                     "OXCGRT_Iraq_20210608_extmvt", "OXCGRT_Iraq_20210608_intmvt", 
                     "OXCGRT_Iraq_20210608_stay", "OXCGRT_Iraq_20210608_events", "OXCGRT_Iraq_20210608_elderly", 
                     "OXCGRT_Iraq_20210608_mask", "OXCGRT_Iraq_20210608_transport", 
                     "OXCGRT_Cote d'Ivoire_20210620_tracing", "OXCGRT_Cote d'Ivoire_20210620_elderly", 
                     "OXCGRT_Cote d'Ivoire_20210620_mask", "OXCGRT_Papua New Guinea_20210705_elderly", 
                     "OXCGRT_Papua New Guinea_20210705_mask", "OXCGRT_Russia_20210320_school", 
                     "OXCGRT_Russia_20210320_tracing", "OXCGRT_Russia_20210320_testing", 
                     "OXCGRT_Russia_20210320_intmvt", "OXCGRT_Russia_20210320_stay", 
                     "OXCGRT_Russia_20210320_info", "OXCGRT_Russia_20210320_workplace", 
                     "OXCGRT_Russia_20210320_gatherings", "OXCGRT_Russia_20210320_events", 
                     "OXCGRT_Russia_20210320_mask", "OXCGRT_Russia_20210320_transport", 
                     "OXCGRT_Somalia_20210118_extmvt", "OXCGRT_Somalia_20210118_stay", 
                     "OXCGRT_Somalia_20210118_workplace", "OXCGRT_Somalia_20210118_gatherings", 
                     "OXCGRT_Somalia_20210118_events", "OXCGRT_Somalia_20210118_transport", 
                     "OXCGRT_South Korea_20210315_school", "OXCGRT_South Korea_20210315_intmvt", 
                     "OXCGRT_South Korea_20210315_stay", "OXCGRT_South Korea_20210315_workplace", 
                     "OXCGRT_South Korea_20210315_gatherings", "OXCGRT_South Korea_20210315_events", 
                     "OXCGRT_South Korea_20210315_transport", "OXCGRT_Tanzania_20210216_intmvt", 
                     "OXCGRT_Tanzania_20210216_stay", "OXCGRT_Tanzania_20210216_gatherings", 
                     "OXCGRT_Tanzania_20210216_events", "OXCGRT_Tanzania_20210216_transport"
), 

# these polices were identfied from sampling within; there are 13 dupe_ids in total here (out of 100 sampled), suggesting that 13% of what has been removed are observations that are not policies
c("OXCGRT_United States_Arizona_20210809_stay", 
  "OXCGRT_United States_Arizona_20210809_workplace", "OXCGRT_United States_Arizona_20210809_gatherings", 
  "OXCGRT_United States_Arizona_20210809_transport", "OXCGRT_China_20210628_school", 
  "OXCGRT_China_20210628_intmvt", "OXCGRT_China_20210628_stay", 
  "OXCGRT_China_20210628_workplace", "OXCGRT_China_20210628_gatherings", 
  "OXCGRT_China_20210628_events", "OXCGRT_China_20210628_transport", 
  "OXCGRT_Dominican Republic_20210511_tracing", "OXCGRT_Dominican Republic_20210511_testing", 
  "OXCGRT_Dominican Republic_20210511_intmvt", "OXCGRT_Dominican Republic_20210511_stay", 
  "OXCGRT_Dominican Republic_20210511_extmvt", "OXCGRT_Dominican Republic_20210511_workplace", 
  "OXCGRT_Dominican Republic_20210511_gatherings", "OXCGRT_Dominican Republic_20210511_events", 
  "OXCGRT_Dominican Republic_20210511_elderly", "OXCGRT_Dominican Republic_20210511_mask", 
  "OXCGRT_Dominican Republic_20210511_transport", "OXCGRT_Germany_20201022_school", 
  "OXCGRT_Germany_20201022_tracing", "OXCGRT_Germany_20201022_testing", 
  "OXCGRT_Germany_20201022_info", "OXCGRT_Germany_20201022_workplace", 
  "OXCGRT_Germany_20201022_gatherings", "OXCGRT_Germany_20201022_events", 
  "OXCGRT_Germany_20201022_mask", "OXCGRT_Germany_20201022_transport", 
  "OXCGRT_Mozambique_20210517_tracing", "OXCGRT_Mozambique_20210517_vax", 
  "OXCGRT_Mozambique_20210517_extmvt", "OXCGRT_Mozambique_20210517_workplace", 
  "OXCGRT_Mozambique_20210517_gatherings", "OXCGRT_Mozambique_20210517_events", 
  "OXCGRT_Mozambique_20210517_elderly", "OXCGRT_Mozambique_20210517_mask", 
  "OXCGRT_Mozambique_20210517_transport", "OXCGRT_Nicaragua_20210518_school", 
  "OXCGRT_Nicaragua_20210518_intmvt", "OXCGRT_Nicaragua_20210518_stay", 
  "OXCGRT_Nicaragua_20210518_workplace", "OXCGRT_Nicaragua_20210518_gatherings", 
  "OXCGRT_Nicaragua_20210518_events", "OXCGRT_Nicaragua_20210518_transport", 
  "OXCGRT_Nicaragua_20210323_workplace", "OXCGRT_Nicaragua_20210323_gatherings", 
  "OXCGRT_Nicaragua_20210323_events", "OXCGRT_Papua New Guinea_20201221_intmvt", 
  "OXCGRT_Papua New Guinea_20201221_stay", "OXCGRT_Papua New Guinea_20201221_transport", 
  "OXCGRT_Serbia_20201016_school", "OXCGRT_Serbia_20201016_tracing", 
  "OXCGRT_Serbia_20201016_extmvt", "OXCGRT_Serbia_20201016_testing", 
  "OXCGRT_Serbia_20201016_intmvt", "OXCGRT_Serbia_20201016_stay", 
  "OXCGRT_Serbia_20201016_info", "OXCGRT_Serbia_20201016_workplace", 
  "OXCGRT_Serbia_20201016_gatherings", "OXCGRT_Serbia_20201016_events", 
  "OXCGRT_Serbia_20201016_mask", "OXCGRT_Serbia_20201016_transport", 
  "OXCGRT_Serbia_20201022_school", "OXCGRT_Serbia_20201022_tracing", 
  "OXCGRT_Serbia_20201022_extmvt", "OXCGRT_Serbia_20201022_testing", 
  "OXCGRT_Serbia_20201022_intmvt", "OXCGRT_Serbia_20201022_stay", 
  "OXCGRT_Serbia_20201022_info", "OXCGRT_Serbia_20201022_workplace", 
  "OXCGRT_Serbia_20201022_gatherings", "OXCGRT_Serbia_20201022_events", 
  "OXCGRT_Serbia_20201022_mask", "OXCGRT_Serbia_20201022_transport", 
  "OXCGRT_Switzerland_20210410_gatherings", "OXCGRT_Switzerland_20210410_events", 
  "OXCGRT_Syria_20210112_school", "OXCGRT_Syria_20210112_tracing", 
  "OXCGRT_Syria_20210112_testing", "OXCGRT_Syria_20210112_intmvt", 
  "OXCGRT_Syria_20210112_stay", "OXCGRT_Syria_20210112_workplace", 
  "OXCGRT_Syria_20210112_gatherings", "OXCGRT_Syria_20210112_events", 
  "OXCGRT_Syria_20210112_elderly", "OXCGRT_Syria_20210112_transport", 
  "OXCGRT_Tanzania_20210207_gatherings", "OXCGRT_Tanzania_20210207_events"
),
c("OXCGRT_United States_Arizona_20210823_stay", "OXCGRT_United States_Arizona_20210823_workplace", 
  "OXCGRT_United States_Arizona_20210823_gatherings", "OXCGRT_United States_Arizona_20210823_events", 
  "OXCGRT_United States_Arizona_20210823_transport", "OXCGRT_Burkina Faso_20210808_stay", 
  "OXCGRT_Burkina Faso_20210808_workplace", "OXCGRT_Burkina Faso_20210808_gatherings", 
  "OXCGRT_Burkina Faso_20210808_events", "OXCGRT_Burkina Faso_20210808_transport", 
  "OXCGRT_Nicaragua_20210323_stay", "OXCGRT_Nicaragua_20210323_workplace", 
  "OXCGRT_Nicaragua_20210323_gatherings", "OXCGRT_Nicaragua_20210323_events", 
  "OXCGRT_Russia_20210227_school", "OXCGRT_Russia_20210227_tracing", 
  "OXCGRT_Russia_20210706_tracing", "OXCGRT_Russia_20210227_extmvt", 
  "OXCGRT_Russia_20210227_testing", "OXCGRT_Russia_20210227_intmvt", 
  "OXCGRT_Russia_20210706_intmvt", "OXCGRT_Russia_20210227_stay", 
  "OXCGRT_Russia_20210706_stay", "OXCGRT_Russia_20210227_info", 
  "OXCGRT_Russia_20210706_info", "OXCGRT_Russia_20210227_workplace", 
  "OXCGRT_Russia_20210227_gatherings", "OXCGRT_Russia_20210227_events", 
  "OXCGRT_Russia_20210706_events", "OXCGRT_Russia_20210227_elderly", 
  "OXCGRT_Russia_20210227_mask", "OXCGRT_Russia_20210227_transport", 
  "OXCGRT_Russia_20210706_elderly", "OXCGRT_Russia_20210706_mask", 
  "OXCGRT_Russia_20210706_transport", "OXCGRT_Somalia_20210125_school", 
  "OXCGRT_Somalia_20210125_tracing", "OXCGRT_Somalia_20210125_vax", 
  "OXCGRT_Somalia_20210125_extmvt", "OXCGRT_Somalia_20210125_testing", 
  "OXCGRT_Somalia_20210125_intmvt", "OXCGRT_Somalia_20210125_stay", 
  "OXCGRT_Somalia_20210125_info", "OXCGRT_Somalia_20210125_workplace", 
  "OXCGRT_Somalia_20210125_gatherings", "OXCGRT_Somalia_20210125_events", 
  "OXCGRT_Somalia_20210125_elderly", "OXCGRT_Somalia_20210125_mask", 
  "OXCGRT_Somalia_20210125_transport"),

c("OXCGRT_Dominican Republic_20210516_intmvt", 
  "OXCGRT_Dominican Republic_20210516_transport", "OXCGRT_Russia_20210227_school", 
  "OXCGRT_Russia_20210227_tracing", "OXCGRT_Russia_20210227_extmvt", 
  "OXCGRT_Russia_20210227_testing", "OXCGRT_Russia_20210227_intmvt", 
  "OXCGRT_Russia_20210227_stay", "OXCGRT_Russia_20210227_info", 
  "OXCGRT_Russia_20210227_workplace", "OXCGRT_Russia_20210227_gatherings", 
  "OXCGRT_Russia_20210227_events", "OXCGRT_Russia_20210227_elderly", 
  "OXCGRT_Russia_20210227_mask", "OXCGRT_Russia_20210227_transport", 
  "OXCGRT_Somalia_20210125_school", "OXCGRT_Somalia_20210125_tracing", 
  "OXCGRT_Somalia_20210125_vax", "OXCGRT_Somalia_20210125_extmvt", 
  "OXCGRT_Somalia_20210125_testing", "OXCGRT_Somalia_20210125_intmvt", 
  "OXCGRT_Somalia_20210125_stay", "OXCGRT_Somalia_20210125_info", 
  "OXCGRT_Somalia_20210125_workplace", "OXCGRT_Somalia_20210125_gatherings", 
  "OXCGRT_Somalia_20210125_events", "OXCGRT_Somalia_20210125_elderly", 
  "OXCGRT_Somalia_20210125_mask", "OXCGRT_Somalia_20210125_transport", 
  "OXCGRT_Burkina Faso_20210808_stay", "OXCGRT_Burkina Faso_20210808_workplace", 
  "OXCGRT_Burkina Faso_20210808_gatherings", "OXCGRT_Burkina Faso_20210808_events", 
  "OXCGRT_Burkina Faso_20210808_transport", "OXCGRT_Nicaragua_20210323_stay", 
  "OXCGRT_Nicaragua_20210323_workplace", "OXCGRT_Nicaragua_20210323_gatherings", 
  "OXCGRT_Nicaragua_20210323_events"),

c("OXCGRT_United States_Iowa_20210419_stay", "OXCGRT_United States_Iowa_20210419_gatherings", 
  "OXCGRT_United States_Iowa_20210419_events", "OXCGRT_United States_Iowa_20210419_transport", 
  "OXCGRT_United States_Nebraska_20210726_workplace", "OXCGRT_United States_Nebraska_20210726_gatherings", 
  "OXCGRT_United States_Nebraska_20210726_transport", "OXCGRT_Bolivia_20210314_stay", 
  "OXCGRT_Bolivia_20210314_workplace", "OXCGRT_Bolivia_20210314_gatherings", 
  "OXCGRT_Bolivia_20210314_events", "OXCGRT_Bolivia_20210314_transport", 
  "OXCGRT_Gambia_20210518_intmvt", "OXCGRT_Gambia_20210518_stay", 
  "OXCGRT_Gambia_20210518_extmvt", "OXCGRT_Gambia_20210518_gatherings", 
  "OXCGRT_Gambia_20210518_events", "OXCGRT_Gambia_20210518_mask", 
  "OXCGRT_Gambia_20210518_transport", "OXCGRT_Mali_20210628_tracing", 
  "OXCGRT_Mali_20210628_vax", "OXCGRT_Mali_20210628_testing", "OXCGRT_Mali_20210628_elderly", 
  "OXCGRT_Mali_20210628_mask", "OXCGRT_Rwanda_20210410_school", 
  "OXCGRT_Rwanda_20210410_tracing", "OXCGRT_Rwanda_20210410_testing", 
  "OXCGRT_Rwanda_20210410_intmvt", "OXCGRT_Rwanda_20210410_stay", 
  "OXCGRT_Rwanda_20210410_info", "OXCGRT_Rwanda_20210410_extmvt", 
  "OXCGRT_Rwanda_20210410_workplace", "OXCGRT_Rwanda_20210410_gatherings", 
  "OXCGRT_Rwanda_20210410_events", "OXCGRT_Rwanda_20210410_elderly", 
  "OXCGRT_Rwanda_20210410_mask", "OXCGRT_Rwanda_20210410_transport", 
  "OXCGRT_Tanzania_20210524_gatherings", "OXCGRT_Tanzania_20210524_events"),

c("OXCGRT_China_Qinghai_20200429_testing", 
  "OXCGRT_China_Shanxi_20210712_stay", "OXCGRT_Central African Republic_20210420_extmvt", 
  "OXCGRT_Chad_20210426_school", "OXCGRT_Chad_20210426_tracing", 
  "OXCGRT_Chad_20210426_vax", "OXCGRT_Chad_20210426_testing", "OXCGRT_Chad_20210426_stay", 
  "OXCGRT_Chad_20210426_info", "OXCGRT_Chad_20210426_workplace", 
  "OXCGRT_Chad_20210426_gatherings", "OXCGRT_Chad_20210426_events", 
  "OXCGRT_Chad_20210426_elderly", "OXCGRT_Chad_20210426_mask", 
  "OXCGRT_Chad_20210426_transport", "OXCGRT_Latvia_20210803_testing", 
  "OXCGRT_Timor-Leste_20210807_vax"))



# remove observations that are not policies; note that a lot of these were already removed in the keep_1/keep_2 code
ext_data_dedupe= ext_data_dedupe %>%
  filter(!(unique_id %in% non_policies))

# who phsm made a mistake in how they make the 'database var'; fix it here
ext_data_dedupe = ext_data_dedupe  %>%
  mutate(
    dataset = 
      case_when(dataset == 'ACAPS' & grepl("CDC", unique_id) ~ 'CDC_ITF',
                dataset == 'OXCGRT' & grepl("CDC", unique_id) ~ 'CDC_ITF', 
                TRUE~dataset
      )
  )

 
## separate rows for observations in subnational countries that have multiple valid provinces rolled within the province variable  
subnat <-c('France', "Germany", 'Spain', 'Italy')
subnat_other <- c('China', 'Brazil', 'Switzerland', 'Japan', 'Nigeria', 'India', 'Australia', 'Canada', 'United States of America', 'Russia')

 
ext_data_dedupe_prov_split <- ext_data_dedupe %>%
  filter(country %in% c(subnat, subnat_other)) %>%
  filter(grepl(paste(country_list$prov, collapse = '|'), province)) %>%
  mutate(province = ifelse(province == 'Washington, D.C.', 'Washington; D.C.', province)) %>%
  separate_rows(province, sep = ',') %>%
  mutate(province = ifelse(province == 'Washington; D.C.', 'Washington, D.C.', province)) %>%
  rowwise %>%
  mutate(
    sheet_id =
           case_when(!is.na(province) & province %in% country_list$prov[country_list$Country == country] ~paste0(unique_id, '_', province),
                     !is.na(province) & province %!in% country_list$prov[country_list$Country == country] ~paste0(unique_id, '_', "Other subnational governments"))
         ) %>%
  ungroup



ext_data_dedupe  = rbind(ext_data_dedupe_prov_split,
                         ext_data_dedupe %>%
               filter(country %!in% c(subnat, subnat_other)|
                      c(country %in% c(subnat, subnat_other) & !grepl(paste(country_list$prov, collapse = '|'), province)))%>%
               mutate(sheet_id =
                        case_when(
                        country %!in% c(subnat, subnat_other) ~  paste0(unique_id, '_', country),
                        country %in% c(subnat, subnat_other) & is.na(province) ~ paste0(unique_id, '_', 'National'),
                        country %in% c(subnat, subnat_other)  & !is.na(province) & province %!in% country_list$prov ~paste0(unique_id, '_', "Other subnational governments"))

                  ))

# roll back up policies that have non-standardized province names
ext_data_dedupe <-ext_data_dedupe  %>%
  group_by(sheet_id, country) %>%
  mutate(province = paste(province, collapse = ',')) %>%
  ungroup() %>%
  distinct %>%
  group_by(unique_id) %>%
  mutate(policy_count = n(),
         other_dum = grepl("Other subnational governments", sheet_id)) %>%
  ungroup %>%
  filter(!(policy_count>1 & other_dum == TRUE)) %>%
  select(-policy_count, -other_dum)

dim(ext_data_dedupe) 
 
saveRDS(ext_data_dedupe, file = here("data", "collaboration", "ext_data_dedupe.rds"))
fwrite(ext_data_dedupe, file = here("data", "collaboration", "ext_data_dedupe.csv.gz"))


# ---------------------------
# C) data quality comparisons
#-----------------------------

## assess missing links

missing_links = ext_data_dedupe %>%
  group_by(dataset) %>%
  summarise(
    missing_links = length(which(is.na(link) & is.na(pdf_link)))
  )

ext_data %>% filter(
  !grepl("http", link) & !grepl("http", pdf_link)) %>% 
  select(dataset) %>% 
  table


## compare description quality
ext_data_dedupe %>%
  mutate(
    no_desc = ifelse(is.na(description), 1, 0),
    text_length = nchar(description),
    less_than_50 = ifelse(nchar(description)<50, 1, 0)
  ) %>% 
  group_by(dataset) %>%
  summarise(text_lenth = mean(text_length, na.rm = T),
            less_than_50 = sum(less_than_50, na.rm = TRUE),
            no_desc = sum(no_desc)
  )  

qualtrics_2b %>%
  filter(!is.na(description)) %>% # note that coronanet does not release data publicly when a description field is empty
  mutate(
    no_desc = ifelse(is.na(description), 1, 0),
    less_than_50 = ifelse(nchar(description)<50, 1, 0),
    text_length = nchar(description)
  ) %>% 
  summarise(text_lenth = mean(text_length, na.rm = T),
            no_desc = sum(no_desc),
            less_than_50 = sum(less_than_50)
  )


# example average descriptions
dset = c("ACAPS", "CDC_ITF", 'CIHI', 'COVIDAMP', 'EURO', 'JHU', 'OXCGRT')
avg_length = c(172, 537, 254, 227, 297, 230, 329)

for ( d in 1:7){
  print(dset[d])
  ext_data_dedupe %>%
    filter(
      dataset == dset[d] &
        nchar(description) == avg_length[d]
    )%>% select(description) %>% head %>% data.frame %>% print
  
}


# example descriptions less than 50

for ( d in 1:7){
  print(dset[d])
  ext_data_dedupe %>%
    filter(
      dataset == dset[d] &
        nchar(description) <50
    )%>% select(description) %>% head %>% data.frame %>% print
  
}

## assess missing end dates
# external data
ext_data_dedupe = ext_data_dedupe %>%
  mutate(
    date_end_clean = 
      case_when(
        date_end ==  "05-12-2020" ~ "2020-05-12",
        date_end == '2020-09' ~ "2020-09-01",
        date_end == '2020-08' ~ "2020-08-01",
        date_end == '2023' ~ '2023-01-01',
        date_end == '2021-03'~ "2021-03-01",
        date_end == '2020-06' ~ '2020-06-01',
        date_end == '2023-01' ~ '2023-01-01',
        date_end == '03-30-2020' ~ "2020-03-30",
        TRUE~ date_end
      ),
    date_end_clean = str_trim(gsub("Actual end date:", "", str_extract(date_end_clean, "[^;]+"))),
    date_end_clean = as.Date(date_end_clean, "%Y-%m-%d")
  )

ext_data_dedupe %>%
  mutate(
    no_end_date = ifelse(is.na(date_end), 1, 0)
  ) %>% 
  group_by(dataset) %>%
  summarise(no_end_date = sum(no_end_date, na.rm = T),
            avg_start_date= mean(date_start, na.rm = TRUE),
            avg_end_date = mean(date_end_clean, na.rm = TRUE),
            last_recorded_date = max(recorded_date, na.rm = TRUE)
  )  

# coronanet data: no date_end_spec (note pulls data from September 2021;
# in the paper, we use data from February 2023)
qualtrics_2b %>%
  mutate(
    no_end_date = ifelse(is.na(date_end_spec), 1, 0)
  ) %>% 
  summarise(no_end_date = sum(no_end_date, na.rm = T),
            avg_start_date= mean(date_start, na.rm = TRUE),
            avg_end_date = mean(date_end, na.rm = TRUE),
            last_recorded_date = max(recorded_date, na.rm = TRUE)
  )

