# This code transforms the raw survey qualtrics data
# for the CoronaNet Project into a long format
# the output for this code is currently saved under 'coranaNetData_clean.rds' in the Data/CoronaNet folder of the Dropbox

## the code should transform the data such that:
# for the initiating policy actor, there is:
# one column for the country: init_country
# one column for the initiating province, where applicable: init_province
# one column for the initiating city, where applicable: int_city
# one column for the initiating other body, where applicable: init_other_level
# for the geographical target of the policy actors, there is:
# one column for the targeted country/regional grouping  (see note below in line 336 about breaking out the countries in regional groupings): target_country_region
# one column for the targeted province, where applicable: target_province
# one column for the targeted city, where applicable: target_city
# for the type of policy, there is:
# one column for the broad type of policy: type
# one column for the sub-category of a policy: type_sub_cat
# one column for the 'other' text entries of a sub-category of a policy: type_sub_cat_other
# one column for the number of a policy (e.g. how many masks) where applicable: type_sub_num
# one column for other broad policy types: type_other
# there is one column for the who/what target of a policy: target_who_what
# there is one column for the travel mechanism of a policy: travel_mechanism
# there is one column for the compliance of a policy: compliance
# there is one column for the enforcer of a policy: enforcer
# for the timing of a policy there is
# one column for date announced: date_announced
# one column for date policy implemented: date_start
# one column for date when policy ends, if found: date_end

# note that the 'other columns' where they exist, correspond to the above, with a TEXT at the end, unless otherwise specified above
# Data Recoding File
# Cindy Cheng with edits by Bob Kubinec
# April 4th, 2020

# To-do List:

# 1) Finish resolving/checking for when the 'other'/text_entry category is selected for all applicable variables
# I tried to mark where this still needs to be done with the following comments: "!!! NOTE "
# Note, for now, I've left the 'other' text entry vars as their own columns, we may want to combine some of these 'other' text entries with the main variable for some of these variables at some point
# 2) check 'notes' section and address issues there accordingly
# 3) ?organize sources (rename?)
# 4) ?relabel some of the variable names for text entry vars to make them more intuitive
# 5) at the very end, download latest version of dataset and make sure everything still works

# setup -----------------------------

path = "/cloud/project"
pathData <- 'data/'


## load packages and functions

library(tidyr)
library(magrittr)
library(dplyr)
library(readr)
library(qualtRics)
library(stringr)


capwords <- function(s, strict = FALSE) {
  cap <- function(s)
    paste(toupper(substring(s, 1, 1)),
          {
            s <- substring(s, 2)
            if (strict)
              tolower(s)
            else
              s
          },
          sep = "", collapse = " ")
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


## load data

# note we can't connect to qualtrics via API
# because the TUM has not purchased one
# .csv downloads it is!

country_regions = read_csv(file = paste0(pathData, '/regions/country_region_clean.csv'))
qualtrics = read_survey(paste0(pathData, '/CoronaNet/coronanet_raw_latest.csv'))

 
# This script filters out bad records (need to remove/fix these manually so we don't do this)

source("RCode/validation/filter_bad_records.R")

# This script manually recodes values
# Also should be fixed so we don't do this

source("RCode/validation/recode_records.R")

# text entry cleaning ----------------------------------

# remove all diacritics from the text entries for init_city, target_city, target_other

# https://github.com/tidyverse/stringr/issues/149 do as hadley does...
## !!! NOTE probably should do this for all text entries
qualtrics$init_city = stringi::stri_trans_general(qualtrics$init_city, "Latin-ASCII")
qualtrics$target_city  = stringi::stri_trans_general(qualtrics$target_city , "Latin-ASCII")
qualtrics$target_other  = stringi::stri_trans_general(qualtrics$target_other , "Latin-ASCII")
qualtrics$target_city[which(qualtrics$target_city == 'bogota')] = "Bogota"

# replace entries with documented corrected entries as necessary ----------------------------------

# make vector of ids that need to be corrected
correction_record_ids = qualtrics[which(qualtrics$entry_type == 'Correction to Existing Entry (type in Record ID in text box)'), 'entry_type_2_TEXT']

# note that there are some un-matched records; check them out later
matched_corrections = qualtrics$record_id[which(qualtrics$record_id %in% correction_record_ids$entry_type_2_TEXT)]
unmatched_corrections = setdiff(correction_record_ids$entry_type_2_TEXT, matched_corrections)

# make a variable called correct_record_match: if entry is corrected, fill in the corresponding record id entered in entry_type_2_TEXT,
#  if an entry was not corrected, fill in with original record id
qualtrics$correct_record_match = ifelse(
  qualtrics$entry_type == 'Correction to Existing Entry (type in Record ID in text box)',
  qualtrics$entry_type_2_TEXT,
  qualtrics$record_id
)

# check for nas; there shouldn't be any
if (sum(is.na(qualtrics$correct_record_match)) > 0) {
  warning("Code cleaning failed. Missing records in the correct_record_match column.")
  
  print(paste0("These record IDs are corrections without original record referenced: ",
               qualtrics$record_id[is.na(qualtrics$correct_record_match)]))
  
  # remove these records
  
  qualtrics <- filter(qualtrics,!is.na(correct_record_match))
  
}

# remove old entries
qualtrics = qualtrics %>% filter(!record_id %in% correction_record_ids)

# replace old entries with corrected entries
qualtrics$record_id = qualtrics$correct_record_match

# link updated policy(ies) with original entry with variable 'policy_id' ----------------------------------

updated_record_ids = qualtrics[which(qualtrics$entry_type == 'Update on Existing Entry (type in Record ID in text box)'), 'entry_type_3_TEXT']

matched_updates = qualtrics$record_id[which(qualtrics$record_id %in% updated_record_ids$entry_type_3_TEXT)]
(unmatched_updates = setdiff(updated_record_ids$entry_type_3_TEXT, matched_updates)) # need to take a closer look later

# make a variable called correct_record_match: if entry is updated, fill in the corresponding original record id entered in entry_type_3_TEXT,
#  if an entry was not updated, fill in with original record id

qualtrics$policy_id = ifelse(
  qualtrics$entry_type == 'Update on Existing Entry (type in Record ID in text box)',
  qualtrics$entry_type_3_TEXT,
  qualtrics$record_id
)

# after here there are errors

# rename variables ----------------------------------

 
# rename all type questions that ask extra detail about the 'number' of a policy using the same variable name format
names(qualtrics)[which(names(qualtrics) %in% c("type_quarantine_days", "type_mass_gathering"))] = c('type_num_quarantine_days', 'type_num_mass_gathering')
names(qualtrics)[grep(
  'type_health_resource_\\d_TEXT|type_health_resource_\\d\\d_TEXT',
  names(qualtrics)
)] = c(
  'type_num_masks',
  'type_num_ventilators',
  'type_num_ppe',
  'type_num_hand_sanit',
  'type_num_test_kits',
  'type_other_health_materials',
  'type_num_hospitals',
  'type_num_quaranCen',
  'type_num_medCen',
  'type_num_pubTest',
  'type_num_research',
  'type_other_health_infra',
  'type_num_doctors',
  'type_num_nurses',
  'type_num_volunt',
  'type_other_health_staff'
)

# rename/link names for provinces back to the country
names(qualtrics)[grep('init_province', names(qualtrics))]  = paste0("init_prov_", gsub(" |\\-", '', c(
  'European Union', country_regions$Country
)))
 
# remove empty columns ----------------------------------
# find empty columns, only delete empty columns of provinces or sources
empty_columns = names(which(apply(qualtrics, 2, function(x) {
  all(is.na(x))
})))
(empty_columns  = empty_columns[grep('prov|source', empty_columns)])

qualtrics = dplyr::select(qualtrics,-empty_columns)


#  combining columns ----------------------------------

###------  combine columns for init_provinces ----- ###
qualtrics = qualtrics %>%
  # first change all of the NA's in the init_prov column to ""
  mutate_at(vars(starts_with("init_prov")),
            list( ~ replace(., is.na(.), ""))) %>%
  
  # then combine all the init_prov columns together into one column called 'prov'
  unite(index_prov, contains('init_prov'), sep = '') %>%
  
  # remove init_prov_[country name] columns now that they are made redundant by the 'prov' variable
  select(-contains("init_prov")) %>%
  
  # keep only the number which indexes the province and combine it with the country name to make a province index
  mutate(index_prov = ifelse(index_prov != "", paste0(
    init_country, gsub("[^0-9]", "", index_prov)
  ), ""))

# reshape country_regions data to long format
country_regions_long = country_regions %>%
  gather(key="p_num",value="init_prov",-Country,-ISO2) %>% 
  mutate(index_prov=paste0(Country,p_num))

# match province code to actual province name
qualtrics <- left_join(qualtrics,country_regions_long,by="index_prov")


# combining info on target countries --------------------


# double check to make sure 'target_country' is empty when 'target_country_sub' has a value
if (length(qualtrics[which(qualtrics$target_country_sub != ""), 'target_country'] %>% table()) == 0) {
  # replace 'target_country' with 'target_country_sub' when target_country_sub has a value, then remove 'target_country_sub'
  # this is because 'target_country_sub' records the country for when a policy is targeted toward a region inside a country
  qualtrics[which(qualtrics$target_country_sub != ""), 'target_country'] = qualtrics[which(qualtrics$target_country_sub !=
                                                                                             ""), 'target_country_sub']
  qualtrics = qualtrics[, -which(names(qualtrics) == 'target_country_sub')]
  print('All Good')
} else{
  stop("Error: Problem with target_country recoding.")
}

# double check to make sure 'target_country' is empty when 'all countries' is selected
if (all (is.na(qualtrics[which(qualtrics$target_geog_level == "All countries"), 'target_country']))) {
  qualtrics[which(qualtrics$target_geog_level == "All countries"), 'target_country'] = "All countries"
  print('All Good')
} else{
  stop(
    "Error: double check to make sure 'target_country' is empty when 'all countries' is selected"
  )
}

source("RCode/validation/recode_records_countries.R")
#### Clean the 'other countries' text entries
## !!! NOTE that until April 2, it wasn't possible to do text entry for this, so we've lost this data for now
# its only 4 entries however, and should be straightforward to look in the original sources to get that info
# but still need to do this
# add select all/deselect all button; too tired now to trust myself not to fuck it up, do it in the morning
qualtrics$target_country[grepl('Other',  qualtrics$target_country)]

## Clean the 'other regions' text
# !!! NOTE  haven't done this yet, should do this at some point
qualtrics[which(qualtrics$target_region == "Other Regions (please specify below)"), 'target_region_14_TEXT']


saveRDS(qualtrics,"data/qualtrics_wide_clean.rds")

# add in additional rows for target areas as needed ----------------------------------
#
# currently, multiple targets are grouped together in one cell if the policy is the same on all dimensions for all targets

# Disaggregate regions (e.g. Schengen Area) into the relevant component countries
# !!! NOTE: still think about how we want to deal with the 'All' countries entry

regions = c("Europe",
            "North America",
            "Asia",
            "Africa",
            "Latin America",
            "Oceania",
            "Central America",
            "ASEAN Countries",
            "Middle East",
            "European Union (without the UK)",
            "European Union (with the UK)",
            "Schengen Area (with the UK)",
            "Schengen Area (without the UK)")

regions_disagg = c("Russia,Ukraine,France,Spain,Sweden,Norway,Germany,Finland,Poland,Italy,United Kingdom,Romania,Belarus,Kazakhstan,Greece,Bulgaria,Iceland,Hungary,Portugal,Austria,Czechia,Serbia,Ireland,Lithuania,Latvia,Croatia,Bosnia and Herzegovina,Slovakia,Estonia,Denmark,Switzerland,Netherlands,Moldova,Belgium,Armenia,Albania,North Macedonia,Turkey,Slovenia,Montenegro,Kosovo,Cyprus,Azerbaijan,Luxembourg,Georgia,Andorra,Malta,Liechtenstein,San Marino,Monaco,Vatican",
                   "United States,Mexico,Canada,Guatemala,Cuba,Haiti,Dominican Republic,Honduras,El Salvador,Nicaragua,Costa Rica,Panama,Puerto Rico,Jamaica,Trinidad and Tobago,Guadeloupe,Martinique,Bahamas,Belize,Barbados,Saint Lucia,United States,Virgin Islands,Grenada,Antigua and Barbuda,Dominica,Saint Kitts and Nevis,Saint Martin,British Virgin Islands,Anguilla,Montserrat",
                   "Afghanistan,Armenia,Azerbaijan,Bahrain,Bangladesh,Bhutan,Brunei,Cambodia,China,Cyprus,Egypt,Georgia,India,Indonesia,Iran,Iraq,Israel,Japan,Jordan,Kazakhstan,Korea, North Korea,South Korea,Kuwait,Kyrgyzstan,Laos,Lebanon,Malaysia,Maldives,Mongolia,Myanmar,Nepal,Oman,Pakistan,Philippines,Qatar,Russia,Saudi Arabia,Singapore,Sri Lanka,Syria,Tajikistan,Thailand,Timor-Leste,Turkey,Turkmenistan,United Arab Emirates,Uzbekistan,Vietnam,Yemen,Palestine,Taiwan",
                   "Algeria,Angola,Botswana,Burundi,Cameroon,Cabo Verde,Central African Republic,Chad,Comoros,Republic of the Congo,Democratic Republic of the Congo,Benin,Equatorial Guinea,Ethiopia,Eritrea,Djibouti,Gabon,Gambia,Ghana,Guinea,Ivory Coast,Kenya,Lesotho,Liberia,Libya,Madagascar,Malawi,Mali,Mauritania,Mauritius,Morocco,Mozambique,Namibia,Niger,Nigeria,Guinea-Bissau,Réunion,Rwanda,Sao Tome and Príncipe,Senegal,Seychelles,Sierra Leone,Somalia,South Africa,Zimbabwe,South Sudan,Sudan,Swaziland,Togo,Tunisia,Uganda,Egypt,Tanzania,Burkina Faso,Zambia",
                   "Argentina,Bolivia,Brazil,Chile,Colombia,Ecuador,Falkland Islands,Guyana,Paraguay,Peru,Suriname,Uruguay,Venezuela",
                   "Australia,Papua New Guinea,New Zealand,Fiji,Solomon Islands,Vanuatu,Kiribati,Micronesia,Tonga,American Samoa, Marshall Islands,Palau,Tuvalu,Nauru",
                   "Guatemala, Belize, Honduras, El Salvador, Nicaragua, Costa Rica, Panama",
                   "Brunei, Cambodia, Indonesia, Laos, Malaysia, Myanmar, Philippines, Singapore, Thailand, Vietnam",
                   "Bahrain,Cyprus,Egypt,Iran,Iraq,Israel,Jordan,Kuwait,Lebanon,Oman,Palestine,Qatar,Saudi Arabia,Syria,Turkey,United Arab Emirates,Yemen",
                   "Austria,Belgium,Bulgaria,Croatia,Cyprus,Czechia,Denmark,Estonia,Finland,France,Germany,Greece,Hungary,Ireland,Italy,Latvia,Lithuania,Luxembourg,Malta,Netherlands,Poland,Portugal,Romania,Slovakia,Slovenia,Spain,Sweden",
                   "Austria,Belgium,Bulgaria,Croatia,Cyprus,Czechia,Denmark,Estonia,Finland,France, Germany,Greece,Hungary,Ireland,Italy,Latvia,Lithuania,Luxembourg,Malta,Netherlands,Poland,Portugal,Romania,Slovakia,Slovenia,Spain,Sweden,United Kingdom",
                   "Austria,Belgium,Czech Republic,Denmark,Estonia,Finland,France,Germany,Greece,Hungary,Iceland,Italy,Latvia,Liechtenstein,Lithuania,Luxembourg, Malta, Netherlands,Norway,Poland,Portugal,Slovakia,Slovenia,Spain,Sweden,Switzerland,Slovakia,Slovenia,Spain,Sweden,United Kingdom",
                   "Austria,Belgium,Czech Republic,Denmark,Estonia,Finland,France,Germany,Greece,Hungary,Iceland,Italy,Latvia,Liechtenstein,Lithuania,Luxembourg,Malta,Netherlands,Norway,Poland,Portugal,Slovakia,Slovenia,Spain,Sweden,Switzerland,Slovakia,Slovenia,Spain,Sweden")

regions_df = data.frame(regions, regions_disagg)

# drop observations where coder chose more than 3 regions; CHECK later
qualtrics = qualtrics  %>% filter(unlist(lapply(stringr:::str_split(qualtrics$target_region, ','), function(x) length(x[!is.na(x)]))) <4)


# if coder selected multiple regions, separate them out into different columns
num_region_columns = max(unlist(lapply(stringr:::str_split(qualtrics$target_region, ','), function(x) length(x[!is.na(x)]))))
qualtrics = qualtrics  %>% separate(target_region, c(paste0("regions", 1:num_region_columns)), sep = ',', remove = FALSE)

# match regions to the countries that comprise them
qualtrics[, paste0("regions", 1:num_region_columns)]  = data.frame(apply(qualtrics [, paste0("regions", 1:num_region_columns)], 2, function(x){
  regions_df$regions_disagg[match(x, regions_df$regions)]}), stringsAsFactors = FALSE)


# recollapse separate columns into one column
qualtrics  = qualtrics  %>% 
  mutate_at(vars(starts_with("regions")),
            list( ~ replace(., is.na(.), ""))) %>%
  unite(target_regions_disagg, contains('regions'), sep = ',') %>%
  mutate(target_regions_disagg = gsub('\\,\\,|\\,$', "", target_regions_disagg))

# separate out disaggregated target countries into separate rows
qualtrics  = qualtrics %>% separate_rows(target_regions_disagg, sep =',')


## add additional rows for target countries/regional groupings
qualtrics = qualtrics %>%
  mutate_at(vars(starts_with("target_")),
            list( ~ replace(., is.na(.), ""))) %>%
  unite(target_country, c(target_country, target_regions_disagg), sep = ',') %>%
  mutate(target_country = gsub("\\,$|^\\,", "", target_country)) %>%
  separate_rows(target_country, sep = ',')


# replace  empty rows in [target_country] with disagregated 'All countries' from [target_geog_level]
# qualtrics[which(qualtrics$target_country == 'All'), 'target_country'] = paste(country_regions$Country, collapse = ',')
# qualtrics[which(qualtrics$target_country == 'All'), 'target_region'] = 'All countries'

# separate out disaggregated target countries from 'All countries' into separate rows 
# qualtrics  = qualtrics %>% separate_rows(target_country, sep = ',')

qualtrics$target_country = str_trim(qualtrics$target_country)


### for first version, don't break out target provinces/cities into separate rows
# but code for doing so is below

## add in additional rows for target countries as needed
# qualtrics = qualtrics %>% separate_rows(target_province, sep = ';')

# add in additional rows for target cities as needed
#qualtrics = qualtrics %>% separate_rows(target_city, sep = ';')


# check target_other variable text entries are standardized
#  !!!! NOTE haven't done this yet, not necessary for formatting data
# but we should def do this at some point
table(qualtrics$target_other)

# check target_who_what variable text entries are standardized
#  !!!! NOTE haven't done this yet, not necessary for formatting data
# but we should def do this

table(qualtrics$target_who_what)
unique(qualtrics$target_who_what_10_TEXT)



###------ combining info on QUANTITY of policy type, where applicable----- ###

# the policies for which the 'quantity' of a policy type apply are:
# the health resources variables (type_num_health_[health resource]), the number of quarantine days (type_num_quarantine_days), and number of poeple restricted from gathering (type_num_mass_gathterings)

# what makes making it so difficult to turn the health resources data into long format in particular is that:
# some, but not all health resources have a text entry to code the number of resources (e.g. masks)
# and some, but not all text entries are filled up
# at the end of the day, you want one column with the subtype (quality) of the health resource and another column with the number (quantity) of each health resource
# but these do not overlap perfectly in the data as currently formatted

# to solve this, in the below, (A) first we make a column for the number of health policies (type_sub, and type_sub_num) ( we also throw in days quarantine and number of people restricted from meeting)
#  (B) then a column for the type of health policies (type_health_policies)
# type_sub accounts for all sub policies that have a number associated with them (e.g. number of masks, number of quarantine days); type_sub_num gives the actual numbers
# type_health_resource accounts for all policies that were selected but don't have a number associated with them (e.g. the policy was about masks but the source didn't specify how many)
# sometimes type_sub and type_health coincide, sometimes they don't
# (C) then we resolve any duplicates that arise from doing (A) and (B)

# (A) add column names of each health resource to each row that has a text entry
qualtrics[, grep('type_num', names(qualtrics))] =  data.frame(t(apply(select(qualtrics, contains('type_num')), 1, function(x) {
  ifelse(is.na(x), x, paste(names(x), x, sep = "@"))
})), stringsAsFactors = FALSE)

qualtrics = qualtrics %>%
  # first change all of the NA's in the relevant columns that code for the number of policies to ""
  mutate_at(vars(starts_with("type_num")),
            list( ~ coalesce(.,""))) %>%
  # then combine all the type_num columns together into one column called 'type_num'
  # !!! note should probably create code to clean all !'s from text entries before hand
  unite(type_sub_num, contains('type_num'), sep = '!') %>%
  # replace extraneous !
  mutate_at(vars(starts_with("type_sub_num")),
            list( ~ gsub("[[:punct:]]+$|^[[:punct:]]+", "", .))) %>%
  mutate_at(vars(starts_with("type_sub_num")),
            list( ~ gsub("!+", "!", .))) %>%
  # remove type_num_[health resource] columns now that they are made redundant by the 'health_num' variable
  select(-contains("type_num"))  %>%
  # separate entries for each health resource
  separate_rows(type_sub_num, sep = '!') %>%
  # separate columns for type of health resource and number of health resource
  # !!! note should probably create code to clean all @'s from text entries before hand
  separate(type_sub_num, c("type_sub", 'type_sub_num'),  "@", fill = 'right')


# clean names so that they match what they originally were in the codebook
qualtrics$type_sub  = gsub('type_num_', '', qualtrics$type_sub) %>% capwords()
qualtrics$type_sub = qualtrics$type_sub %>% recode(
  MedCen = "Temporary Medical Centers",
  Ppe = "Personal Protective Equipment (e.g. gowns; goggles)",
  Hand_sanit = "Hand Sanitizer",
  Test_kits  = "Test Kits",
  QuaranCen = "Temporary Quarantine Centers",
  Research = "Health Research Facilities",
  PubTest = "Public Testing Facilities (e.g. drive-in testing for COVID-19)"
  
)

# !!! NOTE still need to check if all the text entries for the number of a policy type make sense/find a standard format for them as much as possible
# for the purposes of formatting the data however, not necessary

###------ making additional rows for QUALITY of policy type, where applicable----- ###

### (B) Health resources

# clean name so you can separate on ',' without problems
qualtrics$type_health_resource = gsub(
  "Personal Protective Equipment \\(e.g. gowns, goggles\\)",
  "Personal Protective Equipment (e.g. gowns; goggles)",
  qualtrics$type_health_resource
)

# this allows you to capture multiple health resources that may not have a number value attached to it
# e.g. the event is about doctors, but the source does not say how many doctors
# remember if the event is about doctors and the source says how many, this is already captured in the typ_sub/type_sub_num variables
qualtrics = qualtrics %>% separate_rows(type_health_resource, sep = ',')


# note that when you separate the rows in the above, qualtrics duplicates all of the type_sub/type_sub_num
# such that you get logical inconsistencies when there is a number value associated with the policy
#; e.g. a row where the type_health_resource is 'hospitals' and the type_sub is doctors;
# to fix this you should make those entries NA
qualtrics[which(qualtrics$type_health_resource != qualtrics$type_sub), c('type_sub', 'type_sub_num')] = NA


# (C) Resolve duplicates between number of health policies (type_sub) and quality of health policies (type_health_resource)
qualtrics = qualtrics %>%
  group_by(record_id, type, type_health_resource) %>%
  filter(if (all(is.na(type_sub)))
    row_number() == 1
    else!is.na(type_sub)) %>%
  ungroup()

# delete all of the health resources options (keep quarantine days and mass gathering) from the type_sub variable
# this is so you don't get duplicates when you unite all of the 'quality/kind' variables below (e.g. when uniting sub types for biz, health, schools etc)
qualtrics[which(qualtrics$type_sub == qualtrics$type_health_resource), 'type_sub'] = NA


### make all 'health other texts' into one column
# the relevant variables are: type_other_health_infra, type_other_health_infra, type_other NOT mutually exclusive

# first clean text entries to make them consistent
## !!! NOTE STILL NEED TO DO THIS, but for the purposes of formatting the data, not a priority

# then unite 'other' health categories into one
# and then separate them into separate rows
qualtrics = qualtrics %>%
  mutate_at(vars(
    c(
      type_other_health_infra,
      type_other_health_materials,
      type_other_health_staff
    )
  )  ,
  list( ~ replace(., is.na(.), ""))) %>%
  unite(
    type_other_health,
    c(
      type_other_health_infra,
      type_other_health_materials,
      type_other_health_staff
    ),
    sep = "@"
  ) %>%
  
  # replace extraneous @
  mutate_at(vars(type_other_health),
            list( ~ gsub("@+$|^@+", "", .))) %>%
  separate_rows(type_other_health, sep = "@")

# note that when you separate the rows in the above, qualtrics duplicates all of the type_other_health_mat/staff/infra
# such that you get logical inconsistencies when there is an 'other' text associated with the policy
#; e.g. a row where the type_health_resource is 'other health materials' and the type_other is masks;
# to fix this you should make those entries NA
qualtrics[which(qualtrics$type_health_resource != 'Other Health Materials'), 'type_other_health'] = NA


#### Restrictions
# make a seprate row for each external border restriction sub-category
qualtrics  = qualtrics %>% separate_rows(type_ext_restrict, sep = ',')
qualtrics[which(qualtrics$type_ext_restrict == 'None of the above'), 'type_ext_restrict'] = "None of the given external border restrictions measures"

#### Quarantine
# make a seprate row for each quarantine sub-category
qualtrics = qualtrics %>% separate_rows(type_quarantine, sep = ',')
qualtrics[which(qualtrics$type_quarantine == 'Other'), 'type_quarantine'] = 'Other Quarantine'

# delete all quarantine days from the sub type variable when there is a corresponding entry in the type_quarantine var
# this is so you don't get duplicates when you unite all of the 'quality/kind' variables below
qualtrics[which(qualtrics$type_sub == "Quarantine_days" &
                  !is.na(qualtrics$type_quarantine)), c('type_sub')] = NA


### Restriction on businesses
# make a seprate row for each restriction on businesses sub-category
qualtrics = qualtrics %>% separate_rows(type_business, sep = ',')
qualtrics[which(qualtrics$type_business == 'Other'), 'type_business'] = 'Other Restricted Businesses'

### Schools
# make a seprate row for each school sub-category
qualtrics  = qualtrics %>% separate_rows(type_schools, sep = ',')


##### unite all the quality/kind sub type variables together in one variable called type_sub_cat ####
qualtrics = qualtrics %>%
  mutate_at(vars(
    c(
      type_sub,
      type_ext_restrict,
      type_schools,
      type_business,
      type_health_resource,
      type_quarantine
    )
  )  ,
  list( ~ replace(., is.na(.), ""))) %>%
  unite(
    type_sub_cat,
    c(
      type_sub,
      type_ext_restrict,
      type_schools,
      type_business,
      type_health_resource,
      type_quarantine
    ),
    sep = ""
  )


# clean up 'other' text entries and put into one column -----------------------


### quarantine other texts
# type_quarantine_4_TEXT and type_quarantine_5_TEXT NOT mutually exclusive
# the following code in this section:
# 1) cleans the text entries
# 2) combines them into one column
# 3) separates out the rows

#1) clean text entries
# !!! NOTE: I only did this for up to the March 30 version of the data, still more to resolve for newer entries
table(qualtrics$type_quarantine_4_TEXT)
table(qualtrics$type_quarantine_5_TEXT)

qualtrics[which(qualtrics$type_quarantine_4_TEXT %in% c("over 70 years", "70+")), 'type_quarantine_4_TEXT'] = ">70"
qualtrics[which(qualtrics$type_quarantine_5_TEXT == "not specified, no information found"), 'type_quarantine_5_TEXT'] = "Not specified"
qualtrics[which(qualtrics$type_quarantine_5_TEXT == "based on gender"), 'type_quarantine_5_TEXT'] = "Based on Gender"


qualtrics = qualtrics %>%
  mutate_at(vars(c(
    type_quarantine_4_TEXT, type_quarantine_5_TEXT
  ))  ,
  list( ~ replace(., is.na(.), ""))) %>%
  
  # 2) combine them into one column
  unite(type_quarantine_text,
        c(type_quarantine_4_TEXT, type_quarantine_5_TEXT),
        sep = "@") %>%
  
  # replace extraneous @
  mutate_at(vars(starts_with("type_quarantine_text")),
            list( ~ gsub("@+$|^@+", "", .))) %>%
  # 3) separate the rows
  separate_rows(type_quarantine_text, sep = "@")


# note that when you separate the rows in the above, qualtrics duplicates all of the type_quarantine_4_TEXT, type_quarantine_5_TEXT
# such that you get logical inconsistencies when there is an 'other' text associated with the policy
#; e.g. a row where the type_sub_cat is 'self quarantine' and the type_quarantine_text is an age limit, when it should be NA;
# to fix this you should make those entries NA
quar_text_cats = c(
  'Quarantine only applies to people of certain ages. Please note the age restrictions in the text box.',
  'Other Quarantine'
)
qualtrics[-which(qualtrics$type_sub_cat %in% quar_text_cats), c('type_quarantine_text')] = NA


### busines other texts
# clean/standardize entries

# !!! NOTE STILL HAVEN"T DONE THIS, but for purposes of formatting data to long version, not an issue
table(qualtrics$type_business_6_TEXT)


#### Finally, unite all the text entries for the 'other' variables
# they should all also be mutually exclusive but CHECK
qualtrics = qualtrics %>%
  mutate_at(vars(
    c(
      type_quarantine_text,
      type_business_6_TEXT,
      type_other_health
    )
  )  ,
  list( ~ replace(., is.na(.), ""))) %>%
  unite(
    type_sub_cat_other,
    c(
      type_quarantine_text,
      type_business_6_TEXT,
      type_other_health
    ),
    sep = ""
  )


# note that when you separate the rows in the above, qualtrics duplicates all of the 'other' text entries
# such that you get logical inconsistencies when there is an 'other' text associated with the policy
#; e.g. a row where the type_sub_cat is 'shopping centers' and the business is the text entry type_sub_cat_other is e.g. tattoo parlos;
# to fix this you should make those entries NA
qualtrics[which(
  qualtrics$type == "Restriction of Non-Essential Businesses" &
    qualtrics$type_sub_cat != "Other Restricted Businesses"
), c('type_sub_cat_other')] = NA



# save clean file ---------------
 
saveRDS(qualtrics,
     file = "data/CoronaNet/coranaNetData_clean.rds")
