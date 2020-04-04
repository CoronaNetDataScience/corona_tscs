# This code transforms the raw survey qualtrics data  
# for the CoronaNet Project into a long format 
# the output for this code is currently saved under 'coranaNetData_clean_April+3%2C+2020_01.39.rda' in the Data/CoronaNet folder of the Dropbox

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

# -----------------------------
# Stuff still to do 
# -----------------------------

# 1) Finish resolving/checking for when the 'other'/text_entry category is selected for all applicable variables
# I tried to mark where this still needs to be done with the following comments: "!!! NOTE "
# Note, for now, I've left the 'other' text entry vars as their own columns, we may want to combine some of these 'other' text entries with the main variable for some of these variables at some point
# 2) check 'notes' section and address issues there accordingly
# 3) ?organize sources (rename?)
# 4) ?relabel some of the variable names for text entry vars to make them more intuitive
# 5) at the very end, download latest version of dataset and make sure everything still works

# -----------------------------
# set paths 
# -----------------------------
if(Sys.info()['user'] == 'cindycheng'){
  pathData = "/Users/cindycheng/Dropbox/corona-govt-response/Data"}

# ----------------------------------
## load packages and functions
# ----------------------------------
library(tidyr)
library(magrittr)
library(dplyr)
library(readr)
library(qualtRics)

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# ----------------------------------
## load data
# ----------------------------------
# note we can't connect to qualtrics via API
# because the TUM has not purchased one
# .csv downloads it is!

country_regions = read.csv(file = paste0(pathData, '/regions/country_region_clean.csv'), stringsAsFactors = FALSE) 
qualtrics = read_survey(paste0(pathData, '/CoronaNet/COVID-19+Government+Response+Tracker+Database_April+3%2C+2020_01.39.csv'))


# ----------------------------------
## idioscyncratic cleaning
# ----------------------------------
# Laura is not an RA; she's a colleague who I asked to test the survey
qualtrics = qualtrics[-which(qualtrics$ra_name %in% "Laura Mahrenbach" ),] 

# RA Mara Forster asked to start fresh on some entries and thus to ignore the following three entries:
qualtrics = qualtrics[-which(qualtrics$record_id %in% c(7897317, 5297, 571317)),]

# note RA Adrianna Poppe did not enter in the record id for her correction to 9008490; doing it for her here
qualtrics[which(qualtrics$entry_type_2_TEXT == 'https://bogota.gov.co/mi-ciudad/salud/decreto-081-de-alerta-amarilla-por-coronavirus-en-bogota'), 'entry_type_2_TEXT'] = 9008490

# RA Cheng-Hao Shen unncessarily chose other and NA category for target_who_what but otherwise the record is correct  
qualtrics[which(qualtrics$target_who_what_10_TEXT == "N/A"), c('target_who_what', 'target_who_what_10_TEXT')] = ""

# RA Cheng-Hao Shen had some pretty complicated issues with Palau, that Cindy can resolve
# at a later date, but for now, lets just remove Palau from the data
qualtrics = qualtrics[-which(qualtrics$init_country == 'Palau'),]  

# coder already documents this, correctly , in the target_who_what, removing it from 'target_other'
qualtrics[which(qualtrics$target_other == "this travel ban regards towards government officials and State-Owned Enterprises"),'target_other'] = ''

# add in niklas' corrections
	# RA Niklas had systematically made the mistake of entering the date he collected a source instead of the date the source was published
	# since he already had 68 entries coded, Cindy decided it would be a better use of time to just allow him to fix
	# the mistakes in a spreadsheet rather than having him make corrections on every single one, with the understanding that this was def an exception

# the following code loads his corrections and incorporates them

niklas_sources = read.csv(paste0(pathData, '/niklasSourcesCorrection_corrected.csv'), stringsAsFactors = FALSE)
niklas_sources = niklas_sources[-1,] # remove first row where i had given him a description of the columns
qualtrics[which(qualtrics$record_id %in% niklas_sources$record_id), c('sources_matrix_1_1', 'sources_matrix_2_1')]  = niklas_sources[, c('correct_date_1', 'correct_date_2')]


# ----- Cruise ships 
# lets table/remove this obs for now
# record id: 933728 --; The Government of Canada has chartered a plane to repatriate Canadians on board the Diamond Princess cruise ship docked in Yokohama, Japan. The aircraft will bring passengers from Japan to Canadian Forces Base Trenton, after which they will be assessed and transported to the NAV Canada Training Institute in Cornwall, Ontario, to undergo a further 14-day period of quarantine.
qualtrics = qualtrics[-which(qualtrics$record_id == 933728),]  

# 2890593 ;All American passengers have now disembarked from the Grand Princess.  The following reflects the count as of 8:30 p.m. PDT on March 12: \n \n480 total individuals were disembarked from the ship on March 12, 2020:\n\n    218 individuals were transported via chartered flight to Marine Corps Air Station Miramar near San Diego, California\n    251 individuals were transported via charted flight to Dobbins Air Reserve base near Marietta, Georgia\n    11 individuals were transported for care.
qualtrics = qualtrics[-which(qualtrics$record_id == 2890593),]  %>% data.frame()

# ----- Validate ------
# didn't validate these, just fixed problems/found solutions as necessary, should validate at some point!!!

# these entries should not be possible --- 
	#shouldn't be able to select restriciton of mass gatherings and then type of school
	#shouldn't be able to select quarantine and then type of school
	# shouldn't be able to select quarantine and then type of external restriction

# in terms of the survey version now, I checked on April 2 and this is not possible for this current version of the dataset
	# remove for now, but check what happened later!
	# my guess is that some of these RAs coded these cases as one policy type first, and then backtracked to another  policy type, and qualtrics saved the entries for the sub-types 
	# in the future, we should think about creating code that automates the search for these survey fallacies

qualtrics[which(qualtrics$record_id %in% c('3548864', '9939713', '933728', '9523505')),] %>% data.frame()
qualtrics = qualtrics[-which(qualtrics$record_id %in% c('3548864', '9939713', '933728', '9523505')),] 

# this entry clearly wrong, remove, validate/fix later
qualtrics = qualtrics[-which(qualtrics$target_who_what_10_TEXT == "Tests"),]
qualtrics = qualtrics[-which(qualtrics$target_who_what_10_TEXT == "Facilities were meant to increase testing capacity"),]
qualtrics = qualtrics[-which(qualtrics$target_who_what_10_TEXT ==  "Also foreign residents"),]    

# we didn't have a text entry option for target_country because it was in a multi-box 
	# note that the coder miscoded the country here anyway
	# should go back and validate all entries
qualtrics[which(qualtrics$target_country == 'Other (please specify below)'),'target_country'] = 'Bolivia'

# somebody  incorrectly  coded 'Schengen Area' in the other regions box for the target_region when the option is available 
qualtrics[grep('Schengen Area', qualtrics$target_region_14_TEXT),'target_region'] = "Schengen Area (without the UK)"
qualtrics[grep('Schengen Area', qualtrics$target_region_14_TEXT),'target_region_14_TEXT'] = ''

# others  incorrectly  coded regional provinces within a country in the target_region variable (which is for regional groupings)
qualtrics[grep('Hubei|Wuhan|Lombardy', qualtrics$target_region_14_TEXT),'target_province'] =  qualtrics[grep('Hubei|Wuhan|Lombardy', qualtrics$target_region_14_TEXT),'target_region_14_TEXT']
qualtrics[grep('Hubei|Wuhan|Lombardy', qualtrics$target_region_14_TEXT),'target_region_14_TEXT'] =  ''

# somebdoy  incorrectly  coded provinces within a country in the target_other variable
qualtrics[grep('Lombardy; Veneto', qualtrics$target_other),'target_province'] =  gsub('\\;', '\\,', qualtrics[grep("Lombardy; Veneto", qualtrics$target_other),'target_other'])
qualtrics[grep('Lombardy; Veneto', qualtrics$target_other),'target_other'] = ''

# somebdoy incorrectly coded cities within a country in the target_other variable
qualtrics[grep('Codogno;', qualtrics$target_other),'target_city'] =  qualtrics[grep('Codogno;', qualtrics$target_other),'target_other']
qualtrics[grep('Codogno;', qualtrics$target_other),'target_other'] = ''

# somebody  incorrectly  coded this city in target other and also didn't code the corresponding province
qualtrics[which(qualtrics$target_other == 'Tolima (Ibague)'), 'target_city'] = 'Tolima'
qualtrics[which(qualtrics$target_other == 'Tolima (Ibague)'), 'target_province'] = 'Ibague'
qualtrics[which(qualtrics$target_other == 'Tolima (Ibague)'), 'target_other'] = ''


# coder used 'other' text entry for target_who_what for hopstials; change to 'health infrastructure' for now and we should think about whether to add an extra option for coders
qualtrics[which(qualtrics$target_who_what_10_TEXT == "Health-Infrastructure"),'target_who_what']  = "Health-Infrastructure"
qualtrics[which(qualtrics$target_who_what_10_TEXT == "Infirmiry"),'target_who_what']  = "Health-Infrastructure"

# ----------------------------------
## text entry cleaning
# ----------------------------------

# remove all diacritics from the text entries for init_city, target_city, target_other

## !!! NOTE probably should do this for all text entries
qualtrics$init_city = stringi::stri_trans_general(qualtrics$init_city, "Latin-ASCII")
qualtrics$target_city  = stringi::stri_trans_general(qualtrics$target_city , "Latin-ASCII")
qualtrics$target_other  = stringi::stri_trans_general(qualtrics$target_other , "Latin-ASCII")
qualtrics$target_city[which(qualtrics$target_city == 'bogota')] = "Bogota"

# ----------------------------------
# replace entries with documented corrected entries as necessary
# ----------------------------------

# make vector of ids that need to be corrected
correction_record_ids = qualtrics[which(qualtrics$entry_type == 'Correction to Existing Entry (type in Record ID in text box)'), 'entry_type_2_TEXT'] 

# note that there are some un-matched records; check them out later
matched_corrections = qualtrics$record_id[which(qualtrics$record_id %in% correction_record_ids)]
(unmatched_corrections = setdiff(correction_record_ids, matched_corrections))

# make a variable called correct_record_match: if entry is corrected, fill in the corresponding record id entered in entry_type_2_TEXT,
#  if an entry was not corrected, fill in with original record id
qualtrics$correct_record_match = ifelse(qualtrics$entry_type == 'Correction to Existing Entry (type in Record ID in text box)', qualtrics$entry_type_2_TEXT, qualtrics$record_id)

# check for nas; there shouldn't be any
which(is.na(qualtrics$correct_record_match ))

# remove old entries
qualtrics = qualtrics %>% filter(!record_id %in% correction_record_ids)

# replace old entries with corrected entries
qualtrics$record_id = qualtrics$correct_record_match

# ----------------------------------
# link updated policy(ies) with original entry with variable 'policy_id'
# ----------------------------------
updated_record_ids = qualtrics[which(qualtrics$entry_type == 'Update on Existing Entry (type in Record ID in text box)'), 'entry_type_3_TEXT'] 

matched_updates = qualtrics$record_id[which(qualtrics$record_id %in% updated_record_ids)]
(unmatched_updates = setdiff(updated_record_ids, matched_updates)) # need to take a closer look later

# make a variable called correct_record_match: if entry is updated, fill in the corresponding original record id entered in entry_type_3_TEXT,
#  if an entry was not updated, fill in with original record id

qualtrics$policy_id = ifelse(qualtrics$entry_type == 'Update on Existing Entry (type in Record ID in text box)', qualtrics$entry_type_3_TEXT, qualtrics$record_id)


# ----------------------------------
# rename variables
# ----------------------------------
# rename all type questions that ask extra detail about the 'number' of a policy using the same variable name format
names(qualtrics)[which(names(qualtrics) %in% c("type_quarantine_days", "type_mass_gathering"))] = c('type_num_quarantine_days', 'type_num_mass_gathering')
names(qualtrics)[grep('type_health_resource_\\d_TEXT|type_health_resource_\\d\\d_TEXT', names(qualtrics))] = c('type_num_masks',
                                                                                                               'type_num_ventilators',		
                                                                                                               'type_num_ppe',
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
                                                                                                               'type_other_health_staff')					

# rename/link names for provinces back to the country
names(qualtrics)[grep('init_province', names(qualtrics))]  = paste0("init_prov_", gsub(" |\\-", '', c('European Union', country_regions$Country)))

# ----------------------------------
# remove empty columns
# ----------------------------------
# find empty columns, only delete empty columns of provinces or sources
empty_columns = names(which(apply(qualtrics, 2, function(x){all(is.na(x))})))
(empty_columns  = empty_columns[grep('prov|source', empty_columns)])

qualtrics = dplyr::select(qualtrics, -empty_columns)

# ----------------------------------
# combining columns
# ----------------------------------

###------  combine columns for init_provinces ----- ###
qualtrics = qualtrics %>% 
  # first change all of the NA's in the init_prov column to ""
  mutate_at(vars(starts_with("init_prov" )), 
            list(~ replace(., is.na(.), ""))) %>%
  
  # then combine all the init_prov columns together into one column called 'prov'
  unite(index_prov, contains('init_prov'), sep = '') %>%
  
  # remove init_prov_[country name] columns now that they are made redundant by the 'prov' variable
  select(-contains("init_prov")) %>%
  
  # keep only the number which indexes the province and combine it with the country name to make a province index
  mutate(index_prov = ifelse(index_prov !="", paste0(init_country, gsub("[^0-9]", "", index_prov)), "" ))


country_regions_long = country_regions %>% 
  
  # reshape country_regions data to long format
  pivot_longer(cols = contains('X'),
               names_to = c('region_num'),
               values_to = 'region') %>%
  
  # make province index that matches the 'init_prov' variable in 'qualtrics'
  mutate(index_prov = paste0(Country, gsub('X', '', region_num)))

# match province code to actual province name
qualtrics$init_prov = country_regions_long$region[match(qualtrics$index_prov, country_regions_long$index_prov)]

###------  combining info on target countries ----- ###
# double check to make sure 'target_country' is empty when 'target_country_sub' has a value
if (length(qualtrics[which(qualtrics$target_country_sub !=""), 'target_country'] %>% table()) == 0){
  
  # replace 'target_country' with 'target_country_sub' when target_country_sub has a value, then remove 'target_country_sub'
  # this is because 'target_country_sub' records the country for when a policy is targeted toward a region inside a country
  qualtrics[which(qualtrics$target_country_sub !=""), 'target_country'] = qualtrics[which(qualtrics$target_country_sub !=""), 'target_country_sub']
  qualtrics = qualtrics[,-which(names(qualtrics) == 'target_country_sub')]
  print('All Good')
} else{
  print("GO BACK AND CHECK THIS")
}

# double check to make sure 'target_country' is empty when 'all countries' is selected
if (all (is.na(qualtrics[which(qualtrics$target_geog_level == "All countries"), 'target_country']))){
  qualtrics[which(qualtrics$target_geog_level == "All countries"), 'target_country'] = "All"
  print('All Good')
} else{
  print("GO BACK AND CHECK THIS")
}


#### Clean the 'other countries' text entries
## !!! NOTE that until April 2, it wasn't possible to do text entry for this, so we've lost this data for now
	# its only 4 entries however, and should be straightforward to look in the original sources to get that info
	# but still need to do this
	# add select all/deselect all button; too tired now to trust myself not to fuck it up, do it in the morning
qualtrics$target_country[grepl('Other',  qualtrics$target_country)]

## Clean the 'other regions' text
# !!! NOTE  haven't done this yet, should do this at some point
qualtrics[which(qualtrics$target_region == "Other Regions (please specify below)"), 'target_region_14_TEXT']




# ----------------------------------
# add in additional rows for target areas as needed
# currently, multiple targets are grouped together in one cell if the policy is the same on all dimensions for all targets
# ----------------------------------

## add additional rows for target countries/regional groupings
qualtrics= qualtrics %>% 
  mutate_at(vars(starts_with("target_" )), 
            list(~ replace(., is.na(.), ""))) %>%
  unite(target_country_region, c(target_country, target_region), sep = ',') %>%
  mutate(target_country_region = gsub("\\,$|^\\,", "", target_country_region)) %>%
  separate_rows(target_country_region, sep = ',') 


### !!! NOTE, we still need to write up some code to disaggregate the countries in regional groupings
# e.g. split out Schengen Area into the relevant countries 
# and think about how we want to deal with the 'All' countries entry


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


# --------------------------------------
# get a separate row for every policy sub-type
# --------------------------------------


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
qualtrics[, grep('type_num', names(qualtrics))] =  apply(select(qualtrics, contains('type_num')), 1, function(x){
  ifelse(is.na(x), x, paste(names(x), x, sep = "@"))}) %>% t()  

qualtrics = qualtrics %>%
  # first change all of the NA's in the relevant columns that code for the number of policies to ""
  mutate_at(vars(starts_with("type_num" )  ), 
            list(~ replace(., is.na(.), ""))) %>%
  # then combine all the type_num columns together into one column called 'type_num'
  # !!! note should probably create code to clean all !'s from text entries before hand
  unite(type_sub_num, contains('type_num'), sep = '!') %>%
  
  # replace extraneous !
  mutate_at(vars(starts_with("type_sub_num" )  ), 
            list(~ gsub("[[:punct:]]+$|^[[:punct:]]+", "", .))) %>%
  mutate_at(vars(starts_with("type_sub_num" )  ), 
            list(~ gsub("!+", "!", .))) %>%
  
  # remove type_num_[health resource] columns now that they are made redundant by the 'health_num' variable
  select(-contains("type_num"))  %>%
  
  
  # separate entries for each health resource
  separate_rows(type_sub_num, sep = '!') %>% 
  
  # separate columns for type of health resource and number of health resource
  # !!! note should probably create code to clean all @'s from text entries before hand
  separate(type_sub_num, c("type_sub", 'type_sub_num'),  "@", fill = 'right')


# clean names so that they match what they originally were in the codebook
qualtrics$type_sub  = gsub('type_num_', '', qualtrics$type_sub) %>% capwords()
qualtrics$type_sub = qualtrics$type_sub %>%recode(MedCen = "Temporary Medical Centers",
                                                  Ppe = "Personal Protective Equipment (e.g. gowns; goggles)",
                                                  QuaranCen = "Temporary Quarantine Centers",
                                                  Research = "Health Research Facilities",
                                                  PubTest = "Public Testing Facilities (e.g. drive-in testing for COVID-19)"
)

# !!! NOTE still need to check if all the text entries for the number of a policy type make sense/find a standard format for them as much as possible
# for the purposes of formatting the data however, not necessary

###------ making additional rows for QUALITY of policy type, where applicable----- ###

### (B) Health resources

# clean name so you can separate on ',' without problems
qualtrics$type_health_resource = gsub("Personal Protective Equipment \\(e.g. gowns, goggles\\)", "Personal Protective Equipment (e.g. gowns; goggles)", qualtrics$type_health_resource)

# this allows you to capture multiple health resources that may not have a number value attached to it
	# e.g. the event is about doctors, but the source does not say how many doctors
	# remember if the event is about doctors and the source says how many, this is already captured in the typ_sub/type_sub_num variables
qualtrics= qualtrics %>% separate_rows(type_health_resource, sep = ',')


# note that when you separate the rows in the above, qualtrics duplicates all of the type_sub/type_sub_num 
	# such that you get logical inconsistencies when there is a number value associated with the policy
	#; e.g. a row where the type_health_resource is 'hospitals' and the type_sub is doctors; 
	# to fix this you should make those entries NA
qualtrics[which(qualtrics$type_health_resource!=qualtrics$type_sub), c('type_sub', 'type_sub_num')] = NA


# (C) Resolve duplicates between number of health policies (type_sub) and quality of health policies (type_health_resource)
qualtrics = qualtrics %>% 
  group_by(record_id, type, type_health_resource) %>%
  filter( if (all(is.na(type_sub))) row_number() == 1 else !is.na(type_sub) )%>%
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
  mutate_at( vars(c(type_other_health_infra, type_other_health_materials, type_other_health_staff))  , 
             list(~ replace(., is.na(.), ""))) %>%
  unite(type_other_health, c(type_other_health_infra, type_other_health_materials, type_other_health_staff), sep = "@") %>%
  
  # replace extraneous @
  mutate_at(vars(type_other_health ), 
            list(~ gsub("@+$|^@+", "", .))) %>%
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
qualtrics[which(qualtrics$type_sub=="Quarantine_days" &!is.na(qualtrics$type_quarantine)), c('type_sub')] = NA


### Restriction on businesses 
# make a seprate row for each restriction on businesses sub-category
qualtrics = qualtrics %>% separate_rows(type_business, sep = ',')
qualtrics[which(qualtrics$type_business == 'Other'), 'type_business'] = 'Other Restricted Businesses'

### Schools
# make a seprate row for each school sub-category
qualtrics  = qualtrics %>% separate_rows(type_schools, sep = ',')


##### unite all the quality/kind sub type variables together in one variable called type_sub_cat ####
qualtrics = qualtrics %>% 
  mutate_at( vars(c(type_sub, type_ext_restrict, type_schools, type_business, type_health_resource, type_quarantine))  , 
             list(~ replace(., is.na(.), ""))) %>%
  unite(type_sub_cat, c(type_sub, type_ext_restrict, type_schools, type_business, type_health_resource, type_quarantine ), sep = "")


# -----------------------
# clean up 'other' text entries and put into one column
# -----------------------

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
  mutate_at( vars(c(type_quarantine_4_TEXT, type_quarantine_5_TEXT))  , 
             list(~ replace(., is.na(.), ""))) %>%
  
  # 2) combine them into one column
  unite(type_quarantine_text, c(type_quarantine_4_TEXT, type_quarantine_5_TEXT), sep = "@") %>%		
  
  # replace extraneous @
  mutate_at(vars(starts_with("type_quarantine_text" )  ), 
            list(~ gsub("@+$|^@+", "", .))) %>%
  # 3) separate the rows
  separate_rows(type_quarantine_text, sep = "@")


# note that when you separate the rows in the above, qualtrics duplicates all of the type_quarantine_4_TEXT, type_quarantine_5_TEXT
	# such that you get logical inconsistencies when there is an 'other' text associated with the policy
	#; e.g. a row where the type_sub_cat is 'self quarantine' and the type_quarantine_text is an age limit, when it should be NA; 
	# to fix this you should make those entries NA
quar_text_cats = c('Quarantine only applies to people of certain ages. Please note the age restrictions in the text box.',
                   'Other Quarantine')
qualtrics[-which(qualtrics$type_sub_cat %in% quar_text_cats), c('type_quarantine_text')] = NA


### busines other texts
# clean/standardize entries

# !!! NOTE STILL HAVEN"T DONE THIS, but for purposes of formatting data to long version, not an issue
table(qualtrics$type_business_6_TEXT)


#### Finally, unite all the text entries for the 'other' variables
# they should all also be mutually exclusive but CHECK
qualtrics= qualtrics %>%
  mutate_at( vars(c(type_quarantine_text, type_business_6_TEXT, type_other_health))  , 
             list(~ replace(., is.na(.), ""))) %>%
  unite(type_sub_cat_other, c(type_quarantine_text, type_business_6_TEXT, type_other_health), sep = "")


# note that when you separate the rows in the above, qualtrics duplicates all of the 'other' text entries 
	# such that you get logical inconsistencies when there is an 'other' text associated with the policy
	#; e.g. a row where the type_sub_cat is 'shopping centers' and the business is the text entry type_sub_cat_other is e.g. tattoo parlos; 
	# to fix this you should make those entries NA
qualtrics[which(qualtrics$type == "Restriction of Non-Essential Businesses" & qualtrics$type_sub_cat != "Other Restricted Businesses"), c('type_sub_cat_other')] = NA 


# ---------------
# save clean file
# ---------------
save(qualtrics, file = paste0(pathData, "/coronaNet/coranaNetData_clean_April+3%2C+2020_01.39.rda"))

