# Use this code to generate data for the visualization
# First run cleanQualtrics_short.R
# Bob Kubinec

require(dplyr)
require(readr)
require(readxl)
require(stringr)
require(idealstan)
require(lubridate)
require(tidyr)
require(missRanger)

# update all githubs

system2("git",args=c("-C ~/covid-tracking-data","pull"))
system2("git",args=c("-C ~/covid-19-data","pull"))
system2("git",args=c("-C ~/covid19_tests","pull"))
system2("git",args=c("-C ~/COVID-19","pull"))

# covid test data

covid_test <- read_csv("~/covid19_tests/data_snapshots/covid_tests_last.csv") %>% 
  select(ISO3,Date,tests_raw="Tests_raw",
         test_source="Source",
         test_notes="Notes",
         tests_date="Date_scraped",
         tests_daily_or_total="Daily_or_total") %>% 
  group_by(Date,ISO3,tests_daily_or_total) %>% 
  summarize(tests_raw=mean(tests_raw,na.rm=T))

# cases/deaths

cases <- read_csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
  select(-Lat, -Long,country="Country/Region") %>% 
  gather(key="date_start",value="confirmed_cases",-`Province/State`,-country) %>% 
  mutate(date_start=mdy(date_start),
         country=recode(country,Czechia="Czech Republic",
                        `Hong Kong`="China",
                        `US`="United States of America",
                        `Taiwan*`="Taiwan",
                        `Bahamas`="The Bahamas",
                        `Tanzania`="United Republic of Tanzania",
                        `North Macedonia`="Macedonia",
                        `Micronesia`="Federated States of Micronesia",
                        `Burma`="Myanmar",
                        `Tanzania`="United Republic of Tanzania",
                        `Cote d'Ivoire`="Ivory Coast",
                        `Korea, South`="South Korea",
                        `Timor-Leste`="East Timor",
                        `Congo (Brazzaville)`="Republic of Congo",
                        `Congo (Kinshasa)`="Democratic Republic of the Congo",
                        `Cabo Verde`="Cape Verde",
                        `West Bank and Gaza`="Palestine",
                        `Eswatini`="Swaziland")) %>% 
  group_by(date_start,country) %>% 
  summarize(confirmed_cases=sum(confirmed_cases,na.rm=T))
deaths <- read_csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
  select(-Lat, -Long,country="Country/Region") %>% 
  gather(key="date_start",value="deaths",-`Province/State`,-country) %>% 
  mutate(date_start=mdy(date_start),
         country=recode(country,Czechia="Czech Republic",
                        `Hong Kong`="China",
                        `US`="United States of America",
                        `Taiwan*`="Taiwan",
                        `Bahamas`="The Bahamas",
                        `Tanzania`="United Republic of Tanzania",
                        `North Macedonia`="Macedonia",
                        `Micronesia`="Federated States of Micronesia",
                        `Burma`="Myanmar",
                        `Tanzania`="United Republic of Tanzania",
                        `Cote d'Ivoire`="Ivory Coast",
                        `Korea, South`="South Korea",
                        `Timor-Leste`="East Timor",
                        `Congo (Brazzaville)`="Republic of Congo",
                        `Congo (Kinshasa)`="Democratic Republic of the Congo",
                        `Cabo Verde`="Cape Verde",
                        `West Bank and Gaza`="Palestine",
                        `Eswatini`="Swaziland")) %>% 
  group_by(date_start,country) %>% 
  summarize(deaths=sum(deaths,na.rm=T))
recovered <- read_csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>% 
  select(-Lat, -Long,country="Country/Region") %>% 
  gather(key="date_start",value="recovered",-`Province/State`,-country) %>% 
  mutate(date_start=mdy(date_start),
         country=recode(country,Czechia="Czech Republic",
                        `Hong Kong`="China",
                        `US`="United States of America",
                        `Taiwan*`="Taiwan",
                        `Bahamas`="The Bahamas",
                        `Tanzania`="United Republic of Tanzania",
                        `North Macedonia`="Macedonia",
                        `Micronesia`="Federated States of Micronesia",
                        `Burma`="Myanmar",
                        `Tanzania`="United Republic of Tanzania",
                        `Cote d'Ivoire`="Ivory Coast",
                        `Korea, South`="South Korea",
                        `Timor-Leste`="East Timor",
                        `Congo (Brazzaville)`="Republic of Congo",
                        `Congo (Kinshasa)`="Democratic Republic of the Congo",
                        `Cabo Verde`="Cape Verde",
                        `West Bank and Gaza`="Palestine",
                        `Eswatini`="Swaziland")) %>% 
  group_by(date_start,country) %>% 
  summarize(recovered=sum(recovered,na.rm=T))

# niehaus data

niehaus <- read_csv("data/data_niehaus/03_21_20_0105am_wep.csv") %>% 
  group_by(country) %>% 
  fill(pop_WDI_PW:EmigrantStock_EMS,.direction="down") %>% 
  slice(n()) %>% 
  filter(!is.na(ifs)) %>% 
  select(country,ccode,ifs,pop_WDI_PW:EmigrantStock_EMS)


# load cleaned data

clean_data <- readRDS("data/CoronaNet/coranaNetData_clean_wide.rds")

# severity index

# severity <- readRDS("data/severity_fit.rds")
# 
# # output by countries
# 
# sev_data <- summary(severity) %>% 
#   select(country="Person",severity_index_5perc=`Low Posterior Interval`,
#          severity_index_median="Posterior Median",
#          severity_index_95perc=`High Posterior Interval`,
#          date_start="Time_Point") %>% 
#   mutate(country=recode(country,Czechia="Czech Republic",
#                 `Hong Kong`="China",
#                 `United States`="United States of America",
#                 `Bahamas`="The Bahamas",
#                 `Tanzania`="United Republic of Tanzania",
#                 `North Macedonia`="Macedonia",
#                 `Micronesia`="Federated States of Micronesia",
#                 `Timor Leste`="East Timor",
#                 `Republic of the Congo`="Republic of Congo",
#                 `Cabo Verde`="Cape Verde",
#                 `Eswatini`="Swaziland"))

# select only columns we need


release <- filter(clean_data,!is.na(init_country),is.na(init_other),is.na(target_other) | target_other=="",
                  validation) %>% 
              select(record_id,policy_id,entry_type,event_description,country="init_country",
                     date_announced,
                     date_start,
                     date_end,
                     init_country_level,
                     province="init_prov",
                     city="init_city",
                     matches("type"),
                     target_country="target_country",
                     target_geog_level,
                     target_region,
                     target_province,
                     target_city,
                     target_other,
                     target_who_what,
                     recorded_date="RecordedDate",
                     target_direction,
                     travel_mechanism,
                     compliance,
                     enforcer,
                     link="sources_matrix_1_2") %>% 
  select(-matches("TEXT"))

# iterate and capture unique vars

type_vars <- names(release)[grepl(x=names(release),
                                  pattern="type\\_")]

unique_vars <- lapply(type_vars, function(c) {
  tibble(type_var=c,
         vals=unique(release[[c]]))
}) %>% bind_rows

# separate out free text entry

free_text <- filter(unique_vars,grepl(x=type_var,pattern="other|num"),!is.na(vals))
cats <- filter(unique_vars,!grepl(x=type_var,pattern="other|num"),!is.na(vals)) %>% 
  mutate(vals=str_replace(vals,
                     "Personal Protective Equipment \\(e\\.g\\. gowns, goggles\\)",
                     "Personal Protective Equipment"))

cats <- lapply(1:nrow(cats), function(i) {
  this_data <- slice(cats,i)
  tibble(orig_val=this_data$vals,
         type_var=this_data$type_var,
         vals=(str_split(this_data$vals,pattern=",")[[1]]))
}) %>% bind_rows

# assign unique IDs

cats <- mutate(cats,
               vals_id=as.numeric(factor(vals)))

all_let <- expand.grid(letters,LETTERS) %>% 
  mutate(new_id=paste0(as.character(Var2),
                       as.character(Var1)),
         vals_id=1:n())

# merge back in to cats

cats <- left_join(cats,select(all_let,new_id,vals_id),
                  by="vals_id") %>% 
  distinct

# now merge back in to regular data 

release_long <- gather(release,key="discard",value="type_text",
                       unique(free_text$type_var)) %>% 
  gather(key="extra",value="type_sub_cat",unique(cats$type_var))

# merge in new IDs

release_long <- left_join(release_long,select(cats,-vals_id),by=c(extra="type_var",
                                                 "type_sub_cat"="orig_val"))

# merge back down

release_long <- distinct(release_long,record_id,policy_id,new_id,.keep_all = T) %>% 
  mutate(record_id=paste0(record_id,new_id)) %>% 
  select(-new_id,-discard,-extra,-type_sub_cat) %>% 
  select(everything(),type_sub_cat="vals") %>% 
  mutate(type_sub_cat=na_if(type_sub_cat,"None of the above"))

release_long <- release_long %>% 
  mutate(province=ifelse(country=="Hong Kong","Hong Kong",province),
         province=ifelse(country=="Macau","Macau",province),
         init_country_level=recode(init_country_level,`No, it is at the national level`="National",
                                   `Yes, it is at the city/municipal level`="Municipal",
                                   `Yes, it is at the city/municipal level`="Provincial"),
         date_announced=lubridate::mdy(date_announced),
         date_start=lubridate::mdy(date_start),
         date_end=lubridate::mdy(date_end),
         entry_type=recode(entry_type,
                           `Correction to Existing Entry for record ID ${e://Field/record_id} (<- if no record ID listed, type in Record ID in text box)`="correction",
                           `Update on Existing Entry (type in Record ID in text box)`="update",
                           `Update on Existing Entry for record ID ${e://Field/record_id} (<- if no record ID listed, type in Record ID in text box)`="update",
                           `New Entry`="new_entry")) %>% 
  filter(!is.na(date_start),
         recorded_date<(today()-days(5))) %>% 
  mutate(type_sub_cat=ifelse(type_sub_cat=="None of the above",NA,type_sub_cat))

# recode records

release_long$country <- recode(release_long$country,Czechia="Czech Republic",
                           `Hong Kong`="China",
                          Macau="China",
                           `United States`="United States of America",
                           `Bahamas`="The Bahamas",
                           `Tanzania`="United Republic of Tanzania",
                           `North Macedonia`="Macedonia",
                           `Micronesia`="Federated States of Micronesia",
                           `Timor Leste`="East Timor",
                           `Republic of the Congo`="Republic of Congo",
                           `Cabo Verde`="Cape Verde",
                           `Eswatini`="Swaziland")

release_long$target_country <- recode(release_long$target_country,Czechia="Czech Republic",
                                  `Hong Kong`="China",
                                  `United States`="United States of America",
                                  `Bahamas`="The Bahamas",
                                  `Tanzania`="United Republic of Tanzania",
                                  `North Macedonia`="Macedonia",
                                  `Micronesia`="Federated States of Micronesia",
                                  `Timor Leste`="East Timor",
                                  `Republic of the Congo`="Republic of Congo",
                                  `Cabo Verde`="Cape Verde",
                                  `Eswatini`="Swaziland")

release_long <- mutate(release_long,init_country_level=ifelse(province %in% c("Hong Kong","Macau"),"No, it is at the national level",
                                                    init_country_level))

# country names

country_names <- read_xlsx("data/ISO WORLD COUNTRIES.xlsx",sheet = "ISO-names")

# try a simple join

release_long <- left_join(release_long,country_names,by=c("country"="ADMIN"))

missing <- filter(release_long,is.na(ISO_A2))

# we will get a warning because of the European Union

if(nrow(missing)>0 && !(all(missing$country=="European Union"))) {
  
  warning("Country doesn't match ISO data.")
  
}

# Add in severity index

#release_long <- left_join(release_long,sev_data,by=c("country","date_announced"))

release_long <- select(release_long,record_id,policy_id,recorded_date,date_announced,date_start,date_end,
                  entry_type,event_description,type,type_sub_cat,type_text,
                  everything())

# now output raw data for sharing

#write_csv(release_long,"../CoronaNet/data/coronanet_release_long.csv")
write_csv(release_long,"data/CoronaNet/coronanet_release.csv")

# merge with other files

release_long_combined <- left_join(cases,deaths, by=c("country","date_start")) %>% 
  left_join(recovered,by=c("country","date_start")) %>% 
  full_join(release_long,by=c("country","date_start")) %>% 
  left_join(covid_test,by=c(ISO_A3="ISO3",
                                                      date_start="Date")) %>% 
  left_join(niehaus,by=c("country"))

write_csv(release_long_combined,"data/CoronaNet/coronanet_release_allvars.csv")

# let's impute the buggers

release_long_combined <- group_by(release_long_combined,country) %>% 
  mutate(miss_test=all(is.na(tests_raw)))

imputed_release_long <- lapply(c(7583,
                            1999332,
                            747352,
                            99226,
                            1884630,
                            19945817,
                            856397,
                            885773,
                            1994005,
                            8847736),function(i) {
  missRanger(ungroup(release_long_combined),formula= FarRight_IO + 
               ExternalLaborOpenness_IO + eco_glob_KOF + 
               soc_glob_KOF + 
               cult_prox_KOF +
               poli_glob_KOF +
               overallGlob_index_KOF +
               news_WB +
               disap_FA +
               polpris_FA +
               latentmean_FA +
               transparencyindex_HR +
               state_IDC +
               muni_IDC +
               dispersive_IDC +
               constraining_IDC +
               inclusive_IDC +
               Rank_FP +
               Score_FP +
               sfi_SFI +
               ti_cpi_TI +
               v2x_polyarchy_VDEM +
               EmigrantStock_EMS ~.-c(record_id,policy_id,miss_test),
             pmm.k=10,
             seed=i) %>% 
    mutate(imputation_seed=i)
})

imputed_release_long <- lapply(imputed_release_long, mutate,tests_daily_or_total=ifelse(miss_test,NA,
                                                                              tests_daily_or_total),
                          tests_raw=ifelse(miss_test,NA,
                                           tests_raw))


#write_csv(release_combined,"../CoronaNet/data/coronanet_release_allvars.csv")

# copy raw data over

#system("cp data/CoronaNet/coranaNetData_clean.rds ../CoronaNet/data/coranaNetData_clean.rds")
