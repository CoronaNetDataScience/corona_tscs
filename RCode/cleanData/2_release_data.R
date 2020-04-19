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
              select(record_id,policy_id,entry_type,event_description,type,country="init_country",
                     date_announced,
                     date_start,
                     date_end,
                     init_country_level,
                     province="init_prov",
                     city="init_city",
                     type_quarantine,
                     type_business,
                     type_schools,
                     type_ext_restrict,
                     type_health_resource,
                     type_other,
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
  mutate(province=ifelse(country=="Hong Kong","Hong Kong",province),
         province=ifelse(country=="Macau","Macau",province),
         init_country_level=recode(`No, it is at the national level`="National",
                                   `Yes, it is at the city/municipal level`="Municipal",
                                   `Yes, it is at the city/municipal level`="Provincial"),
         date_announced=lubridate::mdy(date_announced),
         date_start=lubridate::mdy(date_start),
         type_sub_cat=coalesce(type_quarantine,type_business),
         type_sub_cat=coalesce(type_sub_cat,type_schools),
         type_sub_cat=coalesce(type_sub_cat,type_ext_restrict),
         type_sub_cat=coalesce(type_sub_cat,type_health_resource),
         type_sub_cat=coalesce(type_sub_cat,type_other),
         entry_type=recode(entry_type,
                           `Correction to Existing Entry for record ID ${e://Field/record_id} (<- if no record ID listed, type in Record ID in text box)`="correction",
                           `Update on Existing Entry (type in Record ID in text box)`="update",
                           `Update on Existing Entry for record ID ${e://Field/record_id} (<- if no record ID listed, type in Record ID in text box)`="update",
                           `New Entry`="new_entry")) %>% 
  filter(!is.na(date_start),
         recorded_date<(today()-days(5))) %>% 
  select(-c("type_business",
            "type_quarantine",
            "type_schools",
            "type_ext_restrict",
            "type_health_resource",
            "type_other")) %>% 
  mutate(type_sub_cat=ifelse(type_sub_cat=="None of the above",NA,type_sub_cat))

# recode records

release$country <- recode(release$country,Czechia="Czech Republic",
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

release$target_country <- recode(release$target_country,Czechia="Czech Republic",
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

release <- mutate(release,init_country_level=ifelse(province %in% c("Hong Kong","Macau"),"No, it is at the national level",
                                                    init_country_level))

# country names

country_names <- read_xlsx("data/ISO WORLD COUNTRIES.xlsx",sheet = "ISO-names")

# try a simple join

release <- left_join(release,country_names,by=c("country"="ADMIN"))

missing <- filter(release,is.na(ISO_A2))

# we will get a warning because of the European Union

if(nrow(missing)>0 && !(all(missing$country=="European Union"))) {
  
  warning("Country doesn't match ISO data.")
  
}

# Add in severity index

#release <- left_join(release,sev_data,by=c("country","date_announced"))

release <- select(release,record_id,policy_id,recorded_date,date_announced,date_start,date_end,
                  entry_type,event_description,type,type_sub_cat,
                  everything())

# now output raw data for sharing

#write_csv(release,"../CoronaNet/data/coronanet_release.csv")
write_csv(release,"data/CoronaNet/coronanet_release.csv")

# merge with other files

release_combined <- left_join(cases,deaths, by=c("country","date_start")) %>% 
  left_join(recovered,by=c("country","date_start")) %>% 
  full_join(release,by=c("country","date_start")) %>% 
  left_join(covid_test,by=c(ISO_A3="ISO3",
                                                      date_start="Date")) %>% 
  left_join(niehaus,by=c("country"))

write_csv(release_combined,"data/CoronaNet/coronanet_release_allvars.csv")

# let's impute the buggers

release_combined <- group_by(release_combined,country) %>% 
  mutate(miss_test=all(is.na(tests_raw)))

imputed_release <- lapply(c(7583,
                            1999332,
                            747352,
                            99226,
                            1884630,
                            19945817,
                            856397,
                            885773,
                            1994005,
                            8847736),function(i) {
  missRanger(ungroup(release_combined),formula= FarRight_IO + 
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

imputed_release <- lapply(imputed_release, mutate,tests_daily_or_total=ifelse(miss_test,NA,
                                                                              tests_daily_or_total),
                          tests_raw=ifelse(miss_test,NA,
                                           tests_raw))


#write_csv(release_combined,"../CoronaNet/data/coronanet_release_allvars.csv")

# copy raw data over

#system("cp data/CoronaNet/coranaNetData_clean.rds ../CoronaNet/data/coranaNetData_clean.rds")
