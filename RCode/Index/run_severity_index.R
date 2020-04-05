# Run idealstan model on the combined dataset
# Create a severity index from the policies
# Robert Kubinec
# April 4th, 2020

require(idealstan)
require(ggplot2)
require(tidyr)
require(dplyr)
require(lubridate)

# load cleaned file

clean <- readRDS("data/CoronaNet/coranaNetData_clean.rds") %>% 
  filter(Finished,validation,
         init_country_level=="No, it is at the national level") %>% 
  select(entry_type,type,init_country,init_country_level,
         compliance,date_announced) %>% 
  distinct %>% 
  mutate(compliance=ifelse(grepl(x=compliance,pattern="Mandatory"),1,NA),
         date_announced=mdy(date_announced)) %>% 
  filter(!is.na(date_announced))

# complete the matrix

clean_comp <- complete(clean,init_country,date_announced,type) %>% 
  group_by(init_country,type) %>% 
  arrange(date_announced) %>% 
  fill(compliance,.direction="down") %>% 
  mutate(compliance=coalesce(compliance,0),
         compliance=ifelse(compliance=="Curfew",1 - compliance,compliance)) %>% 
  group_by(init_country) %>% 
  filter(date_announced<today(),!all(compliance==0)) %>% 
  ungroup %>% 
  mutate(init_country=factor(init_country),
         init_country=relevel(init_country,"United States")) %>% 
  group_by(date_announced) %>% 
  mutate(total_day=sum(compliance))

# get rid of days where there were no changes (will screw with the time series)

comp_days <- distinct(clean_comp,date_announced,total_day) %>% 
  arrange(date_announced) %>% 
  mutate(diff = total_day - dplyr::lag(total_day)) 

# do preliminary analysis

to_make <- id_make(clean_comp,outcome_disc="compliance",person_id="init_country",
                   item_id="type",time_id="date_announced")

# note no missing data :)


severity_fit <- id_estimate(to_make,model_type=1,vary_ideal_pts="random_walk",ncores=1,nchains=1,niters=1000,
                            fixtype="prefix",
                            restrict_ind_high="Restriction of Non-Essential Businesses",
                            restrict_ind_low="Curfew",
                            id_refresh = 10,
                            const_type="items")

saveRDS(severity_fit,"data/severity_fit.rds")
