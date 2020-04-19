# Run idealstan model on the combined dataset
# Create a activity index from the policies
# Robert Kubinec
# April 4th, 2020

require(idealstan)
require(ggplot2)
require(tidyr)
require(dplyr)
require(lubridate)
require(readr)

# load cleaned file

clean <- read_csv("data/CoronaNet/coronanet_release.csv") %>% 
  filter(init_country_level=="No, it is at the national level",
         type!="Other Policy Not Listed Above") %>% 
  select(entry_type,type,country,
         compliance,date_announced,
         target_geog_level,
         type_sub_cat,
         target_country,
         target_direction,
         travel_mechanism,
         target_who_what,
         date_end,
         date_start,
         record_id) %>% 
  distinct %>% 
  mutate(compliance=as.numeric(grepl(x=compliance,pattern="Mandatory")),
    # compliance=case_when(grepl(x=compliance,pattern="Mandatory")~2,
    #                           grepl(x=compliance,pattern="Voluntary")~1,
    #                           TRUE~2),
         date_start=mdy(date_start),
         date_end=mdy(date_end),
         combine_type=paste0(type,"_",type_sub_cat)) %>% 
  filter(!is.na(date_start),
         !is.na(type)) %>% 
  mutate(combine_type=forcats::fct_collapse(factor(combine_type),
                               gatherings=c("Restrictions of Mass Gatherings_",
                                            "Restrictions of Mass Gatherings_Mass_gathering"),
                               health_testing=c("Health Testing_Other Health Materials",
                                                "Health Testing_Hospitals",
                                                "Health Testing_"),
                               health_resources=c("Health Resources_Doctors",
                                                  "Health Resources_Hand Sanitizer",
                                                  "Health Resources_Health Research Facilities",
                                                  "Health Resources_Health Volunteers",
                                                  "Health Resources_Hospitals",
                                                  "Health Resources_Unspecified Health Materials",
                                                  "Health Testing_Other Health Materials",
                                                  "Health Resources_",
                                                  "Health Resources_Temporary Quarantine Centers",
                                                  "Health Resources_Nurses",
                                                  "Health Resources_Other Health Infrastructure",
                                                  "Health Resources_Other Health Materials",
                                                  "Health Resources_Other Heath Staff",                                                 "Health Resources_Unspecified Health Infrastructure",
                                                  "Health Resources_Unspecified Health Staff"),
                               restrict_other_biz=c("Restriction of Non-Essential Businesses_Other Restricted Businesses",
                                                    "Restriction of Non-Essential Government Services_Other Restricted Businesses"),
                               restrict_commerce=c("Restriction of Non-Essential Businesses_",
                                                   "Restriction of Non-Essential Businesses_Non-Essential Commercial Businesses"))) %>% 
  # sum target countries matrix
  group_by(record_id) %>% 
  mutate(target_country_num=case_when(all(target_country==unique(country),na.rm=T)~NA_real_,
                                      target_country==""~NA_real_,
                                      "All countries" %in% target_country~216,
                                   TRUE~as.numeric(length(unique(target_country))))) %>% 
  ungroup %>% 
       # mutate(target_country_num=(target_country_num-min(target_country_num,na.rm=T))/(max(target_country_num,na.rm=T)-min(target_country_num,na.rm=T))) %>% 
  distinct(country,target_country_num,combine_type,date_start,compliance)


# complete the matrix

clean_comp <- clean %>% complete(country,combine_type,date_start) %>% group_by(country,combine_type) %>% 
  arrange(date_start) %>% 
  fill(compliance,.direction="down") %>% 
  mutate(target_country_num=coalesce(target_country_num,0),
         compliance=coalesce(compliance,0),
         target_country_num=ifelse(compliance==1,target_country_num,0),
         compliance=ifelse(combine_type=="Curfew_",
                           recode(compliance,`1`=0,
                           `0`=1),compliance)) %>% 
  group_by(country) %>% 
  filter(date_start<today(),!all(compliance==0)) %>% 
  ungroup %>% 
  mutate(country=factor(country),
         country=relevel(country,"United States")) %>% 
  group_by(date_start) %>% 
  mutate(total_day=sum(compliance)) %>% 
  group_by(country,combine_type) %>% 
  arrange(country,combine_type,date_start) %>% 
  mutate(target_country_num=cumsum(target_country_num),
         target_country_num=ifelse(target_country_num>216,216,target_country_num)) %>% 
  filter(!is.na(country)) %>% 
  mutate(model_id=ifelse(target_country_num>0,11,1),
         #target_country_num=as.numeric(scale(target_country_num)),
         ordered_id=3)

# get rid of days where there were no changes (will screw with the time series)

comp_days <- distinct(clean_comp,date_start,total_day) %>% 
  arrange(date_start) %>% 
  mutate(diff = total_day - dplyr::lag(total_day)) 

# do preliminary analysis

to_make <- id_make(distinct(ungroup(clean_comp)),outcome_disc="compliance",outcome_cont="target_country_num",
                    model_id="model_id",
                   person_id="country",
                   item_id="combine_type",time_id="date_start")

# note no missing data :)

# activity_fit <- id_estimate(to_make,vary_ideal_pts="random_walk",ncores=4,nchains=4,niters=500,
#                             warmup=300,
#             fixtype="prefix",
#             restrict_ind_high="gatherings",
#             restrict_ind_low="Curfew_",
#             id_refresh = 10,
#             const_type="items")

activity_fit <- parallel::mclapply(1:4, function(i) {
  id_estimate(to_make,vary_ideal_pts="random_walk",ncores=1,nchains=1,niters=500,warmup=300,
                            fixtype="prefix",
                            restrict_ind_high="gatherings",
                            restrict_ind_low="Curfew_",
                            id_refresh = 10,
                            const_type="items")
  },mc.cores=4)

saveRDS(activity_fit,"data/activity_fit.rds")

get_stan_mods <- lapply(activity_fit,function(s) s@stan_samples)

combine_stan_mods <- rstan::sflist2stanfit(get_stan_mods)

activity_fit[[1]]@stan_samples <- combine_stan_mods 

saveRDS(activity_fit[[1]],"data/activity_fit_collapse.rds")



# all_lev <- as.character(unique(clean_comp$country))
# 
# all_lev <- all_lev[all_lev!="Chad"]
# 
id_plot_legis_dyn(activity_fit[[1]],include=all_lev) + ylab("activity Index") + guides(color="none") +
  ggtitle("CoronaNet Index of activity of Measures\nOpposing COVID-19 Pandemic",
          subtitle="Posterior Median Estimates with 5% - 95% Intervals")

#ggsave("index.png")
