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
  select(entry_type,type,init_country,
         compliance,date_announced,type_sub_cat,
         target_geog_level,
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
  filter(!is.na(date_start)) %>% 
  mutate(combine_type=forcats::fct_collapse(factor(combine_type),
                               gatherings=c("Restrictions of Mass Gatherings_",
                                            "Restrictions of Mass Gatherings_Mass_gathering"),
                               health_resources=c("Health Resources_Doctors",
                                                  "Health Resources_Hand Sanitizer",
                                                  "Health Resources_Health Research Facilities",
                                                  "Health Resources_Health Volunteers",
                                                  "Health Resources_Hospitals",
                                                  "Health Resources_Masks",
                                                  "Health Resources_Nurses",
                                                  "Health Resources_Other Health Infrastructure",
                                                  "Health Resources_Other Health Materials",
                                                  "Health Resources_Other Heath Staff",
                                                  "Health Resources_Personal Protective Equipment (e.g. gowns; goggles)",
                                                  "Health Resources_Public Testing Facilities (e.g. drive-in testing for COVID-19)",
                                                  "Health Resources_Temporary Quarantine Centers",
                                                  "Health Resources_Unspecified Health Infrastructure",
                                                  "Health Resources_Unspecified Health Staff"),
                               restrict_other_biz=c("Restriction of Non-Essential Businesses_Other Restricted Businesses",
                                                    "Restriction of Non-Essential Government Services_Other Restricted Businesses"),
                               restrict_commerce=c("Restriction of Non-Essential Businesses_",
                                                   "Restriction of Non-Essential Businesses_Non-Essential Commercial Businesses"))) %>% 
  # sum target countries matrix
  group_by(record_id) %>% 
  mutate(target_country_num=case_when(all(target_country==unique(init_country),na.rm=T)~NA_real_,
                                      target_country==""~NA_real_,
                                      "All countries" %in% target_country~216,
                                   TRUE~as.numeric(length(unique(target_country))))) %>% 
  filter(!(type %in% c("Health Monitoring","Public Awareness Campaigns",
                       "New Task Force or Bureau",
                       "Health Resources",
                       "Health Testing",
                       "Other Policy Not Listed Above"))) %>% 
  distinct(init_country,target_country_num,combine_type,date_start,compliance) %>% 
  filter(!(compliance==1 & grepl(x=combine_type,pattern="Externa")))


# complete the matrix

clean_comp <- clean %>% complete(init_country,combine_type,date_start) %>% group_by(init_country,combine_type) %>% 
  arrange(date_start) %>% 
  fill(compliance,.direction="down") %>% 
  mutate(target_country_num=coalesce(target_country_num,0),
         compliance=coalesce(compliance,0),
         compliance=ifelse(combine_type=="Curfew_",
                           recode(compliance,`1`=0,
                           `0`=1),compliance)) %>% 
  group_by(init_country) %>% 
  filter(date_start<today(),!all(compliance==0)) %>% 
  ungroup %>% 
  mutate(init_country=factor(init_country),
         init_country=relevel(init_country,"United States")) %>% 
  group_by(date_start) %>% 
  mutate(total_day=sum(compliance)) %>% 
  group_by(init_country,combine_type) %>% 
  arrange(init_country,combine_type,date_start) %>% 
  mutate(target_country_num=cumsum(target_country_num)) %>% 
  filter(!grepl(x=combine_type,pattern="Health|health|NA|Other|Force|Awareness"),
         !is.na(init_country)) %>% 
  mutate(model_id=ifelse(target_country_num>0,9,1),
         target_country_num=as.numeric(scale(target_country_num)),
         ordered_id=3)

# get rid of days where there were no changes (will screw with the time series)

comp_days <- distinct(clean_comp,date_start,total_day) %>% 
  arrange(date_start) %>% 
  mutate(diff = total_day - dplyr::lag(total_day)) 

# do preliminary analysis

to_make <- id_make(clean_comp,outcome_disc="compliance",outcome_cont="target_country_num",
                    model_id="model_id",
                   person_id="init_country",
                   item_id="combine_type",time_id="date_start")

# note no missing data :)

# severity_fit <- id_estimate(to_make,vary_ideal_pts="random_walk",ncores=4,nchains=4,niters=500,
#                             warmup=300,
#             fixtype="prefix",
#             restrict_ind_high="gatherings",
#             restrict_ind_low="Curfew_",
#             id_refresh = 10,
#             const_type="items")

severity_fit <- parallel::mclapply(1:4, function(i) {
  id_estimate(to_make,vary_ideal_pts="random_walk",ncores=1,nchains=1,niters=500,warmup=300,
                            fixtype="prefix",
                            restrict_ind_high="gatherings",
                            restrict_ind_low="Curfew_",
                            id_refresh = 10,
                            const_type="items")
  },mc.cores=4)

saveRDS(severity_fit,"data/severity_fit.rds")

get_stan_mods <- lapply(severity_fit,function(s) s@stan_samples)

combine_stan_mods <- rstan::sflist2stanfit(get_stan_mods)

severity_fit[[1]]@stan_samples <- combine_stan_mods 

saveRDS(severity_fit[[1]],"data/severity_fit_collapse.rds")



# all_lev <- as.character(unique(clean_comp$init_country))
# 
# all_lev <- all_lev[all_lev!="Chad"]
# 
# id_plot_legis_dyn(severity_fit,include=all_lev) + ylab("Severity Index") + guides(color="none") +
#   ggtitle("CoronaNet Index of Severity of Measures\nOpposing COVID-19 Pandemic",
#           subtitle="Posterior Median Estimates with 5% - 95% Intervals")

ggsave("index.png")
