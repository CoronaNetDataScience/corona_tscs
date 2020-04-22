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
require(stringr)

# load cleaned file

clean <- read_csv("data/CoronaNet/coronanet_release.csv") %>% 
  filter(init_country_level=="National",
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
  distinct

# need to make sub-categories

clean$type_preschool <- grepl(x=clean$type_sub_cat,pattern="Preschool")
clean$type_primaryschool <- grepl(x=clean$type_sub_cat,pattern="Primary")
clean$type_secondschool <- grepl(x=clean$type_sub_cat,pattern="Secondary Schools")
clean$type_highered <- grepl(x=clean$type_sub_cat,pattern="Higher education")
clean$type_self_quarantine <- grepl(x=clean$type_sub_cat,pattern="Self-Quarantine")
clean$type_bars <- grepl(x=clean$type_sub_cat,pattern="Restaurants/Bars")
clean$type_retail <- grepl(x=clean$type_sub_cat,pattern="Retail Businesses")
clean$type_shopping <- grepl(x=clean$type_sub_cat,pattern="Shopping Centers")
clean$type_grooming <- grepl(x=clean$type_sub_cat,pattern="Personal Grooming")
clean$type_advisory <- grepl(x=clean$type_sub_cat,pattern="Travel Advisory")
clean$type_health_staff <- grepl(x=clean$type_sub_cat,pattern="Doctors|Nurses|Health Staff|Hospitals")
clean$type_health_ppe <- grepl(x=clean$type_sub_cat,pattern="Personal Protective Equipment")
clean$type_health_masks <- grepl(x=clean$type_sub_cat,pattern="Masks")
clean$type_govt_quar <- grepl(x=clean$type_sub_cat,pattern="Government Quarantine")
clean$type_quar_restrict <- grepl(x=clean$type_sub_cat,pattern="Quarantine only applies to people of certain ages")
clean$type_sanitizer <- grepl(x=clean$type_sub_cat,pattern="Hand Sanitizer")
clean$type_screenings <- grepl(x=clean$type_sub_cat,pattern="Health Screenings|Health Certificates")
clean$type_health_other <- grepl(x=clean$type_sub_cat,pattern="Health Volunteers|Health Research|Health Infrastructure|Health Materials")
clean$type_testing <- grepl(x=clean$type_sub_cat,pattern="Public Testing|Test Kits")
clean$type_temporary <- grepl(x=clean$type_sub_cat,pattern="Temporary")

# gather to make new sub-category with new recordIDs

clean <- select(clean,-type_sub_cat) %>% gather(key="extra_cat",value=value_cat,type_preschool:type_temporary) %>% 
  group_by(record_id) %>% 
  mutate(combine_type=ifelse(value_cat,paste0(type,"_",extra_cat),
                             type)) %>% 
  select(-value_cat,-extra_cat) %>% 
  distinct %>% 
  group_by(combine_type) %>% 
  mutate(combine_type2=ifelse(n()>2,combine_type,type)) %>% 
  ungroup %>% 
  select(-combine_type,combine_type="combine_type2")


clean <- clean %>% 
  mutate(compliance=as.numeric(grepl(x=compliance,pattern="Mandatory")),
    # compliance=case_when(grepl(x=compliance,pattern="Mandatory")~2,
    #                           grepl(x=compliance,pattern="Voluntary")~1,
    #                           TRUE~2),
         date_start=ymd(date_start),
         date_end=ymd(date_end)) %>% 
  filter(!is.na(date_start),
         !is.na(type)) %>% 
  mutate(target_geog_level=case_when(target_geog_level %in% c("One or more regional groupings",
                                                              "All countries",
                                                              "One or more countries and one or more regional groupings",
                                                              "One or more countries, but not all countries") &
                                       !grepl(x=type,pattern="External")~"country",
                                     TRUE~target_geog_level),
    target_geog_level=case_when(grepl(x=type,pattern="External")~as.numeric(forcats::fct_collapse(factor(target_geog_level,levels=c("A geographical or administrative unit within a country",
                                                             "One or more countries, but not all countries",
                                                             "One or more countries and one or more regional groupings",
                                                             "One or more regional groupings",
                                                             "All countries")),
                                                             region=c("One or more countries and one or more regional groupings",
                                                                      "One or more regional groupings",
                                                                      "One or more countries, but not all countries"))),
                                TRUE~as.numeric(factor(target_geog_level,levels=c("A geographical or administrative unit within a country",
                                                                                  "country"))))) %>% 
  # sum target countries matrix
  group_by(record_id) %>% 
  mutate(target_country_num=case_when(all(target_country==unique(country),na.rm=T)~NA_real_,
                                      target_country==""~NA_real_,
                                      "All countries" %in% target_country~216,
                                   TRUE~as.numeric(length(unique(target_country))))) %>% 
  ungroup %>% 
       # mutate(target_country_num=(target_country_num-min(target_country_num,na.rm=T))/(max(target_country_num,na.rm=T)-min(target_country_num,na.rm=T))) %>% 
  distinct(country,target_country_num,combine_type,date_start,target_geog_level,compliance)


# complete the matrix

# function that takes a vector and increases over time values

set_deck <- function(x) {
  for(i in 2:length(x)) {
    if(!is.na(x[i-1])) {
      if(!is.na(x[i])) {
        if(x[i-1]>x[i]) {
          x[i] <- x[i-1]
        } else {
          x[i] <- x[i]
        }
      } else {
        x[i] <- x[i-1]
      }
      
    } else {
      x[i] <- x[i]
    }
  }
  return(x)
}

clean_comp <- clean %>% complete(country,combine_type,date_start) %>% group_by(country,combine_type) %>% 
  arrange(country,combine_type,date_start) %>% 
  mutate(compliance=set_deck(compliance),
         target_geog_level=set_deck(target_geog_level)) %>% 
  ungroup %>% 
  mutate(target_country_num=coalesce(target_country_num,0),
         compliance=coalesce(compliance,0),
         target_geog_level=ifelse(compliance==1,target_geog_level,0),
         target_geog_level=ifelse(is.na(target_geog_level) & compliance==1,2,target_geog_level),
         compliance=ifelse(combine_type=="Restriction of Non-Essential Businesses_type_bars",
                           recode(compliance,`2`=0,
                           `0`=2),compliance),
         combine_disc=ifelse(grepl(x=combine_type,pattern="External"),
                             target_geog_level,
                             compliance)) %>% 
  group_by(country) %>% 
  filter(date_start<today(),!all(compliance==0)) %>% 
  ungroup %>% 
  mutate(country=factor(country),
         country=relevel(country,"United States of America")) %>% 
  group_by(date_start) %>% 
  mutate(total_day=sum(compliance)) %>% 
  ungroup %>% 
  # group_by(country,combine_type) %>% 
  # arrange(country,combine_type,date_start) %>% 
  # mutate(target_country_num=cumsum(target_country_num),
  #        target_country_num=ifelse(target_country_num>216,216,target_country_num)) %>% 
  filter(!is.na(country)) %>% 
  mutate(model_id=3,
         #target_country_num=as.numeric(scale(target_country_num)),
         ordered_id=ifelse(grepl(x=combine_type,pattern="External"),4,3))

# get rid of days where there were no changes (will screw with the time series)

comp_days <- distinct(clean_comp,date_start,total_day) %>% 
  arrange(date_start) %>% 
  mutate(diff = total_day - dplyr::lag(total_day)) 

# do preliminary analysis

countries <- c("United States of America","Germany","Brazil","Switzerland","Israel")

to_make <- ungroup(distinct(clean_comp)) %>% 
  filter(country %in% countries) %>% 
  filter(date_start<ymd("2020-04-10"),
         date_start>ymd("2020-02-15"),
         !(date_start %in% comp_days$date_start[comp_days$diff==0])) %>% 
           id_make(outcome_disc="combine_disc",
                   person_id="country",
                   ordered_id="ordered_id",
                   item_id="combine_type",time_id="date_start")

# note no missing data :)

activity_fit <- id_estimate(to_make,vary_ideal_pts="random_walk",ncores=2,nchains=2,niters=500,
                            warmup=300,
            fixtype="prefix",
            restrict_ind_high="Quarantine/Lockdown_type_self_quarantine",
            restrict_ind_low="Restriction of Non-Essential Businesses_type_bars",
            id_refresh = 10,
            const_type="items")

# activity_fit <- parallel::mclapply(1:4, function(i) {
#   id_estimate(to_make,vary_ideal_pts="random_walk",ncores=1,nchains=1,niters=500,warmup=300,
#                             fixtype="prefix",
#                             restrict_ind_high="Quarantine/Lockdown_type_self_quarantine",
#                             restrict_ind_low="Restriction of Non-Essential Businesses_type_bars",
#                             id_refresh = 10,
#                             const_type="items")
#   },mc.cores=4)

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
