# Use this code to generate data for the visualization
# First run cleanQualtrics_short.R
# Bob Kubinec

require(dplyr)
require(readr)
require(readxl)
require(stringr)
require(idealstan)



# load cleaned data

clean_data <- readRDS("data/CoronaNet/coranaNetData_clean.rds")

# severity index

severity <- readRDS("data/CoronaNet/severity_fit.rds")

# output by countries

sev_data <- summary(severity) %>% 
  select(country="Person",severity_index="Posterior Median",
         date_announced="Time_Point") %>% 
  mutate(country=recode(country,Czechia="Czech Republic",
                `Hong Kong`="China",
                `United States`="United States of America",
                `Bahamas`="The Bahamas",
                `Tanzania`="United Republic of Tanzania",
                `North Macedonia`="Macedonia",
                `Micronesia`="Federated States of Micronesia",
                `Timor Leste`="East Timor",
                `Republic of the Congo`="Republic of Congo",
                `Cabo Verde`="Cape Verde",
                `Eswatini`="Swaziland"))

# select only columns we need

data_viz <- filter(clean_data,init_country_level=="No, it is at the national level",!is.na(init_country),
                   init_country!="European Union") %>% 
              select(record_id,entry_type,event_description,type,country="init_country",
                     target_country="target_country_region",target_direction,compliance,date_announced,link="sources_matrix_1_2") %>% 
  mutate(date_announced=lubridate::mdy(date_announced)) %>% 
  filter(!is.na(date_announced))

# recode records

data_viz$country <- recode(data_viz$country,Czechia="Czech Republic",
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

data_viz$target_country <- recode(data_viz$target_country,Czechia="Czech Republic",
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

# country names

country_names <- read_xlsx("data/ISO WORLD COUNTRIES.xlsx",sheet = "ISO-names")

# try a simple join

data_viz <- left_join(data_viz,country_names,by=c("country"="ADMIN"))

missing <- filter(data_viz,is.na(ISO_A2))

if(nrow(missing)>0) {
  
  stop("Country doesn't match ISO data.")
  
}


# need to recode country names

data_viz <- data_viz %>% group_by(record_id) %>% 
  mutate(target_country=paste0(target_country,collapse=",")) %>% 
  distinct %>% 
  mutate(target_country=case_when(grepl(x=target_country,
                                        pattern="All")~"All",
                                  grepl(x=target_country,
                                        pattern=",,")~"Domestic",
                                  grepl(x=target_country,
                                        pattern=",$")~"Domestic",
                                  TRUE~target_country))

# Add in severity index

data_viz <- left_join(data_viz,sev_data,by=c("country","date_announced"))

# fill in blank dates

data_viz <- group_by(data_viz,country,date_announced) %>% 
  arrange(country,date_announced) %>% 
  fill(severity_index,.direction="downup")

write_csv(data_viz,"data/CoronaNet/data_viz_clean.csv")

# just countries + targets 

clean_target <- function(c,country) {
  # by country figure out how to do this
  if(any(c=="All")) {
    return(c)
  } else {
    # pull apart the vector
    c <- str_split(c,pattern=",")[[1]]
    # remove any empty ones
    c <- c[c!=""]
    c <- unique(c)
    c <- c[c!=country[1]]
    return(paste0(c,collapse=","))
  }
}

combine_target <- function(c) {
  # iterate over c
  
  for(i in 2:length(c)) {
    
    if(c[i]==c[i-1]) {
      # do nothing
      c[i] <- c[i-1]
    } else {
      # are we at worldwide coverage?
      if(grepl(x=c[i-1],pattern="All")) {
        c[i] <-  "All" 
      } else if(grepl(x=c[i],pattern="All")) {
        c[i] <- "All"
      } else {
        # add to existing total 
        
        new_count <- str_split(c[i],pattern=",")[[1]]
        old_count <- str_split(c[i-1],pattern=",")[[1]]
        
        new_count <- new_count[!(new_count %in% old_count)]
        
        c[i] <- paste0(c(old_count,new_count),collapse=",")
        
        
      }
      
    }
    
  }
  
  return(c)
}

collapse_data <- data_viz %>% filter(target_country!="Domestic",!grepl(pattern="Other Regions",x=target_country)) %>% group_by(country,date_announced) %>% 
  summarize(all_targets=case_when("All" %in% target_country~"All",
                               TRUE~paste0(unique(target_country),collapse=","))) %>% 
  group_by(country,date_announced) %>% 
  mutate(all_targets=clean_target(all_targets,country),
         all_targets=na_if(all_targets,"")) %>% 
  ungroup %>% 
  filter(!is.na(all_targets),!is.na(date_announced)) %>% 
  complete(country,date_announced) %>% 
  group_by(country) %>% 
  arrange(country,date_announced) %>% 
  fill(all_targets,.direction="down") %>% 
  mutate(all_targets=coalesce(all_targets,""),
         all_targets=combine_target(all_targets),
         all_targets=str_replace_all(all_targets,"^,|,$",""))


collapse_data %>% 
  write_csv("data/CoronaNet/data_viz_collapse.csv")
