library(rstudioapi)
library(pmdplyr)
library(ggplot2)
library(xlsx)

##################################### This is to get the list of instances to be validated from the main dataset

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

CoronaNet <- read.csv2("CoronaNet_02.04.csv", sep = ",")#round 1

CoronaNet_validation_round1 <- subset(CoronaNet[, c('record_id', 'init_country', 'date_announced', 'sources_matrix_1_2', 'sources_matrix_2_2', 'sources_matrix_3_2', 'check'
                                      ) ], check == 1)

CoronaNet_validation_round1$RA_validate <- NA
CoronaNet_validation_round1$RA_reconcile <- NA
CoronaNet_validation_round1$RA_match <- NA
CoronaNet_validation_round1$PI_reconcile <- NA

write.csv(CoronaNet_validation_round1,"Validation_assignment.csv", row.names = FALSE)
write.xlsx(CoronaNet_validation_round1,"Validation_assignment.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

###*****####This is to clean the validation dataset and make it comparable to the main clean dataset####

#CoronaNet_validate <- read.csv2("CoronaNet_validate_05_04.csv", sep = ",")


###*****####This is to compare the validation dataset with the main clean dataset####

load(file = "coranaNetData_clean_03_04.rda")#get the main clean dataset

val_records <- as.character(CoronaNet_validate$entry_type_3_TEXT)[-c(1:2)]

small_main <- qualtrics %>%
  filter(record_id %in% val_records) %>%
  select(record_id, ra_name, type, init_country, target_geog_level, 
         target_direction, compliance, date_start, date_end)

small_validate <- CoronaNet_validate[3:nrow(CoronaNet_validate),] %>%
  select(entry_type_3_TEXT, ra_name, type, init_country, target_geog_level, target_direction, compliance, date_start, date_end)

colnames(small_main)

colnames(small_validate)[1] <- 'record_id'

compare_df(small_main, small_validate, c("record_id"))
