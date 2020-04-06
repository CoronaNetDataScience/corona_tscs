library(rstudioapi)
library(pmdplyr)
library(ggplot2)
library(xlsx)
library(tidyverse)
library(dplyr)
library(compareDF)
library(htmlTable)

##################################### This is to get the list of instances to be validated from the main dataset

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

CoronaNet <- read.csv2("CoronaNet_02.04.csv", sep = ",")#round 1

CoronaNet_validation_round1_02_04 <- subset(CoronaNet[, c('record_id', 'init_country', 'date_announced', 'sources_matrix_1_2', 'sources_matrix_2_2', 'sources_matrix_3_2', 'source_file_Name', 'check') ], check == 1)

CoronaNet_validation_round1_02_04$RA_validate <- NA
CoronaNet_validation_round1_02_04$RA_reconcile <- NA
CoronaNet_validation_round1_02_04$RA_match <- NA
CoronaNet_validation_round1_02_04$PI_reconcile <- NA

write.csv(CoronaNet_validation_round1_02_04,"Validation_assignment_round1.csv", row.names = FALSE)
#write.xlsx(CoronaNet_validation_round1_02_04,"Validation_assignment.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

###*****####Comparable dataset####

CoronaNet_validate <- read.csv2("CoronaNet_validate_06_04.csv", sep = ",")
CoronaNet_last <- read.csv2("CoronaNet_06_04.csv", sep = ",")#latest

###*****####This is to compare the validation dataset with the main clean dataset####

#load(file = "coranaNetData_clean_03.rda")#get the main clean dataset

val_records <- as.character(CoronaNet_validate$entry_type_3_TEXT)[-c(1:2)]

small_main <- CoronaNet_last %>%
  filter(record_id %in% val_records) %>%
  select(record_id, ra_name, type, init_country, target_geog_level, 
         target_direction, compliance, date_start, date_end, StartDate)

small_validate <- CoronaNet_validate[3:nrow(CoronaNet_validate),] %>%
  select(entry_type_3_TEXT, ra_name, type, init_country, target_geog_level, target_direction, compliance, date_start, date_end, StartDate)

colnames(small_main)

colnames(small_validate)[1] <- 'record_id'

ctable_validation <- compare_df(small_main, small_validate, c("record_id"))

ctable_validation <- compare_df(small_main[, c('record_id', "init_country", "type", "ra_name")], small_validate[, c('record_id', "init_country", "type", "ra_name")], c("record_id"))

library(htmlTable)
view_html(ctable_validation$comparison_df)
