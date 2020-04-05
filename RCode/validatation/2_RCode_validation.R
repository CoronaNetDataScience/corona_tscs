library(rstudioapi)
library(pmdplyr)
library(ggplot2)
library(xlsx)

#####################################

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

#CoronaNet <- read.csv2("CoronaNet_31.03.csv", sep = ",")
CoronaNet <- read.csv2("CoronaNet_02.04.csv", sep = ",")

CoronaNet_validation_round1 <- subset(CoronaNet[, c('record_id', 'init_country', 'date_announced', 'sources_matrix_1_2', 'sources_matrix_2_2', 'sources_matrix_3_2', 'check'
                                      ) ], check == 1)

CoronaNet_validation_round1$RA_validate <- NA
CoronaNet_validation_round1$RA_reconcile <- NA
CoronaNet_validation_round1$RA_match <- NA
CoronaNet_validation_round1$PI_reconcile <- NA

write.csv(CoronaNet_validation_round1,"Validation.csv", row.names = FALSE)
write.xlsx(CoronaNet_validation_round1,"Validation.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

###*****###

