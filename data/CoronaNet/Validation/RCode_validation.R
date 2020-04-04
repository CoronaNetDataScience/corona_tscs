library(rstudioapi)
library(readstata13)
library(pmdplyr)
library (Amelia)
library(randomizr)
library(ggplot2)

#####################################

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

CoronaNet <- read.csv2("CoronaNet_31.03.csv", sep = ",")

CoronaNet_validation_round1 <- subset(CoronaNet[, c('record_id', 'init_country', 'date_announced', 'sources_matrix_1_2', 'sources_matrix_2_2', 'sources_matrix_3_2', 'check'
                                      ) ], check == 1)

CoronaNet_validation_round1$RA_validate <- NA
CoronaNet_validation_round1$RA_validate[1:10] <- 'Mohammad'

write.csv(CoronaNet_validation_round1,"Validation.csv", row.names = FALSE)

###*****###

