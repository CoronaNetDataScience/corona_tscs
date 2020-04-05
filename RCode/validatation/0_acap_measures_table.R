# check count of measures recorded by ACAP database
setwd("/Users/cindycheng/Dropbox/corona-govt-response/Data/ACAP COVID-19")

library(magrittr)
library(stringr)
library(xtable)

data = read.csv('20200326-acaps-covid-19-goverment-measures-dataset-v2.csv', stringsAsFactors = FALSE)
xtable(table(str_trim(data$MEASURE))%>% sort())
