# load R data and create spreadsheets for all countries

require(dplyr)
require(tidyr)
require(readr)
require(googlesheets4)
require(googledrive)
require(lubridate)

# identify

drive_auth()
sheets_auth(token = drive_token())

# date infection started
# we assume

seq_days <- seq(ymd('2019-12-15'),today(),by="days")
num_days <- length(seq_days)

niehaus_data <- read_csv("data/data_niehaus/03_21_20_0105am_wep.csv")

# iterate over countries

over_countries <- lapply(unique(niehaus_data$country), function(c) {
  this_country <- tibble(country=c,
                         day=seq_days,
                         ra_name="",
                         num_tested=rep("",num_days),
                         link_num_tested=rep("",num_days))
  
  # create new gs sheet
  
  this_country_sheet <- sheets_create(name=paste0(c,"_coronavirus_data"),sheets=c)

  # output data to sheet

  this_country_sheet <- sheets_write(this_country,ss=this_country_sheet,sheet=c)
  
  #drive_mkdir(paste0("panoptes_pdf_upload/",c))
  
  Sys.sleep(20)
  
  # # move to google drive folder
  # 
  # drive_mv(file=as_sheets_id(this_country_sheet),
  #          path="~/coronavirus_data/")
  
})





