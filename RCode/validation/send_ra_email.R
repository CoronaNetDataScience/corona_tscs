# send RA update emails

require(googlesheets4)
require(dplyr)
require(blastula)
require(ggplot2)
require(readr)
require(stringr)
require(lubridate)
require(qualtRics)


# get list of RA emails

ra_data <- qualtRics::read_survey("data/CoronaNet/RA/ra_data_pull.csv")

# jataware

sheets_auth()

jat_direct <- sheets_get("https://docs.google.com/spreadsheets/d/1EW-Vsue2fQ7k41gC31hWjIUr290VPDO-0w-Ig_Wh71E/edit?ts=5e865e7d#gid=0") %>% 
  sheets_read()


# need to merge this back to our data

country_merge <- distinct(select(ra_data,ra_name,init_country)) %>% 
  mutate(init_country=recode(init_country,Czechia="Czech Republic",
                             `United Republic of Tanzania`="Tanzania",
                             `Micronesia`="F.S. Micronesia",
                             `Timor Leste`="East Timor",
                             `Democratic Republic of the Congo`="DR Congo",
                             `Republic of the Congo`="Congo",
                             `Cabo Verde`="Cape Verde",
                             `Sao Tome and Principe`="São Tomé and Príncipe"))
         

jat_direct <- left_join(jat_direct,country_merge,by=c(Country="init_country"))

# save this for use

saveRDS(jat_direct,"data/CoronaNet/RA/jat_direct.rds")

# loop and download each dataset

lapply(unique(jat_direct$Country), function(c) {
  jat_data <- try(sheets_get(unique(jat_direct$`Tracker URL`[jat_direct$Country==c])) %>% 
    sheets_read() %>% 
    mutate(publish_date=mdy_hms(publish_date)))
  Sys.sleep(30)
  
  if('try-error' %in% class(jat_data)) {
    jat_data <- sheets_get(unique(jat_direct$`Tracker URL`[jat_direct$Country==c])) %>% 
      sheets_read() %>% 
      mutate(publish_date=mdy_hms(publish_date))
  }
  
  saveRDS(jat_data,paste0("data/CoronaNet/RA/jat_",c,".rds"))
  
})

# now send emails 

ra_emails <- select(ra_data,ra_name,RecipientEmail) %>% 
  distinct(RecipientEmail,.keep_all = T) %>% 
  mutate(ra_name=coalesce(ra_name,"")) %>% 
  filter(ra_name!="")

email_out <- lapply(ra_emails$ra_name, function(r) {
  this_data <- filter(ra_emails,ra_name==r)
  
  this_email <- render_email("RCode/validation/RA_update_email.Rmd",
                             render_options=list(params=list(ra_name=this_data$ra_name)))
  print(paste0("Sending to ",r))
  
  if(!all(is.na(this_data$RecipientEmail))) {
    check <- try(smtp_send(this_email,to=this_data$RecipientEmail,
                           from="admin@coronanet-project.org",
                           subject="Weekly Update Email from CoronaNet Project",
                           credentials=creds_file("aws_cred"),verbose=T))
    
    while('try-error' %in% class(check)) {
      Sys.sleep(20) 
      check <- try(smtp_send(this_email,to=this_data$RecipientEmail,
                             from="admin@coronanet-project.org",
                             subject="Weekly Update Email from CoronaNet Project",
                             credentials=creds_file("aws_cred"),verbose=T))
    }
    return(check)
  } else {
    print("Recipient email address is missing.")
  }
  
  
})

